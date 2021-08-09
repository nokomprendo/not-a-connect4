{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module NaC4.Server.WsApp (wsApp, wsIdleApp) where

import NaC4.Game as G
import NaC4.Protocol as P
import NaC4.Server.Model
import NaC4.Server.Params as Params

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever, guard, join)
import Control.Monad.ST (stToIO)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as U
import Data.Time.Clock.POSIX (getCurrentTime, utcTimeToPOSIXSeconds)
import Lens.Micro.Platform
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS

-------------------------------------------------------------------------------
-- main apps
-------------------------------------------------------------------------------

-- TODO use Async to run wsApp and wsIdleApp concurrently

wsIdleApp :: TVar Model -> IO ()
wsIdleApp modelVar = forever $ do
    deleteTimeout modelVar
    startOneGame modelVar
    threadDelay 500_000

wsApp :: TVar Model -> Application -> Application
wsApp modelVar = websocketsOr WS.defaultConnectionOptions (serverApp modelVar)

serverApp ::TVar Model -> WS.PendingConnection -> IO ()
serverApp modelVar pc = do
    conn <- WS.acceptRequest pc
    msgToServer <- recvMsg conn
    case msgToServer of
        Just (Connect user) -> do
            T.putStrLn $ "hello " <> user
            ok <- atomically $ addClient modelVar user conn
            if ok 
            then do
                sendMsg (Connected $ "hello " <> user) conn
                finally 
                    (WS.withPingThread conn 30 (return ()) (runHandler modelVar user conn))
                    (stopHandler modelVar user)
            else sendMsg (NotConnected $ user <> " already used") conn
        _ -> T.putStrLn "unknown query"

-------------------------------------------------------------------------------
-- basic helpers
-------------------------------------------------------------------------------

recvMsg :: WS.Connection -> IO (Maybe MsgToServer)
recvMsg conn = parseMsgToServer . WS.fromLazyByteString <$> WS.receiveData conn

sendMsg :: MsgToClient -> WS.Connection -> IO ()
sendMsg msg conn = WS.sendTextData conn (fmtMsgToClient msg)

myGetTime :: IO Double
myGetTime = 
    let itod = fromIntegral :: Int -> Double
    in (0.001*) . itod . round <$> ((1000*) . utcTimeToPOSIXSeconds <$> getCurrentTime)

-------------------------------------------------------------------------------
-- handler
-------------------------------------------------------------------------------

stopHandler :: TVar Model -> User -> IO ()
stopHandler modelVar user = do
    atomically $ deleteClient modelVar user
    T.putStrLn $ "bye " <> user

runHandler :: TVar Model -> User -> WS.Connection -> IO ()
runHandler modelVar user conn = forever $ do
    msg <- recvMsg conn
    case msg of
        Just (P.PlayMove j) -> handlePlaymove modelVar user j
        _ -> putStrLn "unknown message"

-------------------------------------------------------------------------------
-- IO helpers
-------------------------------------------------------------------------------

handlePlaymove :: TVar Model -> User -> Int -> IO ()
handlePlaymove modelVar user j = do
    time <- myGetTime
    model <- readTVarIO modelVar
    case checkUser user model of
        Nothing -> T.putStrLn $ "invalid playmove from " <> user
        Just (bk, bt) -> do
            let game0 = bt^.bGame
            if j `U.notElem` G._moves game0 
            then do
                let board = bt^.bBoard
                T.putStrLn $ "invalid move from " <> user
                atomically $ modifyTVar' modelVar 
                    (updateBattle bk game0 game0 board time wsPenaltyTime)
            else do
                game1 <- stToIO $ G.playJ j game0
                (board, _, _) <- stToIO $ P.fromGame game1
                atomically $ modifyTVar' modelVar 
                    (updateBattle bk game0 game1 board time 0)
            deleteTimeout modelVar
            join $ atomically $ doPlaymove modelVar bk

doPlaymove :: TVar Model -> BattleKey -> STM (IO ())
doPlaymove modelVar bk = do
    model <- readTVar modelVar
    case M.lookup bk (model^.mBattles) of
        Nothing -> return $ return ()
        Just bt -> do
            let (userR, userY) = bk
                game = bt^.bGame
                player = _currentPlayer game
                status = _status game
                board = bt^.bBoard
                connR = (model^.mClients) M.! userR
                connY = (model^.mClients) M.! userY
            if G.isRunning game
            then 
                let (conn, pTime) = if player==PlayerR then (connR, bt^.bTimeR)
                                                       else (connY, bt^.bTimeY)
                    time = Params.wsBattleTime - pTime
                in return $ sendMsg (GenMove board player status time) conn
            else do
                finishBattle modelVar bk bt status
                return $ do
                    T.putStrLn $ userR <> " vs " <> userY 
                        <> " -> " <> fmtStatus status 
                    sendMsg (EndGame board PlayerR status P.Ok) connR
                    sendMsg (EndGame board PlayerY status P.Ok) connY


deleteTimeout :: TVar Model -> IO ()
deleteTimeout modelVar = do
    time <- myGetTime
    (battles, model) <- atomically $ do
        model <- readTVar modelVar
        let battles = findTimeouts time model
        let finish (bk, bt, st) = finishBattle modelVar bk bt st
        mapM_ finish battles
        return (battles, model)
    let finishWs ((userR, userY), bt, status) = do
            T.putStrLn $ userR <> " vs " <> userY <> " -> " 
                <> fmtStatus status <> " (by timeout)"
            let connR = (model^.mClients) M.! userR
                connY = (model^.mClients) M.! userY
                board = bt^.bBoard
            sendMsg (EndGame board PlayerR status Timeout) connR
            sendMsg (EndGame board PlayerY status Timeout) connY
    mapM_ finishWs battles

startOneGame :: TVar Model -> IO ()
startOneGame modelVar = join $ atomically $ do
    model <- readTVar modelVar
    let clients = model^.mClients
        waiting = model^.mWaiting
    let maybeBattle = do
            guard (length waiting >= 2)
            (userR,userY) <- findFirstGame model
            guard (userR `elem` waiting && userY `elem` waiting)
            Just (userR, userY, clients M.! userR, clients M.! userY)
    case maybeBattle of
        Nothing -> return $ return ()
        Just (userR, userY, clientR, clientY) -> do
            writeTVar modelVar (model & mWaiting %~ S.delete userR 
                                      & mWaiting %~ S.delete userY)
            return $ do
                sendMsg (NewGame userR userY) clientR
                sendMsg (NewGame userR userY) clientY
                game <- stToIO $ G.mkGame G.PlayerR
                (board, _, _) <- stToIO $ fromGame game
                time <- myGetTime
                atomically $ modifyTVar' modelVar
                    (\m -> m & mBattles %~ M.insert (userR,userY)
                                                (Battle game board 0 0 time))
                (b, p, s) <- stToIO $ fromGame game
                sendMsg (GenMove b p s Params.wsBattleTime) clientR

-------------------------------------------------------------------------------
-- STM helpers
-------------------------------------------------------------------------------

addClient :: TVar Model -> User -> WS.Connection -> STM Bool
addClient modelVar user conn = do
    model <- readTVar modelVar
    if M.member user (model^.mClients)
        then return False
        else do 
            let users = M.keys (model^.mClients)
                nbGames1 = [((user,u),0) | u<-users]
                nbGames2 = [((u,user),0) | u<-users]
            writeTVar modelVar $ model 
                & mClients %~ M.insert user conn
                & mWaiting %~ S.insert user
                & mNbGames %~ M.unionWith max (M.fromList $ nbGames1 ++ nbGames2)
                & mUserStats %~ M.insertWith (\_new old -> old) user newUserStats
            return True

deleteClient :: TVar Model -> User -> STM ()
deleteClient modelVar user = do
    model <- readTVar modelVar
    let (bs0, bs1) = M.partitionWithKey (\k _ -> userInBattleKey user k) (model^.mBattles)
        insertOpponents = 
            flip $ M.foldlWithKey' (\si k _ -> S.insert (opponent user k) si)
    if null bs0
    then writeTVar modelVar $ model & mClients %~ M.delete user
                                    & mWaiting %~ S.delete user
    else writeTVar modelVar $ model & mClients %~ M.delete user
                                    & mWaiting %~ insertOpponents bs0
                                    & mBattles .~ bs1

finishBattle :: TVar Model -> BattleKey -> Battle -> G.Status -> STM ()
finishBattle modelVar b@(userR,userY) bt status = do
    model <- readTVar modelVar
    let (Battle _ board timeR timeY _) = bt
    writeTVar modelVar $ model 
        & mWaiting %~ flip (foldr S.insert) [userR, userY]
        & mBattles %~ M.delete b
        & mResults %~ (Result userR userY board status timeR timeY :)
        & mNbGames %~ M.insertWith (+) (userR, userY) 1
        & mUserStats %~ M.adjust (updateStats PlayerR status timeR) userR
        & mUserStats %~ M.adjust (updateStats PlayerY status timeY) userY

