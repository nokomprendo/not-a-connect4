{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module NaC4.Server.WsApp (wsApp, wsIdleApp) where

import NaC4.Game as G
import NaC4.Protocol as P
import NaC4.Server.Model
import qualified NaC4.Server.Params as Params

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever, guard, when)
import Control.Monad.ST (stToIO, RealWorld)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX (getCurrentTime, utcTimeToPOSIXSeconds)
import Lens.Micro.Platform
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS

-------------------------------------------------------------------------------
-- wsApp
-------------------------------------------------------------------------------

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
                    (WS.withPingThread conn 30 (return ()) (run modelVar user conn))
                    (stop modelVar user)
            else sendMsg (NotConnected $ user <> " already used") conn
        _ -> T.putStrLn "unknown query"

-------------------------------------------------------------------------------
-- helpers
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
-- handle ws messages
-------------------------------------------------------------------------------

checkUser :: User -> Model -> Maybe (User, User, Battle, Game RealWorld)
checkUser user m1 = do
    let bs = m1^.mBattles & M.filterWithKey (\ k _ -> userInBattleKey user k)
    guard (M.size bs == 1)
    let ((userR,userY), bt) = M.elemAt 0 bs
        g0 = bt^.bGame
    guard (_currentPlayer g0 == G.PlayerR && user == userR
            || _currentPlayer g0 == G.PlayerY && user == userY)
    return (userR, userY, bt, g0)

updateBattle :: User -> User -> Game RealWorld -> Game RealWorld 
             -> P.Board -> Double -> Double -> Model -> Model
updateBattle userR userY g0 g1 b1 time1 time2 m =
    let f bt = let dt = time1 - bt^.bTimeI
               in if _currentPlayer g0 == G.PlayerR
                  then bt & bGame.~g1 & bBoard.~b1 & bTimeI.~time2 & bTimeR+~dt
                  else bt & bGame.~g1 & bBoard.~b1 & bTimeI.~time2 & bTimeY+~dt
    in m & mBattles %~ M.adjust f (userR, userY)

run :: TVar Model -> User -> WS.Connection -> IO ()
run modelVar user conn = forever $ do
    msg <- recvMsg conn
    case msg of
        Just (P.PlayMove move) -> do
            time1 <- myGetTime

            -- TODO refactor (atomic STM)
            m1 <- readTVarIO modelVar
            case checkUser user m1 of
                Nothing -> T.putStrLn $ "skipping playmove from " <> user
                Just (userR, userY, bt, g0) -> do
                    -- TODO check move
                    g1 <- stToIO $ G.playJ move g0
                    (board, player, status) <- stToIO $ P.fromGame g1
                    time2 <- myGetTime
                    atomically $ modifyTVar' modelVar 
                        (updateBattle userR userY g0 g1 board time1 time2)
                    let connR = (m1^.mClients) M.! userR
                        connY = (m1^.mClients) M.! userY
                        conn1 = if G._currentPlayer g1 == G.PlayerR
                                then connR else connY
                    if G.isRunning g1
                    then 
                        let pTime = if player==PlayerR then bt^.bTimeR else bt^.bTimeY
                            time = Params.wsBattleTime - pTime
                        in sendMsg (GenMove board player status time) conn1
                    else do
                        atomically $ finishBattle modelVar (userR,userY) bt board status
                        T.putStrLn $ userR <> " vs " <> userY 
                            <> " -> " <> fmtStatus status 
                        sendMsg (EndGame board PlayerR status P.Ok) connR
                        sendMsg (EndGame board PlayerY status P.Ok) connY

        _ -> putStrLn "unknown message"

stop :: TVar Model -> User -> IO ()
stop modelVar user = do
    atomically $ deleteClient modelVar user
    T.putStrLn $ "bye " <> user

-------------------------------------------------------------------------------
-- start battles
-------------------------------------------------------------------------------

computeFirstGame :: Model -> Maybe BattleKey
computeFirstGame m = 
    let fGames = M.filterWithKey $ \(ur,uy) _ ->
                    M.member ur (m^.mClients) 
                    && M.member uy (m^.mClients)
                    && M.notMember (uy,uy) (m^.mBattles)
                    && (m^.mNbGames) M.! (ur,uy) < Params.wsMaxNbGames
        fAcc Nothing ki ai = Just (ki,ai)
        fAcc (Just (k0,a0)) ki ai = if ai<a0 then Just (ki,ai) else Just (k0,a0)
    in fst <$> M.foldlWithKey' fAcc Nothing (fGames $ m^.mNbGames)

wsIdleApp :: TVar Model -> IO ()
wsIdleApp modelVar = forever $ do
    deleteTimeout modelVar
    startOneGame modelVar
    threadDelay 100_000

-- TODO timeout: report in stats and to client ?

deleteTimeout :: TVar Model -> IO ()
deleteTimeout modelVar = do
    time1 <- myGetTime
    (battles, model) <- atomically $ do
        model <- readTVar modelVar
        let battles = findTimeouts time1 model
        let finish (bk, bt, st) = finishBattle modelVar bk bt (bt^.bBoard) st
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
startOneGame modelVar = do
    res <- atomically $ do
        m <- readTVar modelVar
        let clients = m^.mClients
            waiting = m^.mWaiting
        let maybeBattle = do
                guard (length waiting >= 2)
                (userR,userY) <- computeFirstGame m
                guard (userR `elem` waiting && userY `elem` waiting)
                Just (userR, userY, clients M.! userR, clients M.! userY)
        when (isJust maybeBattle) $
            let Just (userR, userY, _, _) = maybeBattle
            in writeTVar modelVar (m & mWaiting %~ S.delete userR 
                                     & mWaiting %~ S.delete userY)
        return maybeBattle
    case res of
        Just (userR, userY, clientR, clientY) -> do
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
        Nothing -> return ()

