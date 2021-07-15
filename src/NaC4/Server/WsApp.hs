{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module NaC4.Server.WsApp (wsApp, startBattles) where

import NaC4.Game as G
import NaC4.Protocol as P
import NaC4.Server.Model
import qualified NaC4.Server.Params as Params

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever, guard)
import Control.Monad.ST (stToIO)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
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
            ok <- addClient modelVar user conn
            if ok 
            then do
                sendMsg (Connected $ "hello " <> user) conn
                finally 
                    (WS.withPingThread conn 30 (return ()) (run modelVar user conn))
                    (stop modelVar user)
            else sendMsg (NotConnected $ user <> " already used") conn
        _ -> T.putStrLn "unknown query"

-- TODO refactor
run :: TVar Model -> User -> WS.Connection -> IO ()
run modelVar user conn = forever $ do
    msg <- recvMsg conn
    case msg of
        Just (P.PlayMove move) -> do
            time1 <- myGetTime
            m1 <- readTVarIO modelVar
            let bs = m1^.mBattles & M.filterWithKey (\ k _ -> userInBattleKey user k)
            if M.size bs /= 1
            then T.putStrLn $ "invalid playmove from " <> user
            else 
                let ((userR,userY), bt0@(Battle g0 _ _ _)) = M.elemAt 0 bs
                in if (_currentPlayer g0 == G.PlayerR && user /= userR)
                        || (_currentPlayer g0 == G.PlayerY && user /= userY)
                    then T.putStrLn $ "not your turn " <> user
                    else do
                        g1 <- stToIO $ G.playJ move g0
                        time2 <- myGetTime
                        atomically $ modifyTVar' modelVar $ \m -> 
                            m & mBattles %~ M.mapWithKey (\(ur,uy) bt -> 
                                if userR==ur && userY==uy
                                then 
                                    let dt = time1 - bt^.bTimeI
                                    in if _currentPlayer g0 == G.PlayerR
                                        then bt & bGame.~g1 & bTimeI.~time2 & bTimeR+~dt
                                        else bt & bGame.~g1 & bTimeI.~time2 & bTimeY+~dt
                                else bt)
                        let connR = (m1^.mClients) M.! userR
                            connY = (m1^.mClients) M.! userY
                        let conn1 = if G._currentPlayer g1 == G.PlayerR
                                    then connR else connY
                        (board, player, status) <- stToIO $ P.fromGame g1
                        if G.isRunning g1
                        then sendMsg (GenMove board player status) conn1
                        else do
                            T.putStrLn $ userR <> " vs " <> userY 
                                <> " -> " <> T.pack (show status)
                            sendMsg (EndGame board PlayerR status) connR
                            sendMsg (EndGame board PlayerY status) connY
                            finishBattle modelVar (userR,userY) bt0 board status
        _ -> putStrLn "unknown message; skipping"

stop :: TVar Model -> User -> IO ()
stop modelVar user = do
    delClient modelVar user
    T.putStrLn $ "bye " <> user

-------------------------------------------------------------------------------
-- runner
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

startBattles :: TVar Model -> IO ()
startBattles modelVar = do
    -- look for possible new game
    res <- atomically $ do
        m <- readTVar modelVar
        let clients = m^.mClients
            waiting = m^.mWaiting
        let res0 = do
                guard (length waiting >= 2)
                (userR,userY) <- computeFirstGame m
                guard (userR `elem` waiting && userY `elem` waiting)
                Just (userR, userY, clients M.! userR, clients M.! userY)
        case res0 of
            Nothing -> return Nothing
            Just (userR, userY, _, _) -> do
                writeTVar modelVar (m & mWaiting %~ S.delete userR 
                                      & mWaiting %~ S.delete userY)
                return res0
    -- handle result
    case res of
        Nothing -> do
            -- no new game
            threadDelay 1_000_000
            startBattles modelVar
        Just (userR, userY, clientR, clientY) -> do
            -- start new game
            sendMsg (NewGame userR userY) clientR
            sendMsg (NewGame userR userY) clientY
            game <- stToIO $ G.mkGame G.PlayerR
            time <- myGetTime
            atomically $ modifyTVar' modelVar
                (\m -> m & mBattles %~ M.insert (userR,userY) (Battle game 0 0 time))
            (b, p, s) <- stToIO $ fromGame game
            sendMsg (GenMove b p s) clientR
            startBattles modelVar

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

