{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module NaC4.Server.WsApp (wsApp, loopRunner) where

import NaC4.Game as G
import NaC4.Protocol as P
import NaC4.Server.Model

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever)
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

maxNbGames :: Int
maxNbGames = 10

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
                let app = WS.withPingThread conn 30 (return ()) (run modelVar user conn)
                finally app (stop modelVar user)
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

-- TODO refactor
loopRunner :: TVar Model -> IO ()
loopRunner modelVar = do

    res <- atomically $ do
        m <- readTVar modelVar
        let clients = m^.mClients
            nbGames = m^.mNbGames
            waiting = m^.mWaiting
        let f (ur,uy) _ = M.member ur clients && M.member uy clients
                            && M.notMember (uy,uy) (m^.mBattles)
                            && (m^.mNbGames) M.! (ur,uy) < maxNbGames
        let activeNbGames = M.filterWithKey f nbGames
        if length (m^.mWaiting) < 2
        then return Nothing
        else do
            let fusers Nothing ki ai = Just (ki,ai)
                fusers (Just (k0,a0)) ki ai = if ai<a0 then Just (ki,ai) else Just (k0,a0)
            let usersRYM = M.foldlWithKey' fusers Nothing activeNbGames
            case usersRYM of
                Just ((userR, userY),_) -> 
                    if userR `elem` waiting && userY `elem` waiting 
                    then do
                        writeTVar modelVar (m & mWaiting %~ S.delete userR
                                              & mWaiting %~ S.delete userY)
                        return $ Just (userR, userY, clients M.! userR, clients M.! userY)
                    else return Nothing
                _ -> return Nothing

    case res of
        Nothing -> do
            threadDelay 1_000_000
            loopRunner modelVar
        Just (userR, userY, playerR, playerY) -> do
            sendMsg (NewGame userR userY) playerR
            sendMsg (NewGame userR userY) playerY
            game <- stToIO $ G.mkGame G.PlayerR
            time <- myGetTime
            atomically $ modifyTVar' modelVar
                (\m -> m & mBattles %~ M.insert (userR,userY) (Battle game 0 0 time))
            (b, p, s) <- stToIO $ fromGame game
            sendMsg (GenMove b p s) playerR
            loopRunner modelVar

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

