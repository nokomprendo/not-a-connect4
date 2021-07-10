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
import Control.Monad.ST
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
            T.putStrLn $ "connect " <> user
            ok <- addClient modelVar user conn
            if ok 
            then do
                sendMsg (Connected $ "hello " <> user) conn
                finally (run modelVar user conn) (stop modelVar user)
            else sendMsg (NotConnected $ user <> " already used") conn
        _ -> T.putStrLn "unknown query"

run :: TVar Model -> User -> WS.Connection -> IO ()
run modelVar user conn = forever $ do
    msg <- recvMsg conn
    case msg of
        Just (P.PlayMove move) -> do
            T.putStrLn $ "playmove: " <> user <> " plays " <> T.pack (show move)
            m1 <- readTVarIO modelVar
            let battle = m1^.mBattles & find (isInBattle user)
            case battle of
                Nothing -> T.putStrLn $ "invalid playmove from " <> user
                Just (Battle userR userY g n) -> do
                    -- TODO check user
                    if _currentPlayer g == G.PlayerR && user == userR
                        || _currentPlayer g == G.PlayerY && user == userY
                    then do
                        g1 <- stToIO $ G.playJ move g
                        -- TODO refactor ?
                        atomically $ modifyTVar' modelVar $ \m -> 
                            m & mBattles %~ map (\bt -> 
                                if userR==bt^.bUserR && userY==bt^.bUserY
                                then Battle userR userY g1 n
                                else bt)
                        let connR = (m1^.mClients) M.! userR
                            connY = (m1^.mClients) M.! userY
                        let conn1 = if G._currentPlayer g1 == G.PlayerR
                                    then connR else connY
                        (board, player, status) <- stToIO $ P.fromGame g1
                        if G.isRunning g1
                        then sendMsg (GenMove board player status) conn1
                        else do
                            atomically $ modifyTVar' modelVar $ \m -> 
                                m & mResults %~ (Result userR userY board status :)
                            print status
                            sendMsg (EndGame board PlayerR status) connR
                            sendMsg (EndGame board PlayerY status) connY
                            -- TODO repeat battle

                    else T.putStrLn $ "not your turn " <> user
                        -- TODO play game
        _ -> putStrLn "unknown message; skipping"

stop :: TVar Model -> User -> IO ()
stop modelVar user = do
    delClient modelVar user
    putStrLn "stop"

-------------------------------------------------------------------------------
-- runner
-------------------------------------------------------------------------------

sleepTime :: Int
sleepTime = 1_000_000 

-- nbGames :: Int
-- nbGames = 10 

loopRunner :: TVar Model -> IO ()
loopRunner modelVar = do
    threadDelay sleepTime

    res <- atomically $ do
        m <- readTVar modelVar
        case m^.mWaiting of
            (userR:userY:ws) -> do
                writeTVar modelVar (m & mWaiting .~ ws)
                let cs = m ^. mClients
                return $ Just (userR, userY, cs M.! userR, cs M.! userY)
            _ -> return Nothing

    case res of
        Nothing -> loopRunner modelVar
        Just (userR, userY, playerR, playerY) -> do
            T.putStrLn $ "newgame: " <> userR <> " vs " <> userY
            sendMsg (NewGame userR userY) playerR
            sendMsg (NewGame userR userY) playerY
            game <- stToIO $ G.mkGame G.PlayerR
            atomically $ modifyTVar' modelVar
                (\m -> m & mBattles %~ (Battle userR userY game 0 :))
            (b, p, s) <- stToIO $ fromGame game
            sendMsg (GenMove b p s) playerR
            loopRunner modelVar

-------------------------------------------------------------------------------
-- ws
-------------------------------------------------------------------------------

recvMsg :: WS.Connection -> IO (Maybe MsgToServer)
recvMsg conn = parseMsgToServer . WS.fromLazyByteString <$> WS.receiveData conn

sendMsg :: MsgToClient -> WS.Connection -> IO ()
sendMsg msg conn = WS.sendTextData conn (fmtMsgToClient msg)





{-

import Data.Aeson (decode, encode)
import Data.Time.Clock (diffTimeToPicoseconds, utctDayTime)
import Data.Time.Clock.POSIX (getCurrentTime)

wsGameTimeStepD :: Double
wsGameTimeStepD = 0.1 * 10**6

wsGameTimeStep :: Int
wsGameTimeStep = round wsGameTimeStepD

myGetTime :: IO Double
myGetTime = (* 10e-12) . fromIntegral . diffTimeToPicoseconds . utctDayTime
            <$> getCurrentTime

serverApp :: MVar WsModel -> WS.PendingConnection -> IO ()
serverApp var pc = do
    conn <- WS.acceptRequest pc
    msg <- decode . WS.fromLazyByteString <$> WS.receiveData conn
    case msg of
        Just WsMonitorAsk -> do
            T.putStrLn "new monitor"
            iConn <- addMonitorConn var conn
            finally (handleMonitor conn) (disconnectMonitor var iConn)
        Just WsControlAsk -> do
            T.putStrLn "new control"
            (iConn, agent) <- addControlConn var conn
            WS.sendTextData conn (encode $ WsColor $ agentCol agent) 
            finally (handleControl var iConn conn) (disconnectControl var iConn)
        _ -> T.putStrLn "warning: unknown WS connection"

loopWsModel :: MVar WsModel -> IO ()
loopWsModel mVar = forever $ do
    threadDelay wsGameTimeStep
    t1 <- myGetTime
    wsmodel <- modifyMVar mVar $ \ wsmodel0 -> do
        let dt = t1 - wsLastTime wsmodel0
            game1 = stepGame dt (wsGame wsmodel0)
            wsmodel1 = wsmodel0 { wsGame = game1, wsLastTime = t1 }
        return (wsmodel1, wsmodel1)
    let conns = wsConns $ wsMonitorMgr wsmodel
        game = wsGame wsmodel
    forM_ conns $ \ wsconn ->
        WS.sendTextData (wsConn wsconn) (encode $ WsGame game) 

-------------------------------------------------------------------------------
-- monitor
-------------------------------------------------------------------------------

addMonitorConn :: MVar WsModel -> WS.Connection -> IO Int
addMonitorConn var conn = modifyMVar var f
    where f wsmodel0 = do
            let (i, mgr) = addConn conn (wsMonitorMgr wsmodel0)
                wsmodel1 = wsmodel0 { wsMonitorMgr = mgr }
            return (wsmodel1, i)

handleMonitor :: WS.Connection -> IO ()
handleMonitor conn = forever $ do
    _ <- WS.receiveDataMessage conn
    return ()

disconnectMonitor :: MVar WsModel -> Int -> IO ()
disconnectMonitor var iConn =
    modifyMVar_ var $ \ wsmodel0 -> do
        let mgr0 = wsMonitorMgr wsmodel0
            mgr1 = rmConn iConn mgr0
            game1 = rmAgent iConn (wsGame wsmodel0)
        return wsmodel0 { wsGame = game1, wsMonitorMgr = mgr1 }

-------------------------------------------------------------------------------
-- control
-------------------------------------------------------------------------------

addControlConn :: MVar WsModel -> WS.Connection -> IO (Int, Agent)
addControlConn var conn = modifyMVar var f
    where f wsmodel0 = do
            agent <- genAgent
            let (i, mgr) = addConn conn (wsControlMgr wsmodel0)
                game1 = addAgent i agent (wsGame wsmodel0)
                wsmodel1 = wsmodel0 { wsGame = game1, wsControlMgr = mgr }
            return (wsmodel1, (i, agent))

handleControl :: MVar WsModel -> Int -> WS.Connection -> IO ()
handleControl var iConn conn = forever $ do
    msg <- decode . WS.fromLazyByteString <$> WS.receiveData conn
    case msg of
        Just (WsActionStart dir) -> modifyMVar_ var $ \ wsmodel0 -> do
            let game0 = wsGame wsmodel0 
                game1 = startAgent iConn dir game0
            return wsmodel0 { wsGame = game1 }
        Just WsActionStop -> modifyMVar_ var $ \ wsmodel0 -> do
            let game0 = wsGame wsmodel0 
                game1 = stopAgent iConn game0
            return wsmodel0 { wsGame = game1 }
        _ -> return ()

disconnectControl :: MVar WsModel -> Int -> IO ()
disconnectControl var iConn =
    modifyMVar_ var $ \ wsmodel0 -> do
        let mgr0 = wsControlMgr wsmodel0
            mgr1 = rmConn iConn mgr0
            game1 = rmAgent iConn (wsGame wsmodel0)
        return wsmodel0 { wsGame = game1, wsControlMgr = mgr1 }

-}


