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
        Just (Connect player pool) -> do
            T.putStrLn $ "connect " <> player <> " into " <> pool
            ok <- addClient modelVar player conn
            if ok 
            then do
                sendMsg (Connected $ "hello " <> player) conn
                finally (run modelVar player conn) (stop modelVar player)
            else sendMsg (NotConnected $ player <> " already used") conn
        _ -> T.putStrLn "unknown query"

run :: TVar Model -> P.Player -> WS.Connection -> IO ()
run modelVar player conn = forever $ do
    msg <- recvMsg conn
    case msg of
        Just (P.PlayMove move) -> do
            T.putStrLn $ "playmove: " <> player <> " plays " <> T.pack (show move)
            m <- readTVarIO modelVar
            let battle = m^.battles & find (isInBattle player)
            case battle of
                Nothing -> T.putStrLn $ "invalid playmove from " <> player
                Just (pr, py, cr, cy, g) -> do
                    -- TODO check player
                    g1 <- stToIO $ G.playJ move g
                    -- TODO writeTVarIO modelVar $ 
                    if G.isRunning g1
                    then do
                        let c = if G._currentPlayer g1 == G.PlayerR then cr else cy
                        (board, color) <- stToIO $ P.fromGame g1
                        sendMsg (GenMove board color) c
                    else T.putStrLn "TODO endgame"
                    -- TODO play game
        _ -> putStrLn "unknown message; skipping"

stop :: TVar Model -> P.Player -> IO ()
stop modelVar player = do
    rmClient modelVar player
    putStrLn "stop"

-------------------------------------------------------------------------------
-- runner
-------------------------------------------------------------------------------

sleepTime :: Int
sleepTime = 1_000_000 

nbGames :: Int
nbGames = 10 

loopRunner :: TVar Model -> IO ()
loopRunner modelVar = do
    threadDelay sleepTime

    mPrPyCrCy <- atomically $ do
        m <- readTVar modelVar
        case m^.waiting of
            (pr:py:ws) -> do
                writeTVar modelVar (m & waiting .~ ws)
                let cs = m ^. clients
                return $ Just (pr, py, cs M.! pr, cs M.! py)
            _ -> return Nothing

    case mPrPyCrCy of
        Nothing -> loopRunner modelVar
        Just (pr, py, cr, cy) -> do
            T.putStrLn $ "newgame: " <> pr <> " vs " <> py
            sendMsg (NewGame pr py) cr
            sendMsg (NewGame pr py) cy
            game <- stToIO $ G.mkGame G.PlayerR
            atomically $ modifyTVar' modelVar
                (\m -> m & battles %~ ((pr, py, cr, cy, game):))
            (b, c) <- stToIO $ fromGame game
            sendMsg (GenMove b c) cr
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


