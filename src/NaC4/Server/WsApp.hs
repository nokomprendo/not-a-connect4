{-# LANGUAGE OverloadedStrings #-}

module NaC4.Server.WsApp  where

import NaC4.Game
import NaC4.Protocol as P
import NaC4.ProtocolImpl as P
import NaC4.Server.Model

import qualified Network.WebSockets as WS

import Control.Concurrent.MVar
import Control.Monad.ST
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)

wsApp :: MVar Model -> Application -> Application
wsApp model = websocketsOr WS.defaultConnectionOptions (serverApp model)

serverApp ::MVar Model -> WS.PendingConnection -> IO ()
serverApp model pc = do
    conn <- WS.acceptRequest pc
    msgFromClient <- WS.fromLazyByteString <$> WS.receiveData conn
    case parseProtocol msgFromClient of
        Just (Connect player pool) -> do
            T.putStrLn $ "connect " <> player <> " into " <> pool
            modifyMVar_ model (\m -> do
                let cs = _clients m
                return m { _clients = M.insert player conn cs })
                -- TODO check if already in the map
            WS.sendTextData conn (fmtProtocol $ Connected $ "hello " <> player)

            {-
            (b, c) <- (_game <$> readMVar model) >>= stToIO . fromGame
            let msgToClient = fmtProtocol $ GenMove b c
            -}

        _ -> putStrLn "unknown query"



{-

import Common.Game
import Common.WsProtocol

import Control.Concurrent (modifyMVar, modifyMVar_, MVar, threadDelay)
import Control.Exception (finally)
import Control.Monad (forever, forM_)
import Data.Aeson (decode, encode)
import Data.Time.Clock (diffTimeToPicoseconds, utctDayTime)
import Data.Time.Clock.POSIX (getCurrentTime)

data WsModel = WsModel
    { wsGame :: Game
    , wsLastTime :: Double
    , wsMonitorMgr :: WsConnManager WS.Connection
    , wsControlMgr :: WsConnManager WS.Connection
    } deriving Show

wsGameTimeStepD :: Double
wsGameTimeStepD = 0.1 * 10**6

wsGameTimeStep :: Int
wsGameTimeStep = round wsGameTimeStepD

myGetTime :: IO Double
myGetTime = (* 10e-12) . fromIntegral . diffTimeToPicoseconds . utctDayTime
            <$> getCurrentTime

newWsModel :: IO WsModel
newWsModel = do
    t0 <- myGetTime
    return $ WsModel newGame t0 newWsConnManager newWsConnManager

wsApp :: MVar WsModel -> Application -> Application
wsApp var = websocketsOr WS.defaultConnectionOptions (serverApp var)

serverApp :: MVar WsModel -> WS.PendingConnection -> IO ()
serverApp var pc = do
    conn <- WS.acceptRequest pc
    msg <- decode . WS.fromLazyByteString <$> WS.receiveData conn
    case msg of
        Just WsMonitorAsk -> do
            putStrLn "new monitor"
            iConn <- addMonitorConn var conn
            finally (handleMonitor conn) (disconnectMonitor var iConn)
        Just WsControlAsk -> do
            putStrLn "new control"
            (iConn, agent) <- addControlConn var conn
            WS.sendTextData conn (encode $ WsColor $ agentCol agent) 
            finally (handleControl var iConn conn) (disconnectControl var iConn)
        _ -> putStrLn "warning: unknown WS connection"

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



{-

module NaC4.Server.WsManager where

-- TODO Data.Map.Strict ?

data WsConn a = WsConn
    { wsId :: Int
    , wsConn :: a
    }

instance Eq (WsConn a) where
    c1 == c2 = wsId c1 == wsId c2

data WsManager a = WsManager
    { wsNextId :: Int
    , wsConns :: [WsConn a]
    } deriving Eq

instance Show (WsManager a) where
    show (WsManager i cs) = "WsManager " ++ show i ++ " " ++ show (map wsId cs)

newWsManager :: WsManager a
newWsManager = WsManager 0 []

addConn :: a -> WsManager a -> (Int, WsManager a)
addConn conn (WsManager id0 cs) =
    (id0, WsManager (1+id0) (WsConn id0 conn : cs))

rmConn :: Int -> WsManager a -> WsManager a
rmConn id0 mgr = mgr { wsConns = filter ((/=) id0 . wsId) (wsConns mgr) }

-}

