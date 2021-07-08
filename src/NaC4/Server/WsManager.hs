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

