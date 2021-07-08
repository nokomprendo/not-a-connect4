{-# LANGUAGE OverloadedStrings #-}

-- import NaC4.Client.Bot
import NaC4.Protocol
import NaC4.ProtocolImpl

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [host, port, player, pool] -> 
            withSocketsDo $ WS.runClient host (read port) "" 
                $ clientApp (T.pack player) (T.pack pool)
        _ -> do
            T.putStrLn "usage: host port player pool"
            T.putStrLn "  127.0.0.1 3000 myname zepool"

clientApp :: Player -> Pool -> WS.ClientApp ()
clientApp player pool conn = do
    putStrLn "Connecting..."
    WS.sendTextData conn (fmtMsgToServer $ Connect player pool)
    msgToClient <- parseMsgToClient . WS.fromLazyByteString <$> WS.receiveData conn
    print msgToClient


        -- Just (NotConnected msg) -> T.putStrLn msg

