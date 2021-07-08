{-# LANGUAGE OverloadedStrings #-}

import NaC4.Client.Bot
import NaC4.Protocol
import NaC4.ProtocolImpl

-- import Data.Aeson (decode, encode)
import qualified Data.Text as T
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import System.Environment

main :: IO ()
main = do
    [player, pool] <- map T.pack <$> getArgs
    withSocketsDo $ WS.runClient "127.0.0.1" 3000 "" (clientApp player pool)

clientApp :: Player -> Pool -> WS.ClientApp ()
clientApp player pool conn = do
    putStrLn "Connecting..."
    WS.sendTextData conn (fmtProtocol $ Connect player pool)
    msgFromServer <- parseProtocol . WS.fromLazyByteString <$> WS.receiveData conn
    print msgFromServer

