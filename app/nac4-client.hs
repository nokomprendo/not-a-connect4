{-# LANGUAGE OverloadedStrings #-}

import NaC4.Client.Bot
import NaC4.Protocol
import NaC4.ProtocolImpl

-- import Data.Aeson (decode, encode)
import Data.Text as T
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS

main :: IO ()
main = withSocketsDo $ WS.runClient "127.0.0.1" 3000 "" clientApp

clientApp :: WS.ClientApp ()
clientApp conn = do
    putStrLn "Connecting..."
    WS.sendTextData conn ("CONNECT" :: T.Text)
    msgFromServer <- parseProtocol . WS.fromLazyByteString <$> WS.receiveData conn
    print msgFromServer

