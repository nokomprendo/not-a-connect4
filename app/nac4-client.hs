{-# LANGUAGE OverloadedStrings #-}

import NaC4.Client.Bot
import NaC4.Protocol

-- import Data.Aeson (decode, encode)
import Data.Text as T
import Network.Socket (withSocketsDo)
import Network.WebSockets (ClientApp, runClient, sendClose, sendTextData,
    receiveData)

main :: IO ()
main = withSocketsDo $ runClient "127.0.0.1" 3000 "" clientApp

clientApp :: ClientApp ()
clientApp conn = do
    putStrLn "Connecting..."
    sendTextData conn ("CONNECT" :: T.Text)
    m <- receiveData conn
    print (m :: T.Text)

{-

clientApp :: ClientApp ()
clientApp conn = do
    putStrLn "Connected!"
    sendTextData conn (encode WsControlAsk)
    m <- decode <$> receiveData conn
    print (m :: Maybe Game)
    _ <- getLine
    sendClose conn ("Bye!" :: Text)

-}

