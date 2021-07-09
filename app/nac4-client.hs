{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import NaC4.Client.Bot
import qualified NaC4.Game as G
import NaC4.Protocol

import Control.Monad.ST
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as U
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import System.Random.MWC
import System.Environment (getArgs, getProgName)
import System.Exit (die)

main :: IO ()
main = do
    args <- getArgs
    -- TODO config bot
    bot <- BotMc 64 <$> createSystemRandom
    case args of
        [host, port, player, pool] -> 
            withSocketsDo $ WS.runClient host (read port) "" 
                $ clientApp bot (T.pack player) (T.pack pool)
        _ -> do
            progName <- getProgName
            putStrLn $ "usage: " <> progName <> " host port player pool"
            putStrLn $ "example: " <> progName <> " 127.0.0.1 3000 myname zepool"

clientApp :: (Bot RealWorld b) => b -> Player -> Pool -> WS.ClientApp ()
clientApp bot player pool conn = do
    sendMsg (Connect player pool) conn
    msgToClient <- recvMsg conn
    case msgToClient of
        Just (Connected msg) -> do
            T.putStrLn $ "connected: " <> msg
            run bot conn
        Just (NotConnected msg) -> die ("not-connected: " <> T.unpack msg)
        _ -> die "connection failed"

run :: (Bot RealWorld b) => b -> WS.ClientApp ()
run bot conn = do
    msgToClient <- recvMsg conn
    case msgToClient of
        Just (NewGame pr py) -> T.putStrLn $ "newgame: " <> pr <> " " <> py
        Just (GenMove b c) -> do
            gameM <- stToIO (toGame b c)
            case gameM of
                Nothing -> T.putStrLn "genmove: error"
                Just game -> do
                    T.putStrLn $ "genmove: " <> b <> " " <> fmtColor c
                    k <- stToIO $ genmove bot game
                    let j = G._moves game U.! k
                    T.putStrLn $ "playmove: " <> T.pack (show j)
                    sendMsg (PlayMove  j) conn
        Just (EndGame b s) -> do
            T.putStrLn $ "endgame: " <> b <> " " <> fmtStatus s
            die "TODO handle multiple games"
        _ -> die "unknown error"
    run bot conn

recvMsg :: WS.ClientApp (Maybe MsgToClient)
recvMsg conn = parseMsgToClient . WS.fromLazyByteString <$> WS.receiveData conn

sendMsg :: MsgToServer -> WS.ClientApp ()
sendMsg msg conn = WS.sendTextData conn $ fmtMsgToServer msg

