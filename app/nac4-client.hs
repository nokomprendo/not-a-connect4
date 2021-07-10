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
import Text.Read

type BotFunc = G.Game RealWorld -> ST RealWorld Int

main :: IO ()
main = do
    args <- getArgs
    case args of
        (host:port:player:botArgs) -> do
            mBotFunc <- mkBotFunc botArgs
            case mBotFunc of
                Just botFunc -> withSocketsDo $ WS.runClient host (read port) "" 
                                              $ clientApp botFunc (T.pack player)
                Nothing -> usage
        _ -> usage

mkBotFunc :: [String] -> IO (Maybe BotFunc)
mkBotFunc ["random"] = Just . genmove . BotRandom <$> createSystemRandom
mkBotFunc ["mc", nsimsStr] = do
    gen <- createSystemRandom
    return $ do
        nsims <- readMaybe nsimsStr
        Just (genmove $ BotMc nsims gen)
mkBotFunc ["mcts", nsimsStr] = do
    gen <- createSystemRandom
    return $ do
        nsims <- readMaybe nsimsStr
        Just (genmove $ BotMcts nsims gen)
mkBotFunc _ = return Nothing

clientApp :: BotFunc -> Player -> WS.ClientApp ()
clientApp botFunc player conn = do
    sendMsg (Connect player) conn
    msgToClient <- recvMsg conn
    case msgToClient of
        Just (Connected msg) -> do
            T.putStrLn $ "connected: " <> msg
            run botFunc conn
        Just (NotConnected msg) -> die ("not-connected: " <> T.unpack msg)
        _ -> die "connection failed"

run :: BotFunc -> WS.ClientApp ()
run botFunc conn = do
    msgToClient <- recvMsg conn
    case msgToClient of
        Just (NewGame pr py) -> T.putStrLn $ "newgame: " <> pr <> " " <> py
        Just (GenMove b c) -> do
            gameM <- stToIO (toGame b c)
            case gameM of
                Nothing -> T.putStrLn "genmove: error"
                Just game -> do
                    T.putStrLn $ "genmove: " <> b <> " " <> fmtColor c
                    k <- stToIO $ botFunc game
                    let j = G._moves game U.! k
                    T.putStrLn $ "playmove: " <> T.pack (show j)
                    sendMsg (PlayMove  j) conn
        Just (EndGame b s) -> do
            T.putStrLn $ "endgame: " <> b <> " " <> fmtStatus s
            die "TODO handle multiple games"
        _ -> die "unknown error"
    run botFunc conn

recvMsg :: WS.ClientApp (Maybe MsgToClient)
recvMsg conn = parseMsgToClient . WS.fromLazyByteString <$> WS.receiveData conn

sendMsg :: MsgToServer -> WS.ClientApp ()
sendMsg msg conn = WS.sendTextData conn $ fmtMsgToServer msg

usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ "usage: " <> progName <> " host port player bot [botArgs]\n"
    putStrLn $ "example: " <> progName <> " 127.0.0.1 3000 myname mcts 512"
    putStrLn $ "example: " <> progName <> " not-a-connect4.herokuapp.com 80 myname mc 64"

