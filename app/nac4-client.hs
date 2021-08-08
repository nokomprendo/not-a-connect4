{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import NaC4.Client.Bot
import qualified NaC4.Game as G
import NaC4.Protocol

import Control.Monad.ST
import Data.Massiv.Array hiding (map, reverse)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as U
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import System.Random.MWC
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Text.Read

-------------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        (host:portStr:user:botArgs) -> do
            mBotFunc <- mkBotFunc botArgs
            let mPort = readMaybe portStr
            case (mBotFunc, mPort) of
                (Just botFunc, Just port) -> withSocketsDo 
                    $ WS.runClient host port "" 
                    $ clientApp botFunc (T.pack user)
                _ -> usage
        _ -> usage

usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ "usage: " <> progName <> " host port user bot [botArgs]"
    putStrLn ""
    putStrLn "example: "
    putStrLn $ "  " <> progName <> " 127.0.0.1 3000 myname mcts 512"
    putStrLn $ "  " <> progName <> " not-a-connect4.herokuapp.com 80 myname mc 64"
    putStrLn ""
    putStrLn "bots: "
    putStrLn "  random"
    putStrLn "  mc <nsim>"
    putStrLn "  mcts <nsims>"

-------------------------------------------------------------------------------
-- bot
-------------------------------------------------------------------------------

type BotFunc = Double -> G.Game RealWorld -> ST RealWorld Int

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

-------------------------------------------------------------------------------
-- network
-------------------------------------------------------------------------------

clientApp :: BotFunc -> User -> WS.ClientApp ()
clientApp botFunc user conn = do
    sendMsg (Connect user) conn
    msgToClient <- recvMsg conn
    case msgToClient of
        Just (Connected msg) -> do
            T.putStrLn $ "connected: " <> msg
            run botFunc conn
        Just (NotConnected msg) -> die ("not-connected: " <> T.unpack msg)
        _ -> die "connection failed"

-- TODO run genmove in a thread and kill it if recv endgame

run :: BotFunc -> WS.ClientApp ()
run botFunc conn = do
    msgToClient <- recvMsg conn
    case msgToClient of
        Just (NewGame pr py) -> T.putStrLn $ "newgame: " <> pr <> " " <> py
        Just (GenMove b p s t) -> do
            gameM <- stToIO (toGame b p s)
            case gameM of
                Nothing -> T.putStrLn "genmove: error"
                Just game -> do
                    T.putStrLn $ "genmove: " <> b <> " " <> fmtPlayer p <> " "
                        <> fmtTime t
                    k <- stToIO $ botFunc t game
                    let j = G._moves game U.! k
                    T.putStrLn $ "playmove: " <> T.pack (show j)
                    sendMsg (PlayMove j) conn
        Just (EndGame b p s) -> do
            T.putStrLn $ "endgame: " <> b <> " " <> fmtPlayer p <> " "
                <> fmtStatus s
            mG <- stToIO (toGame b p s)
            case mG of
                Nothing -> T.putStrLn "failed to parse game"
                Just g -> stToIO (showGame g) >>= T.putStrLn
        _ -> die "unknown error"
    run botFunc conn

recvMsg :: WS.ClientApp (Maybe MsgToClient)
recvMsg conn = parseMsgToClient . WS.fromLazyByteString <$> WS.receiveData conn

sendMsg :: MsgToServer -> WS.ClientApp ()
sendMsg msg conn = WS.sendTextData conn $ fmtMsgToServer msg

-------------------------------------------------------------------------------
-- game
-------------------------------------------------------------------------------

formatCell :: G.Cell -> T.Text
formatCell G.CellE = "."
formatCell G.CellR = "R"
formatCell G.CellY = "Y"

showGame :: G.Game s -> ST s T.Text
showGame g = 
    T.unlines . map (T.concat . map formatCell) . reverse . toLists2 
        <$> freezeS (G._cells g)

