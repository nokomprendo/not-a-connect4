{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import NaC4.Client.Bot
import qualified NaC4.Game as G
import NaC4.Protocol
import NaC4.Utils

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.ST
import Data.Foldable (forM_)
import qualified Data.Massiv.Array as A
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
            mMyBot <- parseMyBot botArgs
            let mPort = readMaybe portStr
            case (mMyBot, mPort) of
                (Just myBot, Just port) -> withSocketsDo 
                    $ WS.runClient host port "" 
                    $ clientApp myBot (T.pack user)
                _ -> usage
        _ -> usage

usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ "usage: " <> progName <> " host port user bot [botArgs]"
    putStrLn ""
    putStrLn "example: "
    putStrLn $ "  " <> progName <> " 127.0.0.1 3000 my-mcts mcts 50000"
    putStrLn $ "  " <> progName <> " not-a-connect4.herokuapp.com 80 my-mc mc 10000"
    putStrLn ""
    putStrLn "bots: "
    putStrLn "  random"
    putStrLn "  zero"
    putStrLn "  mctime"
    putStrLn "  mctstime"
    putStrLn "  mc <nsim>"
    putStrLn "  mcts <nsims>"

-------------------------------------------------------------------------------
-- network
-------------------------------------------------------------------------------

clientApp :: MyBot -> User -> WS.ClientApp ()
clientApp myBot user conn = do
    sendMsg (Connect user) conn
    msgToClient <- recvMsg conn
    case msgToClient of
        Just (Connected msg) -> do
            T.putStrLn $ "connected: " <> msg
            modelVar <- newTVarIO (mkModel myBot)
            run modelVar conn
        Just (NotConnected msg) -> die ("not-connected: " <> T.unpack msg)
        _ -> die "connection failed"

run :: TVar Model -> WS.ClientApp ()
run modelVar conn = do
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
                    stopThread modelVar
                    void $ forkIO $ startThread modelVar t game conn

        Just (EndGame b p s bs) -> do
            stopThread modelVar
            T.putStrLn $ "endgame: " <> b <> " " <> fmtPlayer p <> " "
                <> fmtStatus s <> " " <> fmtBattleStatus bs
            mG <- stToIO (toGame b p s)
            case mG of
                Nothing -> T.putStrLn "failed to parse game"
                Just g -> stToIO (showGame g) >>= T.putStrLn
        _ -> die "unknown error"

    run modelVar conn

recvMsg :: WS.ClientApp (Maybe MsgToClient)
recvMsg conn = parseMsgToClient . WS.fromLazyByteString <$> WS.receiveData conn

sendMsg :: MsgToServer -> WS.ClientApp ()
sendMsg msg conn = WS.sendTextData conn $ fmtMsgToServer msg

-------------------------------------------------------------------------------
-- MyBot
-------------------------------------------------------------------------------

data MyBot
    = MyBotRandom (BotRandom RealWorld)
    | MyBotMc (BotMc RealWorld)
    | MyBotMcts (BotMcts RealWorld)
    | MyBotZero BotZero
    | MyBotMcTime BotMcTimeIO
    | MyBotMctsTime BotMctsTimeIO

parseMyBot :: [String] -> IO (Maybe MyBot)
parseMyBot ["random"] = Just . MyBotRandom . BotRandom <$> createSystemRandom
parseMyBot ["zero"] = Just . MyBotRandom . BotRandom <$> createSystemRandom
parseMyBot ["mctime"] = Just . MyBotMcTime . BotMcTimeIO <$> createSystemRandom
parseMyBot ["mc", nsimsStr] = do
    gen <- createSystemRandom
    return $ do
        nsims <- readMaybe nsimsStr
        Just $ MyBotMc $ BotMc nsims gen
parseMyBot ["mcts", nsimsStr] = do
    gen <- createSystemRandom
    return $ do
        nsims <- readMaybe nsimsStr
        Just $ MyBotMcts $ BotMcts nsims gen
parseMyBot _ = return Nothing

myGenmove :: MyBot -> Double -> G.Game RealWorld -> IO Int
myGenmove (MyBotRandom b) t g = stToIO $ genmove b t g
myGenmove (MyBotMc b) t g = stToIO $ genmove b t g
myGenmove (MyBotMcts b) t g = stToIO $ genmove b t g
myGenmove (MyBotZero b) t g = stToIO $ genmove b t g
myGenmove (MyBotMcTime b) t g = genmoveIO b t g
myGenmove (MyBotMctsTime b) t g = genmoveIO b t g

-------------------------------------------------------------------------------
-- model
-------------------------------------------------------------------------------

data Model = Model
    { _mThread :: Maybe ThreadId
    , _mBot :: MyBot
    }

mkModel :: MyBot -> Model
mkModel = Model Nothing 

stopThread :: TVar Model -> IO ()
stopThread modelVar = do
    maybeThreadId <- atomically $ do
        model <- readTVar modelVar
        writeTVar modelVar model { _mThread = Nothing }
        return $ _mThread model
    forM_ maybeThreadId $ \threadId -> do 
        T.putStrLn "thread killed"
        killThread threadId

startThread :: TVar Model -> Double -> G.Game RealWorld -> WS.Connection -> IO ()
startThread modelVar time game conn = do
    threadId <- myThreadId
    myBot <- atomically $ do
        model <- readTVar modelVar
        writeTVar modelVar model { _mThread = Just threadId }
        return $ _mBot model
    k <- myGenmove myBot time game
    let j = G._moves game A.! k
    T.putStrLn $ "playmove: " <> T.pack (show j)
    sendMsg (PlayMove j) conn
    atomically $ modifyTVar' modelVar (\m -> m { _mThread = Nothing })

