{-# LANGUAGE NumericUnderscores #-}

import NaC4.Server.HttpApp
import NaC4.Server.Model
import NaC4.Server.Runner
import NaC4.Server.WsApp

import Control.Concurrent (forkIO)
import Data.IORef (newIORef)
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Environment (lookupEnv)

main :: IO ()
main = do
    port <- read . fromMaybe "3000" <$> lookupEnv "PORT"
    putStrLn $ "listening port " ++ show port ++ "..."
    modelRef <- newIORef newModel
    _ <- forkIO $ loopBattles 1_000_000 modelRef
    run port 
        $ logStdout 
        $ wsApp modelRef
        (httpApp modelRef)

