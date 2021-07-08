
import NaC4.Server.HttpApp
import NaC4.Server.Model
import NaC4.Server.WsApp

import Control.Concurrent (newMVar, forkIO)
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Environment (lookupEnv)

main :: IO ()
main = do
    port <- read . fromMaybe "3000" <$> lookupEnv "PORT"
    putStrLn $ "listening port " ++ show port ++ "..."
    model <- newModel
    modelVar <- newMVar model
    -- _ <- forkIO $ loopWsModel wsModelVar
    run port 
        $ logStdout 
        $ wsApp modelVar
        httpApp

