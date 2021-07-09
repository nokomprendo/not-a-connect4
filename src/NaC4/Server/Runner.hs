
module NaC4.Server.Runner where

import NaC4.Server.Model

import Control.Concurrent (threadDelay)
import Data.IORef (IORef, atomicModifyIORef')
import Lens.Micro.Platform

loopBattles :: Int -> IORef Model -> IO ()
loopBattles sleep modelRef = do
    threadDelay sleep
    atomicModifyIORef' modelRef (\m -> (m&counter+~1, m^.counter)) >>= print
    loopBattles sleep modelRef


