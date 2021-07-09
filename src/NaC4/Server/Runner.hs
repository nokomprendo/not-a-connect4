
module NaC4.Server.Runner where

import NaC4.Server.Config
import NaC4.Server.Model

import Control.Concurrent (threadDelay)
import Data.IORef (IORef, atomicModifyIORef')
import Lens.Micro.Platform

loopRunner :: IORef Model -> IO ()
loopRunner modelRef = do
    threadDelay runnerSleep
    atomicModifyIORef' modelRef (\m -> (m&counter+~1, m^.counter)) >>= print
    loopRunner modelRef


