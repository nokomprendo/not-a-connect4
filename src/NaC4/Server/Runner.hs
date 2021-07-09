{-# LANGUAGE NumericUnderscores #-}

module NaC4.Server.Runner where

import NaC4.Server.Config
import NaC4.Server.Model

import Control.Concurrent (threadDelay)
import Data.IORef (IORef, atomicModifyIORef')
import Lens.Micro.Platform

sleepTime = 1_000_000 :: Int
nbGames = 10 :: Int

loopRunner :: IORef Model -> IO ()
loopRunner modelRef = do
    threadDelay runnerSleep
    atomicModifyIORef' modelRef (\m -> (m&counter+~1, m^.counter)) >>= print
    loopRunner modelRef


