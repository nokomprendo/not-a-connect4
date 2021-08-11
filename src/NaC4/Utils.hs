{-# LANGUAGE OverloadedStrings #-}

module NaC4.Utils where

import qualified NaC4.Game as G

import Control.Monad.ST
import qualified Data.Massiv.Array as A
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getCurrentTime, utcTimeToPOSIXSeconds)

myGetTime :: IO Double
myGetTime = 
    let itod = fromIntegral :: Int -> Double
    in (0.001*) . itod . round <$> ((1000*) . utcTimeToPOSIXSeconds <$> getCurrentTime)

formatCell :: G.Cell -> T.Text
formatCell G.CellE = "."
formatCell G.CellR = "R"
formatCell G.CellY = "Y"

showGame :: G.Game s -> ST s T.Text
showGame g = 
    T.unlines . map (T.concat . map formatCell) . reverse . A.toLists2 
        <$> A.freezeS (G._cells g)

