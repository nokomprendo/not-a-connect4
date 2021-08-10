
module NaC4.Utils where

import Data.Time.Clock.POSIX (getCurrentTime, utcTimeToPOSIXSeconds)

myGetTime :: IO Double
myGetTime = 
    let itod = fromIntegral :: Int -> Double
    in (0.001*) . itod . round <$> ((1000*) . utcTimeToPOSIXSeconds <$> getCurrentTime)

