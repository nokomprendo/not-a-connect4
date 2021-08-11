{-# LANGUAGE NumericUnderscores #-}

module NaC4.Params where

viewNbResults :: Int
viewNbResults = 20

wsBattleTime :: Double
wsBattleTime = 10.0

wsIdleDelay :: Int
wsIdleDelay = 500_000

wsMaxNbGames :: Int
wsMaxNbGames = 10

wsPenaltyTime :: Double
wsPenaltyTime = 1.0

wsPenaltyTimeUs :: Int
wsPenaltyTimeUs = truncate $ 1e6 * wsPenaltyTime

