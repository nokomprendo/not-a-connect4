module NaC4.Server.Params where

viewNbResults :: Int
viewNbResults = 20

wsMaxNbGames :: Int
wsMaxNbGames = 10

wsBattleTime :: Double
wsBattleTime = 21.0

wsPenaltyTime :: Double
wsPenaltyTime = 1.0

wsPenaltyTimeUs :: Int
wsPenaltyTimeUs = truncate $ 1e6 * wsPenaltyTime

