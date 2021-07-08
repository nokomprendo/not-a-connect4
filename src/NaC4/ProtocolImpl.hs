{-# LANGUAGE OverloadedStrings #-}

module NaC4.ProtocolImpl where

import NaC4.Protocol as P
import NaC4.Game as G

import Control.Monad.ST
import qualified Data.Massiv.Array as A
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import Text.Read

-------------------------------------------------------------------------------
-- parse message
-------------------------------------------------------------------------------

parseMsgToServer :: T.Text -> Maybe MsgToServer
parseMsgToServer input = case T.words input of
    ["connect", player, pool] -> Just $ Connect player pool
    ["playmove", move] -> PlayMove <$> readMaybe (T.unpack move)
    _ -> Nothing

parseMsgToClient :: T.Text -> Maybe MsgToClient
parseMsgToClient input = case T.words input of
    ("connected":xs) -> Just $ Connected (T.unwords xs)
    ("not-connected":xs) -> Just $ NotConnected (T.unwords xs)
    ["newgame", pr, py] -> Just $ NewGame pr py 
    ["genmove", board, color] -> GenMove board <$> parseColor color
    ["endgame", b, res] -> EndGame b <$> parseResult res
    _ -> Nothing

parseColor :: T.Text -> Maybe Color
parseColor "R" = Just ColorR
parseColor "Y" = Just ColorY
parseColor _ = Nothing

parseResult :: T.Text -> Maybe Result
parseResult "WinR" = Just P.WinR
parseResult "WinY" = Just P.WinY
parseResult "Draw" = Just P.Draw
parseResult _ = Nothing

-------------------------------------------------------------------------------
-- format message
-------------------------------------------------------------------------------

fmtMsgToServer :: MsgToServer -> T.Text
fmtMsgToServer (Connect player pool) = fmtMsg ["connect", player, pool]
fmtMsgToServer (PlayMove move) = fmtMsg ["playmove", T.pack (show move)]

fmtMsgToClient :: MsgToClient -> T.Text
fmtMsgToClient (Connected msg) = fmtMsg ["connected", msg]
fmtMsgToClient (NotConnected msg) = fmtMsg ["not-connected", msg]
fmtMsgToClient (NewGame pr py) = fmtMsg ["newgame", pr, py]
fmtMsgToClient (GenMove b c) = fmtMsg ["genmove", b, fmtColor c]
fmtMsgToClient (EndGame b res) = fmtMsg ["endgame", b, fmtResult res]

fmtColor :: Color -> T.Text
fmtColor ColorR = "R"
fmtColor ColorY = "Y"

fmtResult :: Result -> T.Text
fmtResult P.WinR = "WinR"
fmtResult P.WinY = "WinY"
fmtResult P.Draw = "Draw"

fmtMsg :: [T.Text] -> T.Text
fmtMsg xs = T.unwords (xs ++ ["\n"])

-------------------------------------------------------------------------------
-- to/from Game
-------------------------------------------------------------------------------

fmtCell :: Cell -> String
fmtCell CellE = "."
fmtCell CellR = "R"
fmtCell CellY = "Y"

fromGame :: Game s -> ST s (P.Board, P.Color)
fromGame g = do
    b <- T.pack . concatMap fmtCell . A.toList <$> A.freezeS (_cells g)
    let c = if _currentPlayer g == G.PlayerR then ColorR else ColorY
    return (b, c)

parseCell :: Char -> Cell
parseCell 'R' = CellR
parseCell 'Y' = CellY
parseCell _ = CellE

toGame :: P.Board -> P.Color -> ST s (Maybe (Game s))
toGame b c 
    | T.length b /= nI*nJ = return Nothing
    | otherwise = do
        let (s, p) = if c == ColorR then (PlayR, PlayerR) else (PlayY, PlayerY)
            go acc [] = acc
            go acc xs = 
                let (x1, x2) = splitAt (nJ-1) xs
                in go (acc++[x1]) x2
            bb = go [] (map parseCell $ T.unpack b)
        arr <- A.thawS (A.fromLists' A.Seq bb)
        let moves = [ j | (x,j) <- zip (last bb) [0..], x==CellE ]
        return $ Just $ Game s p p (U.fromList moves) arr

