{-# LANGUAGE OverloadedStrings #-}

module NaC4.Protocol where

import qualified NaC4.Game as G

import Control.Monad.ST
import qualified Data.Massiv.Array as A
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import Text.Read (readMaybe)

-------------------------------------------------------------------------------
-- types
-------------------------------------------------------------------------------

type Player = T.Text
type Pool = T.Text
type Move = Int
type Board = T.Text

data Color
    = ColorR
    | ColorY
    deriving (Eq, Show)

type Status = G.Status

data MsgToServer
    = Connect Player Pool
    | PlayMove Move
    deriving (Eq, Show)

data MsgToClient
    = Connected T.Text
    | NotConnected T.Text
    | NewGame Player Player
    | GenMove Board Color
    | EndGame Board Status
    deriving (Eq, Show)

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
    ["endgame", b, res] -> EndGame b <$> parseStatus res
    _ -> Nothing

parseColor :: T.Text -> Maybe Color
parseColor "R" = Just ColorR
parseColor "Y" = Just ColorY
parseColor _ = Nothing

parseStatus :: T.Text -> Maybe Status
parseStatus "WinR" = Just G.WinR
parseStatus "WinY" = Just G.WinY
parseStatus "Tie" = Just G.Tie
parseStatus "PlayR" = Just G.PlayR
parseStatus "PlayY" = Just G.PlayY
parseStatus _ = Nothing

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
fmtMsgToClient (EndGame b res) = fmtMsg ["endgame", b, fmtStatus res]

fmtColor :: Color -> T.Text
fmtColor ColorR = "R"
fmtColor ColorY = "Y"

fmtStatus :: Status -> T.Text
fmtStatus G.WinR = "WinR"
fmtStatus G.WinY = "WinY"
fmtStatus G.Tie = "Tie"
fmtStatus G.PlayR = "PlayR"
fmtStatus G.PlayY = "PlayY"

fmtMsg :: [T.Text] -> T.Text
fmtMsg xs = T.unwords (xs ++ ["\n"])

-------------------------------------------------------------------------------
-- to/from Game
-------------------------------------------------------------------------------

fmtCell :: G.Cell -> String
fmtCell G.CellE = "."
fmtCell G.CellR = "R"
fmtCell G.CellY = "Y"

fromGame :: G.Game s -> ST s (Board, Color)
fromGame g = do
    b <- T.pack . concatMap fmtCell . A.toList <$> A.freezeS (G._cells g)
    let c = if G._currentPlayer g == G.PlayerR then ColorR else ColorY
    return (b, c)

parseCell :: Char -> G.Cell
parseCell 'R' = G.CellR
parseCell 'Y' = G.CellY
parseCell _ = G.CellE

toGame :: Board -> Color -> ST s (Maybe (G.Game s))
toGame b c 
    | T.length b /= G.nI*G.nJ = return Nothing
    | otherwise = do
        let (s, p) = if c == ColorR then (G.PlayR, G.PlayerR) else (G.PlayY, G.PlayerY)
            go acc [] = acc
            go acc xs = 
                let (x1, x2) = splitAt G.nJ xs
                in go (acc++[x1]) x2
            bb = go [] (map parseCell $ T.unpack b)
        arr <- A.thawS (A.fromLists' A.Seq bb)
        let moves = [ j | (x,j) <- zip (last bb) [0..], x==G.CellE ]
        return $ Just $ G.Game s p p (U.fromList moves) arr

