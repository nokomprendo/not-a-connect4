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

type User = T.Text
type Move = Int
type Board = T.Text

data MsgToServer
    = Connect User 
    | PlayMove Move
    deriving (Eq, Show)

data MsgToClient
    = Connected T.Text
    | NotConnected T.Text
    | NewGame User User
    | GenMove Board G.Player G.Status
    | EndGame Board G.Player G.Status
    deriving (Eq, Show)

-------------------------------------------------------------------------------
-- parse message
-------------------------------------------------------------------------------

parseMsgToServer :: T.Text -> Maybe MsgToServer
parseMsgToServer input = case T.words input of
    ["connect", user] -> Just $ Connect user
    ["playmove", move] -> PlayMove <$> readMaybe (T.unpack move)
    _ -> Nothing

parseMsgToClient :: T.Text -> Maybe MsgToClient
parseMsgToClient input = case T.words input of
    ("connected":xs) -> Just $ Connected (T.unwords xs)
    ("not-connected":xs) -> Just $ NotConnected (T.unwords xs)
    ["newgame", pr, py] -> Just $ NewGame pr py 
    ["genmove", b, p, s] -> GenMove b <$> parsePlayer p <*> parseStatus s
    ["endgame", b, p, s] -> EndGame b <$> parsePlayer p <*> parseStatus s
    _ -> Nothing

parsePlayer :: T.Text -> Maybe G.Player
parsePlayer "R" = Just G.PlayerR
parsePlayer "Y" = Just G.PlayerY
parsePlayer _ = Nothing

parseStatus :: T.Text -> Maybe G.Status
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
fmtMsgToServer (Connect user) = fmtMsg ["connect", user]
fmtMsgToServer (PlayMove move) = fmtMsg ["playmove", T.pack (show move)]

fmtMsgToClient :: MsgToClient -> T.Text
fmtMsgToClient (Connected msg) = fmtMsg ["connected", msg]
fmtMsgToClient (NotConnected msg) = fmtMsg ["not-connected", msg]
fmtMsgToClient (NewGame pr py) = fmtMsg ["newgame", pr, py]
fmtMsgToClient (GenMove b p s) = fmtMsg ["genmove", b, fmtPlayer p, fmtStatus s]
fmtMsgToClient (EndGame b p s) = fmtMsg ["endgame", b, fmtPlayer p, fmtStatus s]

fmtPlayer :: G.Player -> T.Text
fmtPlayer G.PlayerR = "R"
fmtPlayer G.PlayerY = "Y"

fmtStatus :: G.Status -> T.Text
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

fromGame :: G.Game s -> ST s (Board, G.Player, G.Status)
fromGame g = do
    b <- T.pack . concatMap fmtCell . A.toList <$> A.freezeS (G._cells g)
    return (b, G._currentPlayer g, G._status g)

parseCell :: Char -> G.Cell
parseCell 'R' = G.CellR
parseCell 'Y' = G.CellY
parseCell _ = G.CellE

toGame :: Board -> G.Player -> G.Status -> ST s (Maybe (G.Game s))
toGame b p s 
    | T.length b /= G.nI*G.nJ = return Nothing
    | otherwise = do
        let go acc [] = acc
            go acc xs = 
                let (x1, x2) = splitAt G.nJ xs
                in go (acc++[x1]) x2
            bb = go [] (map parseCell $ T.unpack b)
        arr <- A.thawS (A.fromLists' A.Seq bb)
        let moves = [ j | (x,j) <- zip (last bb) [0..], x==G.CellE ]
        return $ Just $ G.Game s p p (U.fromList moves) arr

