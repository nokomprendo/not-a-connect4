module NaC4.Protocol where

import Data.Text

type Player = Text
type Pool = Text
type Move = Int
type Board = Text

data Color
    = ColorR
    | ColorY
    deriving (Eq, Show)

data Result
    = WinR
    | WinY
    | Draw
    deriving (Eq, Show)

data Protocol
    = Connect Player Pool
    | Connected Text
    | NotConnected Text
    | NewGame Player Player
    | GenMove Board Color
    | PlayMove Move
    | EndGame Board Result
    deriving (Eq, Show)

