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

data MsgToServer
    = Connect Player Pool
    | PlayMove Move
    deriving (Eq, Show)

data MsgToClient
    = Connected Text
    | NotConnected Text
    | NewGame Player Player
    | GenMove Board Color
    | EndGame Board Result
    deriving (Eq, Show)

