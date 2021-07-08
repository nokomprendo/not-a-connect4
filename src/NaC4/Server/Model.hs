module NaC4.Server.Model where

import NaC4.Game

import Control.Monad.ST

data Model = Model
    { _game :: Game RealWorld
    } 

newModel :: IO Model
newModel = stToIO (Model <$> (mkGame PlayerR >>= playK 2))


