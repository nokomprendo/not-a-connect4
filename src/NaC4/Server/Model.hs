module NaC4.Server.Model where

import NaC4.Game

import Control.Monad.ST
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Network.WebSockets as WS

data Model = Model
    { _clients :: M.Map T.Text WS.Connection
    , _game :: Game RealWorld
    } 

newModel :: IO Model
newModel = stToIO (Model M.empty <$> mkGame PlayerR)

