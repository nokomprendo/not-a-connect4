{-# LANGUAGE TemplateHaskell #-}

module NaC4.Server.Model where

import NaC4.Game as G
import NaC4.Protocol as P

import Control.Concurrent.STM
import Control.Monad.ST
import qualified Data.Map.Strict as M
import Lens.Micro.Platform
import qualified Network.WebSockets as WS

type Battle = (P.Player, P.Player, Game RealWorld)

data Model = Model
    { _clients :: M.Map P.Player WS.Connection
    , _battles :: [Battle]
    , _waiting :: [P.Player]
    , _counter :: Int
    }

makeLenses ''Model

isInBattle :: P.Player -> Battle -> Bool
isInBattle p (pr, py, _) = p == pr || p == py

newModel :: Model
newModel = Model M.empty [] [] 0

addClient :: TVar Model -> P.Player -> WS.Connection -> IO Bool
addClient modelVar player conn =  atomically $ do
    m <- readTVar modelVar
    if M.member player $ m^.clients
        then return False
        else do 
            writeTVar modelVar (m & clients %~ M.insert player conn
                                  & waiting %~ (player:))
            return True

