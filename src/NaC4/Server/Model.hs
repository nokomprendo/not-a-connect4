{-# LANGUAGE TemplateHaskell #-}

module NaC4.Server.Model where

import NaC4.Game as G
import NaC4.Protocol as P

import Control.Monad.ST
import Data.IORef
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

addClient :: IORef Model -> P.Player -> WS.Connection -> IO Bool
addClient modelRef player conn =  
    atomicModifyIORef' modelRef $ \m -> 
        if M.member player $ m^.clients
            then (m, False)
            else (m & clients %~ M.insert player conn
                    & waiting %~ (player:), True) 

-- cs <- map fst . M.toList . _clients <$> readIORef modelRef

