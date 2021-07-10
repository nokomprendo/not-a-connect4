{-# LANGUAGE TemplateHaskell #-}

module NaC4.Server.Model where

import NaC4.Game as G
import NaC4.Protocol as P

import Control.Concurrent.STM
import Control.Monad.ST
import qualified Data.Map.Strict as M
import Lens.Micro.Platform
import qualified Network.WebSockets as WS

type Battle = (User, User, WS.Connection, WS.Connection, Game RealWorld)

data Model = Model
    { _clients :: M.Map User WS.Connection
    , _battles :: [Battle]
    , _waiting :: [User]
    }

makeLenses ''Model

isInBattle :: User -> Battle -> Bool
isInBattle user (userR, userY, _, _, _) = user == userR || user == userY

newModel :: Model
newModel = Model M.empty [] []

addClient :: TVar Model -> User -> WS.Connection -> IO Bool
addClient modelVar user conn =  atomically $ do
    m <- readTVar modelVar
    if M.member user $ m^.clients
        then return False
        else do 
            writeTVar modelVar (m & clients %~ M.insert user conn
                                  & waiting %~ (user:))
            return True

rmClient :: TVar Model -> User -> IO ()
rmClient modelVar user = atomically $ do
    m <- readTVar modelVar
    -- TODO stop battle and put opponent in waiting
    writeTVar modelVar (m & clients %~ M.delete user
                          & battles %~ filter (not . isInBattle user)
                          & waiting %~ filter (/=user))

