{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module NaC4.Server.Model where

import NaC4.Game as G
import NaC4.Protocol as P

import Control.Concurrent.STM
import Control.Monad.ST
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import GHC.Generics
import Lens.Micro.Platform
import qualified Network.WebSockets as WS

data Battle = Battle
    { _bUserR       :: User
    , _bUserY       :: User
    , _bGame        :: Game RealWorld
    , _bNbGgames    :: Int
    }
makeLenses ''Battle

data Result = Result
    { _rUserR    :: User
    , _rUserY    :: User
    , _rBoard    :: P.Board
    , _rStatus   :: G.Status
    } deriving (Generic)
instance A.ToJSON Result
makeLenses ''Result

data UserStats = UserStats
    { _usWins    :: Int
    , _usLoses   :: Int
    , _usTies    :: Int
    , _usGames   :: Int
    } deriving (Generic)
makeLenses ''UserStats
instance A.ToJSON UserStats

data Model = Model
    { _mClients     :: M.Map User WS.Connection
    , _mWaiting     :: [User]
    , _mNbGames     :: M.Map (User, User) Int
    , _mBattles     :: [Battle]
    , _mResults     :: [Result]
    , _mUserStats   :: M.Map User UserStats
    }
makeLenses ''Model

newModel :: Model
newModel = Model M.empty [] M.empty [] [] (M.fromList [("TODO test", UserStats 1 2 3 6)])
-- TODO newModel = Model M.empty [] M.empty [] [] M.empty

addClient :: TVar Model -> User -> WS.Connection -> IO Bool
addClient modelVar user conn =  atomically $ do
    m <- readTVar modelVar
    if M.member user (m^.mClients)
        then return False
        else do 
            writeTVar modelVar (m & mClients %~ M.insert user conn
                                  & mWaiting %~ (user:))
            return True

delClient :: TVar Model -> User -> IO ()
delClient modelVar user = atomically $ do
    m <- readTVar modelVar
    -- TODO stop battle and put opponent in waiting
    writeTVar modelVar (m & mClients %~ M.delete user
                          & mBattles %~ filter (not . isInBattle user)
                          & mWaiting %~ filter (/=user))

isInBattle :: User -> Battle -> Bool
isInBattle user battle = user == battle^.bUserR || user == battle^.bUserY


