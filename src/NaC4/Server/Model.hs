{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module NaC4.Server.Model where

import NaC4.Game as G
import NaC4.Protocol as P

import Control.Concurrent.STM
import Control.Monad.ST (RealWorld)
import qualified Data.Aeson as A
import Data.List
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

instance Eq Battle where
    b1 == b2 = b1^.bUserR == b2^.bUserR && b1^.bUserY == b2^.bUserY

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
    } deriving (Eq, Generic, Show)
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
newModel = Model M.empty [] M.empty [] [] M.empty

addClient :: TVar Model -> User -> WS.Connection -> IO Bool
addClient modelVar user conn =  atomically $ do
    m <- readTVar modelVar
    if M.member user (m^.mClients)
        then return False
        else do 
            writeTVar modelVar $ m 
                & mClients %~ M.insert user conn
                & mWaiting %~ (user:)
                & mUserStats %~ M.insertWith (\_new old -> old) user (UserStats 0 0 0 0)
            return True

delClient :: TVar Model -> User -> IO ()
delClient modelVar user = atomically $ do
    m <- readTVar modelVar
    case partition (isInBattle user) (m^.mBattles) of
        ([], _) -> writeTVar modelVar $ m 
            & mClients %~ M.delete user
            & mWaiting %~ filter (/=user)
        (bs0, bs1) -> do
            writeTVar modelVar $ m 
                & mClients %~ M.delete user
                & mWaiting %~ (++ map (opponent user) bs0) 
                & mBattles .~ bs1

finishBattle :: TVar Model -> Battle -> P.Board -> G.Status -> IO ()
finishBattle modelVar battle board status = atomically $ do
    let userR = battle^.bUserR
        userY = battle^.bUserY
        result = Result userR userY board status
    m <- readTVar modelVar
    writeTVar modelVar $ m 
        & mWaiting %~ (++[userY, userR]) 
        & mBattles %~ filter (/=battle)
        & mResults %~ (result:)
        & mNbGames %~ M.insertWith (+) (userR, userY) 1
        & mUserStats %~ M.adjust (updateStats PlayerR status) userR
        & mUserStats %~ M.adjust (updateStats PlayerY status) userY

updateStats :: G.Player -> G.Status -> UserStats -> UserStats
updateStats PlayerR WinR us0 = us0 & usWins +~ 1 & usGames +~ 1
updateStats PlayerR WinY us0 = us0 & usLoses +~ 1 & usGames +~ 1
updateStats PlayerR Tie us0 = us0 & usTies +~ 1 & usGames +~ 1
updateStats PlayerY WinR us0 = us0 & usLoses +~ 1 & usGames +~ 1
updateStats PlayerY WinY us0 = us0 & usWins +~ 1 & usGames +~ 1
updateStats PlayerY Tie us0 = us0 & usTies +~ 1 & usGames +~ 1
updateStats _ _ us0 = us0

isInBattle :: User -> Battle -> Bool
isInBattle user battle = user == battle^.bUserR || user == battle^.bUserY

opponent :: User -> Battle -> User
opponent user (Battle ur uy _ _) = if user == ur then uy else ur

