{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module NaC4.Server.Model where

import NaC4.Game as G
import NaC4.Protocol as P

import Control.Concurrent.STM
import Control.Monad.ST (RealWorld)
import qualified Data.Aeson as A
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import GHC.Generics
import Lens.Micro.Platform
import qualified Network.WebSockets as WS

-------------------------------------------------------------------------------
-- types
-------------------------------------------------------------------------------

data Battle = Battle
    { _bGame        :: Game RealWorld
    , _bTimeR       :: Double
    , _bTimeY       :: Double
    , _bTimeI       :: Double
    }
makeLenses ''Battle
type BattleKey = (User, User)

data Result = Result
    { _rUserR   :: User
    , _rUserY   :: User
    , _rBoard   :: P.Board
    , _rStatus  :: G.Status
    , _rTimeR   :: Double
    , _rTimeY   :: Double
    } deriving (Generic)
instance A.ToJSON Result
makeLenses ''Result

data UserStats = UserStats
    { _usWins   :: Int
    , _usLoses  :: Int
    , _usTies   :: Int
    , _usGames  :: Int
    , _usTime   :: Double
    } deriving (Eq, Generic, Show)
makeLenses ''UserStats
instance A.ToJSON UserStats

newUserStats :: UserStats
newUserStats = UserStats 0 0 0 0 0

data Model = Model
    { _mClients     :: M.Map User WS.Connection
    , _mWaiting     :: S.Set User
    , _mNbGames     :: M.Map BattleKey Int
    , _mBattles     :: M.Map BattleKey Battle
    , _mResults     :: [Result]
    , _mUserStats   :: M.Map User UserStats
    }
makeLenses ''Model

newModel :: Model
newModel = Model M.empty S.empty M.empty M.empty [] M.empty

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

addClient :: TVar Model -> User -> WS.Connection -> STM Bool
addClient modelVar user conn = do
    m <- readTVar modelVar
    if M.member user (m^.mClients)
        then return False
        else do 
            let users = M.keys (m^.mClients)
                nbGames1 = [((user,u),0) | u<-users]
                nbGames2 = [((u,user),0) | u<-users]
            writeTVar modelVar $ m 
                & mClients %~ M.insert user conn
                & mWaiting %~ S.insert user
                & mNbGames %~ M.unionWith max (M.fromList $ nbGames1 ++ nbGames2)
                & mUserStats %~ M.insertWith (\_new old -> old) user newUserStats
            return True

delClient :: TVar Model -> User -> STM ()
delClient modelVar user = do
    m <- readTVar modelVar
    let (bs0, bs1) = M.partitionWithKey (\k _ -> userInBattleKey user k) (m^.mBattles)
        insertOpponents = 
            flip $ M.foldlWithKey' (\si k _ -> S.insert (opponent user k) si)
    if null bs0
    then writeTVar modelVar $ m & mClients %~ M.delete user
                                & mWaiting %~ S.delete user
    else writeTVar modelVar $ m & mClients %~ M.delete user
                                & mWaiting %~ insertOpponents bs0
                                & mBattles .~ bs1

userInBattleKey :: User -> BattleKey -> Bool
userInBattleKey user (userR, userY) = user == userR || user == userY

opponent :: User -> BattleKey -> User
opponent user (userR, userY) = 
    if user == userR then userY else userR

finishBattle :: TVar Model -> BattleKey -> Battle -> P.Board -> G.Status -> STM ()
finishBattle modelVar b@(userR,userY) bt board status = do
    m <- readTVar modelVar
    let (Battle _ timeR timeY _) = bt
    writeTVar modelVar $ m 
        & mWaiting %~ flip (foldr S.insert) [userR, userY]
        & mBattles %~ M.delete b
        & mResults %~ (Result userR userY board status timeR timeY :)
        & mNbGames %~ M.insertWith (+) (userR, userY) 1
        & mUserStats %~ M.adjust (updateStats PlayerR status timeR) userR
        & mUserStats %~ M.adjust (updateStats PlayerY status timeY) userY

updateStats :: G.Player -> G.Status -> Double -> UserStats -> UserStats
updateStats PlayerR WinR t us0 = us0 & usWins  +~ 1 & usGames +~ 1 & usTime +~ t
updateStats PlayerR WinY t us0 = us0 & usLoses +~ 1 & usGames +~ 1 & usTime +~ t
updateStats PlayerR Tie  t us0 = us0 & usTies  +~ 1 & usGames +~ 1 & usTime +~ t
updateStats PlayerY WinR t us0 = us0 & usLoses +~ 1 & usGames +~ 1 & usTime +~ t
updateStats PlayerY WinY t us0 = us0 & usWins  +~ 1 & usGames +~ 1 & usTime +~ t
updateStats PlayerY Tie  t us0 = us0 & usTies  +~ 1 & usGames +~ 1 & usTime +~ t
updateStats _ _ _ us0 = us0

clearAll :: TVar Model -> STM ()
clearAll modelVar =
    modifyTVar' modelVar $ \m -> 
        let cs = m^.mClients
            users = M.keys cs
        in m & mBattles .~ mempty
             & mWaiting .~ S.fromList users
             & mResults .~ []
             & mUserStats .~ M.map (const newUserStats) cs
             & mNbGames .~ M.fromList [((ur,uy), 0) | ur<-users, uy<-users, ur/=uy]

