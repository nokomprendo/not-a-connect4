{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module NaC4.Server.Model where

import NaC4.Game as G
import NaC4.Protocol as P

import Control.Concurrent.STM
import Control.Monad.ST (RealWorld)
import qualified Data.Aeson as A
import Data.List (partition, foldl')
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import GHC.Generics
import Lens.Micro.Platform
import qualified Network.WebSockets as WS

-------------------------------------------------------------------------------
-- types
-------------------------------------------------------------------------------

data Battle = Battle
    { _bUserR       :: User
    , _bUserY       :: User
    , _bGame        :: Game RealWorld
    , _bTimeR       :: Double
    , _bTimeY       :: Double
    , _bTimeI       :: Double
    }
makeLenses ''Battle

instance Eq Battle where
    b1 == b2 = b1^.bUserR == b2^.bUserR && b1^.bUserY == b2^.bUserY

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
    -- , _mWaiting     :: [User]       -- TODO Set ?
    , _mWaiting     :: S.Set User
    , _mNbGames     :: M.Map (User, User) Int
    , _mBattles     :: [Battle]     -- TODO map ?
    , _mResults     :: [Result]
    , _mUserStats   :: M.Map User UserStats
    }
makeLenses ''Model

newModel :: Model
newModel = Model M.empty S.empty M.empty [] [] M.empty

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

addClient :: TVar Model -> User -> WS.Connection -> IO Bool
addClient modelVar user conn =  atomically $ do
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

delClient :: TVar Model -> User -> IO ()
delClient modelVar user = atomically $ do
    m <- readTVar modelVar
    case partition (userInBattle user) (m^.mBattles) of
        ([], _) -> writeTVar modelVar $ m 
            & mClients %~ M.delete user
            & mWaiting %~ S.delete user
        (bs0, bs1) -> do
            writeTVar modelVar $ m 
                & mClients %~ M.delete user
                & mWaiting %~ insertList (map (opponent user) bs0) 
                & mBattles .~ bs1

finishBattle :: TVar Model -> Battle -> P.Board -> G.Status -> IO ()
finishBattle modelVar battle board status = atomically $ do
    let (Battle userR userY _ timeR timeY _) = battle
        result = Result userR userY board status timeR timeY
    m <- readTVar modelVar
    writeTVar modelVar $ m 
        & mWaiting %~ S.union (S.fromList [userR, userY]) 
        & mBattles %~ filter (/=battle)
        & mResults %~ (result:)
        & mNbGames %~ M.insertWith (+) (userR, userY) 1
        & mUserStats %~ M.adjust (updateStats PlayerR status timeR) userR
        & mUserStats %~ M.adjust (updateStats PlayerY status timeY) userY

-- TODO test
insertList :: Ord a => [a] -> S.Set a -> S.Set a
insertList l s = foldl' (flip S.insert) s l

updateStats :: G.Player -> G.Status -> Double -> UserStats -> UserStats
updateStats PlayerR WinR t us0 = us0 & usWins  +~ 1 & usGames +~ 1 & usTime +~ t
updateStats PlayerR WinY t us0 = us0 & usLoses +~ 1 & usGames +~ 1 & usTime +~ t
updateStats PlayerR Tie  t us0 = us0 & usTies  +~ 1 & usGames +~ 1 & usTime +~ t
updateStats PlayerY WinR t us0 = us0 & usLoses +~ 1 & usGames +~ 1 & usTime +~ t
updateStats PlayerY WinY t us0 = us0 & usWins  +~ 1 & usGames +~ 1 & usTime +~ t
updateStats PlayerY Tie  t us0 = us0 & usTies  +~ 1 & usGames +~ 1 & usTime +~ t
updateStats _ _ _ us0 = us0

userInBattle :: User -> Battle -> Bool
userInBattle user battle = user == battle^.bUserR || user == battle^.bUserY

-- TODO test
usersInBattle :: User -> User -> Battle -> Bool
usersInBattle userR userY battle = userR == battle^.bUserR || userY == battle^.bUserY

opponent :: User -> Battle -> User
opponent user battle = 
    if user == battle^.bUserR then battle^.bUserY else battle^.bUserR

-- TODO refactor ?
clearAll :: TVar Model -> IO ()
clearAll modelVar =
    atomically $ modifyTVar' modelVar $ \m -> 
        m & mBattles .~ []
            & mWaiting .~ S.fromList (M.keys (m^.mClients))
            & mResults .~ []
            & mUserStats %~ M.filterWithKey (\u _ -> M.member u (m^.mClients))
            & mUserStats %~ M.map (const newUserStats)
            & mNbGames %~ M.filterWithKey (\(ur,uy) _ -> M.member ur (m^.mClients) && M.member uy (m^.mClients))
            & mNbGames %~ M.map (const 0)

