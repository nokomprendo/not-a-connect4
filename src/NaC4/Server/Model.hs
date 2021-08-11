{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module NaC4.Server.Model where

import NaC4.Game as G
import NaC4.Params as Params
import NaC4.Protocol as P

import Control.Monad (guard)
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

-- TODO Game RealWorld -> Game s ?

data Battle = Battle
    { _bGame        :: Game RealWorld
    , _bBoard       :: P.Board
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

userInBattleKey :: User -> BattleKey -> Bool
userInBattleKey user (userR, userY) = user == userR || user == userY

opponent :: User -> BattleKey -> User
opponent user (userR, userY) = 
    if user == userR then userY else userR

updateStats :: G.Player -> G.Status -> Double -> UserStats -> UserStats
updateStats PlayerR WinR t us0 = us0 & usWins  +~ 1 & usGames +~ 1 & usTime +~ t
updateStats PlayerR WinY t us0 = us0 & usLoses +~ 1 & usGames +~ 1 & usTime +~ t
updateStats PlayerR Tie  t us0 = us0 & usTies  +~ 1 & usGames +~ 1 & usTime +~ t
updateStats PlayerY WinR t us0 = us0 & usLoses +~ 1 & usGames +~ 1 & usTime +~ t
updateStats PlayerY WinY t us0 = us0 & usWins  +~ 1 & usGames +~ 1 & usTime +~ t
updateStats PlayerY Tie  t us0 = us0 & usTies  +~ 1 & usGames +~ 1 & usTime +~ t
updateStats _ _ _ us0 = us0

findTimeouts :: Double -> Model -> [(BattleKey, Battle, Status)]
findTimeouts time m = M.foldlWithKey' (accTimeout time) [] (m^.mBattles)
    where 
        accTimeout t acc bk bt = 
            let p = _currentPlayer $ _bGame bt
                (tp, s) = 
                    if p == PlayerR then (bt^.bTimeR, WinY) else (bt^.bTimeY, WinR)
                tt = tp + t - bt^.bTimeI
            in if tt > wsBattleTime 
                then (bk, bt, s) : acc
                else acc

findFirstGame :: Model -> Maybe BattleKey
findFirstGame m = 
    let fGames = M.filterWithKey $ \(ur,uy) _ ->
                    M.member ur (m^.mClients) 
                    && M.member uy (m^.mClients)
                    && M.notMember (uy,uy) (m^.mBattles)
                    && (m^.mNbGames) M.! (ur,uy) < Params.wsMaxNbGames
        fAcc Nothing ki ai = Just (ki,ai)
        fAcc (Just (k0,a0)) ki ai = if ai<a0 then Just (ki,ai) else Just (k0,a0)
    in fst <$> M.foldlWithKey' fAcc Nothing (fGames $ m^.mNbGames)

checkUser :: User -> Model -> Maybe (BattleKey, Battle)
checkUser user m1 = do
    let bs = m1^.mBattles & M.filterWithKey (\ k _ -> userInBattleKey user k)
    guard (M.size bs == 1)
    let (bk@(userR,userY), bt) = M.elemAt 0 bs
        g0 = bt^.bGame
    guard (_currentPlayer g0 == G.PlayerR && user == userR
            || _currentPlayer g0 == G.PlayerY && user == userY)
    return (bk, bt)

updateBattle :: BattleKey -> Game RealWorld -> Game RealWorld -> P.Board
             -> Double -> Double -> Model -> Model
updateBattle (userR,userY) g0 g1 b1 time penalty m =
    let f bt = let dt = time - bt^.bTimeI
               in if _currentPlayer g0 == G.PlayerR
                  then bt & bGame.~g1 & bBoard.~b1 & bTimeI.~time & bTimeR+~dt+penalty
                  else bt & bGame.~g1 & bBoard.~b1 & bTimeI.~time & bTimeY+~dt+penalty
    in m & mBattles %~ M.adjust f (userR, userY)

