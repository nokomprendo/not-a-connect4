{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module NaC4.Server.HttpApp (httpApp) where

import NaC4.Server.Model
import NaC4.Server.View

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Lens.Micro.Platform
import Servant
import Servant.HTML.Lucid

type ApiResultsRoute = "api" :> "results" :> Get '[JSON] [Result]
type ApiUsersVgRoute = "api" :> "users-vg" :> Get '[JSON] [UsersVg]
type ApiGamesVgRoute = "api" :> "games-vg" :> Get '[JSON] [GamesVg]
type ClearRoute = "clear" :> Get '[PlainText] String
type HomeRoute = Get '[HTML] HomeData

type ServerApi
    =    ApiResultsRoute
    :<|> ApiUsersVgRoute
    :<|> ApiGamesVgRoute
    :<|> ClearRoute
    :<|> HomeRoute

handleServerApi :: TVar Model -> Server ServerApi
handleServerApi modelVar
    =    handleGetInModel _mResults modelVar
    :<|> handleUsersVg modelVar
    :<|> handleGamesVg modelVar
    :<|> handleClear modelVar
    :<|> pure HomeData

handleGetInModel :: (Model -> a) -> TVar Model -> Handler a
handleGetInModel f modelVar = liftIO (f <$> readTVarIO modelVar)

handleUsersVg :: TVar Model -> Handler [UsersVg]
handleUsersVg modelVar =
    let safeDiv n d = if d==0 then 0 else n / fromIntegral d
        fmt (u, UserStats w l t g tm) = UsersVg u w l t g tm (safeDiv tm g)
    in map fmt <$> handleGetInModel (M.toList . _mUserStats) modelVar

handleGamesVg :: TVar Model -> Handler [GamesVg]
handleGamesVg modelVar = 
    let fmt ((ur, uy), n) = GamesVg ur uy n
    in map fmt <$> handleGetInModel (M.toList . _mNbGames) modelVar

handleClear :: TVar Model -> Handler String
handleClear modelVar = liftIO (atomically $ clearAll modelVar) >> return "done"

httpApp :: TVar Model -> Application
httpApp modelVar = serve (Proxy @ServerApi) (handleServerApi modelVar)

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

