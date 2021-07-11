{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module NaC4.Server.HttpApp (httpApp) where

import NaC4.Protocol
import NaC4.Server.Model
import NaC4.Server.View

import Control.Concurrent.STM
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import Lens.Micro.Platform
import Servant
import Servant.HTML.Lucid

type ApiResultsRoute = "api" :> "results" :> Get '[JSON] [Result]
type ApiUsersRoute = "api" :> "users" :> Get '[JSON] (M.Map User UserStats)
type ApiUsersVgRoute = "api" :> "users-vg" :> Get '[JSON] [UsersVg]
type ApiGamesVgRoute = "api" :> "games-vg" :> Get '[JSON] [GamesVg]
type HomeRoute = Get '[HTML] HomeData

type ServerApi
    =    ApiResultsRoute
    :<|> ApiUsersRoute
    :<|> ApiUsersVgRoute
    :<|> ApiGamesVgRoute
    :<|> HomeRoute

handleServerApi :: TVar Model -> Server ServerApi
handleServerApi modelVar
    =    handleGetInModel _mResults modelVar
    :<|> handleGetInModel _mUserStats modelVar
    :<|> handleUsersVg modelVar
    :<|> handleGamesVg modelVar
    :<|> handleHome modelVar

handleGetInModel :: (Model -> a) -> TVar Model -> Handler a
handleGetInModel f modelVar = liftIO (f <$> readTVarIO modelVar)

handleUsersVg :: TVar Model -> Handler [UsersVg]
handleUsersVg modelVar = do
    stats <- liftIO (M.toList . _mUserStats <$> readTVarIO modelVar)
    return $ map (\(u, UserStats w l t _) -> UsersVg u w l t) stats

handleGamesVg :: TVar Model -> Handler [GamesVg]
handleGamesVg modelVar = do
    ngames <- liftIO (M.toList . _mNbGames <$> readTVarIO modelVar)
    return $ map (\((ur, uy), n) -> GamesVg ur uy n) ngames

handleHome :: TVar Model -> Handler HomeData
handleHome modelVar = do
    m <- liftIO $ readTVarIO modelVar
    pure $ HomeData (m^.mResults, m^.mUserStats)

httpApp :: TVar Model -> Application
httpApp modelVar = serve (Proxy @ServerApi) (handleServerApi modelVar)

