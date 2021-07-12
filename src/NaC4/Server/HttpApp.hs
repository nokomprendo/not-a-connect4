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
import Servant
import Servant.HTML.Lucid

type ApiResultsRoute = "api" :> "results" :> Get '[JSON] [Result]
type ApiUsersRoute = "api" :> "users" :> Get '[JSON] (M.Map User UserStats)
type ApiUsersVgRoute = "api" :> "users-vg" :> Get '[JSON] [UsersVg]
type ApiGamesVgRoute = "api" :> "games-vg" :> Get '[JSON] [GamesVg]
type ApiTimeVgRoute = "api" :> "time-vg" :> Get '[JSON] [TimeVg]
type HomeRoute = Get '[HTML] HomeData

type ServerApi
    =    ApiResultsRoute
    :<|> ApiUsersRoute
    :<|> ApiUsersVgRoute
    :<|> ApiGamesVgRoute
    :<|> ApiTimeVgRoute
    :<|> HomeRoute

handleServerApi :: TVar Model -> Server ServerApi
handleServerApi modelVar
    =    handleGetInModel _mResults modelVar
    :<|> handleGetInModel _mUserStats modelVar
    :<|> handleUsersVg modelVar
    :<|> handleGamesVg modelVar
    :<|> handleTimeVg modelVar
    :<|> handleHome modelVar

handleGetInModel :: (Model -> a) -> TVar Model -> Handler a
handleGetInModel f modelVar = liftIO (f <$> readTVarIO modelVar)

handleUsersVg :: TVar Model -> Handler [UsersVg]
handleUsersVg modelVar = do
    stats <- handleGetInModel (M.toList . _mUserStats) modelVar
    let fmt (u, us) = UsersVg u (_usWins us) (_usLoses us) (_usTies us)
    return $ map fmt stats

handleGamesVg :: TVar Model -> Handler [GamesVg]
handleGamesVg modelVar = do
    ngames <- handleGetInModel (M.toList . _mNbGames) modelVar
    return $ map (\((ur, uy), n) -> GamesVg ur uy n) ngames

handleTimeVg :: TVar Model -> Handler [TimeVg]
handleTimeVg modelVar = do
    stats <- handleGetInModel (M.toList . _mUserStats) modelVar
    let fmt (u, us) =   let t = _usTime us
                            g = _usGames us
                        in TimeVg u t g (t / fromIntegral g)
    return $ map fmt stats

handleHome :: TVar Model -> Handler HomeData
handleHome modelVar = do
    m <- liftIO $ readTVarIO modelVar
    pure $ HomeData (_mResults m, _mUserStats m)

httpApp :: TVar Model -> Application
httpApp modelVar = serve (Proxy @ServerApi) (handleServerApi modelVar)

