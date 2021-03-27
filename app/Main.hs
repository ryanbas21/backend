{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    object,
    withObject,
    (.:),
  )
import Data.Aeson.Types ()
import Data.Text ()
import Data.Time (Day)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Handler,
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )
import Servant.API ()
import User (EmailPassword (..), User (..))

type RegisterAPI =
  "register" :> ReqBody '[JSON] User :> Post '[JSON] User
    :<|> "login" :> ReqBody '[JSON] Login :> Post '[JSON] User

newtype Login = Login EmailPassword deriving (Eq, Show, Generic)

instance ToJSON Login where
  toJSON (Login (EmailPassword email password)) =
    object
      [ "email" .= email,
        "password" .= password
      ]

instance FromJSON Login where
  parseJSON = withObject "Login" $ \o -> do
    email <- o .: "email"
    password <- o .: "password"
    pure $ Login $ EmailPassword email password

userAPI :: Proxy RegisterAPI
userAPI = Proxy

server1 :: Server RegisterAPI
server1 = registerUser :<|> loginUser
  where
    loginUser :: Login -> Handler User
    loginUser
      (Login (EmailPassword email password)) = return $ User $ EmailPassword email password

    registerUser :: User -> Handler User
    registerUser user = return user

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI server1

main :: IO ()
main = run 8081 app1
