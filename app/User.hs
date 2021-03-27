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
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module User where

import Crypto.KDF.BCrypt
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    object,
    withObject,
    (.:),
  )
import qualified Data.ByteString as BS
import Data.Char (isAscii)
import qualified Data.Text as T (Text, all, length, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Validation (Validation (Failure, Success))
import GHC.Generics (Generic)
import Text.Email.Validate (isValid)

data EmailPassword = EmailPassword T.Text T.Text
  deriving (Eq, Show, Generic)

newtype Error
  = Error [T.Text]
  deriving (Eq, Show)

instance Semigroup Error where
  (Error a) <> (Error b) = Error (a <> b)

instance ToJSON EmailPassword

instance FromJSON EmailPassword

data User
  = User EmailPassword
  | FailedRegistration Error
  deriving (Eq, Show, Generic)

instance ToJSON User where
  toJSON (User (EmailPassword _email _password)) =
    object ["email" .= _email, "password" .= _password]

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> do
    email <- o .: "email"
    password <- o .: "password"
    pure $ User $ EmailPassword email password

validateUserRegistration :: User -> User
validateUserRegistration (FailedRegistration (Error err)) = FailedRegistration $ Error err
validateUserRegistration (User usr) = case checkEmail usr of
  Success a -> case validatePassword usr of
    Success _ -> User usr
    Failure err -> FailedRegistration err
  Failure err -> case validatePassword usr of
    Success _ -> FailedRegistration err
    Failure pwErrs -> FailedRegistration $ err <> pwErrs
  where
    validatePassword :: EmailPassword -> Validation Error T.Text
    validatePassword pass = checkPasswordLength pass *> checkPasswordAlphaNumeric pass

    checkEmail :: EmailPassword -> Validation Error T.Text
    checkEmail (EmailPassword email password) =
      if isValid $ encodeUtf8 email
        then Success email
        else Failure $ Error [T.pack "Email must be valid"]

    checkPasswordLength :: EmailPassword -> Validation Error T.Text
    checkPasswordLength (EmailPassword email password) =
      if T.length password > 7
        then Success password
        else Failure $ Error [T.pack "Password must be at least 8 characters"]

    checkPasswordAlphaNumeric :: EmailPassword -> Validation Error T.Text
    checkPasswordAlphaNumeric (EmailPassword email password) =
      if T.all isAscii password
        then Success password
        else Failure $ Error [T.pack "Password can only contain numbers and letters"]
