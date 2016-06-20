{-# LANGUAGE OverloadedStrings #-}

module Web.Auth0.Authentication(
    module Web.Auth0.Types,
    login,
    tokenInfo
) where

import Web.Auth0.Types
import Web.Auth0.Common

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO)
import Data.Aeson
import Data.List (intersperse)
import Network.HTTP.Nano

-- |Login with username/password
login :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, HasAuth0 r) => String -> String -> String -> String -> [String] -> m AuthToken
login clientID connection username password scopes = do
    url <- (++"oauth/ro") <$> auth0URL
    let body = object ["client_id" .= clientID, "connection" .= connection, "username" .= username, "password" .= password, "grant_type" .= ("password" :: String), "scope" .= concat (intersperse " " scopes)]
    req <- addHeaders [("Content-Type", "application/json")] <$> buildReq POST url (mkJSONData body)
    httpJSON req

-- |Get a user profile given a valid Auth0 token
tokenInfo :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, HasAuth0 r, FromJSON a, FromJSON b) => Token -> m (Profile' a b)
tokenInfo tok = do
    url <- (++"tokeninfo") <$> auth0URL
    let body = object ["id_token" .= tok]
    req <- addHeaders [("Content-Type", "application/json")] <$> buildReq POST url (mkJSONData body)
    httpJSON req
