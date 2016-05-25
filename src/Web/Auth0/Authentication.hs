{-# LANGUAGE OverloadedStrings #-}

module Web.Auth0.Authentication(
    module Web.Auth0.Types,
    tokenInfo
) where

import Web.Auth0.Types
import Web.Auth0.Common

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO)
import Data.Aeson
import Network.HTTP.Nano

tokenInfo :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, HasAuth0 r) => Token -> m Profile
tokenInfo tok = do
    url <- (++"tokeninfo") <$> auth0URL
    let body = object ["id_token" .= tok]
    req <- addHeaders [("Content-Type", "application/json")] <$> buildReq POST url (mkJSONData body)
    httpJSON req
