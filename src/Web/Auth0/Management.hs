{-# LANGUAGE OverloadedStrings #-}

module Web.Auth0.Management(
    module Web.Auth0.Types,
    searchUsers,
    getUser,
    createEmailUser,
    setEmail,
    setPhone
) where

import Web.Auth0.Types
import Web.Auth0.Common

import Control.Lens (view)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO)
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Nano
import Network.HTTP.Types.URI (urlEncode)

-- |Search users based on a lucene query into user profile fields
searchUsers :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, HasAuth0 r) => Query -> m [Profile]
searchUsers q = do
    let path = "api/v2/users?search_engine=v2&q=" ++ (B.unpack . urlEncode False . B.pack $ show q)
    httpJSON =<< a0Req GET path NoRequestData

-- |Get a user by ID
getUser :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, HasAuth0 r) => String -> m Profile
getUser uid = httpJSON =<< a0Req GET ("api/v2/users/"++uid) NoRequestData

-- |Create an email user
createEmailUser :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, HasAuth0 r) => NewEmailUser -> m Profile
createEmailUser dta = httpJSON =<< a0Req POST "api/v2/users" (mkJSONData dta)

-- |Set the email address of a profile
setEmail :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, HasAuth0 r) => String -> String -> m Profile
setEmail uid email = do
    let dta = mkJSONData $ object ["email" .= email, "verify_email" .= False, "email_verified" .= True]
    httpJSON =<< a0Req (CustomMethod "PATCH") ("api/v2/users/"++uid) dta

-- |Set the phone number of a profile
setPhone :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, HasAuth0 r) => String -> String -> m Profile
setPhone uid phone = do
    let dta = mkJSONData $ object ["phone_number" .= phone, "verify_phone_number" .= False, "phone_verified" .= True]
    httpJSON =<< a0Req (CustomMethod "PATCH") ("api/v2/users/"++uid) dta

--
-- Utility
--

a0Req :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, HasAuth0 r) => HttpMethod -> String -> RequestData -> m Request
a0Req mthd path dta = do
    url <- (++path) <$> auth0URL
    tok <- view auth0Token
    addHeaders [("Content-Type", "application/json"),("Authorization", "Bearer " ++ tok)] <$> buildReq mthd url dta
