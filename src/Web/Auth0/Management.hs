{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Web.Auth0.Management(
    module Web.Auth0.Types,
    searchUsers,
    getUser,
    createEmailUser,
    setEmail,
    setPhone,
    setAppMetadata,
) where

import Web.Auth0.Types
import Web.Auth0.Common

import Control.Lens (view)
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Nano
import Network.HTTP.Types.URI (urlEncode)

type Auth0M m r e = (HttpM m r e, HasAuth0 r)

-- |Search users based on a lucene query into user profile fields
searchUsers :: (Auth0M m r e, FromJSON a, FromJSON b) => Query -> m [Profile' a b]
searchUsers q = do
    let path = "api/v2/users?search_engine=v2&q=" ++ (B.unpack . urlEncode False . B.pack $ show q)
    httpJSON =<< a0Req GET path NoRequestData

-- |Get a user by ID
getUser :: (Auth0M m r e, FromJSON a, FromJSON b) => String -> m (Profile' a b)
getUser uid = httpJSON =<< a0Req GET ("api/v2/users/"++uid) NoRequestData

-- |Create an email user
createEmailUser :: (Auth0M m r e, HasAuth0 r, FromJSON a, FromJSON b) => NewEmailUser -> m (Profile' a b)
createEmailUser dta = httpJSON =<< a0Req POST "api/v2/users" (mkJSONData dta)

-- |Set the email address of a profile
setEmail :: (Auth0M m r e, FromJSON a, FromJSON b) => String -> String -> m (Profile' a b)
setEmail uid email = do
    let dta = mkJSONData $ object ["email" .= email, "verify_email" .= False, "email_verified" .= True]
    httpJSON =<< a0Req pATCH ("api/v2/users/"++uid) dta

-- |Set the phone number of a profile
setPhone :: (Auth0M m r e, FromJSON a, FromJSON b) => String -> String -> m (Profile' a b)
setPhone uid phone = do
    let dta = mkJSONData $ object ["phone_number" .= phone, "verify_phone_number" .= False, "phone_verified" .= True]
    httpJSON =<< a0Req pATCH ("api/v2/users/"++uid) dta

setAppMetadata :: (Auth0M m r e, ToJSON d, FromJSON a, FromJSON b) => String -> d -> m (Profile' a b)
setAppMetadata uid metaData = do
    let dta = mkJSONData $ object [ "app_metadata" .= object ["data" .= toJSON metaData ]] 
    httpJSON =<< a0Req pATCH ("api/v2/users/"++uid) dta

--
-- Utility
--

a0Req :: Auth0M m r e => HttpMethod -> String -> RequestData -> m Request
a0Req mthd path dta = do
    url <- (++path) <$> auth0URL
    tok <- view auth0Token
    addHeaders [("Content-Type", "application/json"),("Authorization", "Bearer " ++ tok)] <$> buildReq mthd url dta

pATCH = CustomMethod "PATCH"
