{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Web.Auth0.Management(
    module Web.Auth0.Types,
    searchUsers,
    getUser,
    deleteUser,
    blockUser,
    createEmailUser,
    createPhoneUser,
    updateUserFrom,
    setEmail,
    setPhone,
    setAppMetadata,
    linkProfile
) where

import Web.Auth0.Types
import Web.Auth0.Common

import Control.Lens (view, (^.))
import Data.Aeson
import Data.Maybe (listToMaybe, fromMaybe)
import Network.HTTP.Nano
import qualified Data.ByteString.Char8 as B

type Auth0M m r e = (HttpM m r e, HasAuth0 r)

-- |Search users based on a lucene query into user profile fields
searchUsers :: (Auth0M m r e, FromJSON a, FromJSON b) => Query -> m [Profile' a b]
searchUsers query = do
    let q    = B.unpack (renderQueryUrlEncoded query)
        path = "api/v2/users?search_engine=v2&per_page=100&q=" ++ q
    searchPages path 0

searchPages :: (Auth0M m r e, FromJSON a, FromJSON b) => String -> Int -> m [Profile' a b]
searchPages bpath page = do
    let path = bpath ++ "&page=" ++ show page
    profs <- httpJSON =<< a0Req GET path NoRequestData
    case profs of
        [] -> return profs
        _ -> do
            profs' <- searchPages bpath (page + 1)
            return $ profs ++ profs'

-- |Get a user by ID
getUser :: (Auth0M m r e, FromJSON a, FromJSON b) => String -> m (Profile' a b)
getUser uid = httpJSON =<< a0Req GET ("api/v2/users/"++uid) NoRequestData

-- |Delete a user
deleteUser :: Auth0M m r e => String -> m ()
deleteUser uid = http' =<< a0Req DELETE ("api/v2/users/"++uid) NoRequestData

-- |Create an email user
createEmailUser :: (Auth0M m r e, HasAuth0 r, FromJSON a, FromJSON b) => NewEmailUser -> m (Profile' a b)
createEmailUser dta = httpJSON =<< a0Req POST "api/v2/users" (mkJSONData dta)

-- |Create a phone user
createPhoneUser :: (Auth0M m r e, HasAuth0 r, FromJSON a, FromJSON b) => NewPhoneUser -> m (Profile' a b)
createPhoneUser dta = httpJSON =<< a0Req POST "api/v2/users" (mkJSONData dta)

-- | Copy as much data fields from given 'Profile' as possible and update
-- user specified by provided ID with it.

updateUserFrom :: (Auth0M m r e, HasAuth0 r)
    => String          -- ^ ID of user to update
    -> Profile         -- ^ Where to get data from
    -> m ()
updateUserFrom uid profile = do
    let val = object
            [ "blocked"        .= view profileBlocked profile
            , "email_verified" .= view (profileData . profileEmailVerified) profile
            , "email"          .= view (profileData . profileEmail) profile
            , "phone_number"   .= view (profileData . profilePhoneNumber) profile
            , "phone_verified" .= view (profileData . profilePhoneNumberVerified) profile
            , "user_metadata"  .= view profileUserMeta profile
            , "app_metadata"   .= view profileAppMeta profile
            , "username"       .= view (profileData . profileUsername) profile ]
    http' =<< a0Req PATCH ("api/v2/users/" ++ uid) (mkJSONData val)

-- Set the blocked flag for a user
blockUser :: (Auth0M m r e, HasAuth0 r, FromJSON a, FromJSON b) => String -> Bool -> m (Profile' a b)
blockUser userId b= httpJSON =<< a0Req PATCH ("api/v2/users/"++userId) j
    where j = mkJSONData $ object ["blocked" .= b]

-- |Set the email address of a profile
setEmail :: (Auth0M m r e, FromJSON a, FromJSON b) => String -> String -> m (Profile' a b)
setEmail uid email = do
    let dta = mkJSONData $ object ["email" .= email, "verify_email" .= False, "email_verified" .= True]
    httpJSON =<< a0Req PATCH ("api/v2/users/"++uid) dta

-- |Set the phone number of a profile
setPhone :: (Auth0M m r e, FromJSON a, FromJSON b) => String -> String -> m (Profile' a b)
setPhone uid phone = do
    let dta = mkJSONData $ object ["phone_number" .= phone, "verify_phone_number" .= False, "phone_verified" .= True]
    httpJSON =<< a0Req PATCH ("api/v2/users/"++uid) dta

setAppMetadata :: (Auth0M m r e, ToJSON d, FromJSON a, FromJSON b) => String -> d -> m (Profile' a b)
setAppMetadata uid metaData = do
    let dta = mkJSONData $ object [ "app_metadata" .= toJSON metaData ]
    httpJSON =<< a0Req PATCH ("api/v2/users/"++uid) dta

linkProfile :: Auth0M m r e => String -> String -> m ()
linkProfile rootID subID = do
    sp <- getUser subID
    let dta = mkJSONData $ object
          [ "provider" .= fromMaybe "" (getProvider sp)
          , "user_id" .= subID ]
    http' =<< a0Req POST ("api/v2/users/" ++ rootID ++ "/identities") dta

getProvider :: Profile' Value Value -> Maybe String
getProvider p = do
    ident <- listToMaybe $ p ^. profileIdentities
    return $ ident ^. identityProvider

--
-- Utility
--

a0Req :: Auth0M m r e => HttpMethod -> String -> RequestData -> m Request
a0Req mthd path dta = do
    url <- (++path) <$> auth0URL
    tok <- view auth0Token
    addHeaders [("Content-Type", "application/json"),("Authorization", "Bearer " ++ tok)] <$> buildReq mthd url dta
