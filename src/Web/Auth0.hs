module Web.Auth0(

    module Web.Auth0.Types,
    module Web.Auth0.Authentication,
    module Web.Auth0.Management,
    module Web.Auth0.JWT,
    module Web.Auth0.Stats,

    getVerifiedEmails,
    getVerifiedPhones
) where

import Web.Auth0.Types
import Web.Auth0.Authentication
import Web.Auth0.Management
import Web.Auth0.JWT
import Web.Auth0.Stats

import Control.Lens ((^.))
import Data.Maybe (catMaybes)

-- |Get all verified emails attached to a profile, or any child profile (via linked accounts)
getVerifiedEmails :: Profile' a b -> [String]
getVerifiedEmails prof = catMaybes (email (prof ^. profileData):subs)
    where
    idx = prof ^. profileIdentities
    email d = do
        ver <- d ^. profileEmailVerified
        case ver of
            False -> Nothing
            True -> d ^. profileEmail
    subs = flip fmap idx $ \i -> do
        prof <- i ^. identityProfileData
        email prof

-- |Get all verified phone numbers attached to a profile, or any child profile (via linked accounts)
getVerifiedPhones :: Profile' a b -> [String]
getVerifiedPhones prof = catMaybes (phone (prof ^. profileData):subs)
    where
    idx = prof ^. profileIdentities
    phone d = do
        ver <- d ^. profilePhoneNumberVerified
        case ver of
            False -> Nothing
            True -> d ^. profilePhoneNumber
    subs = flip fmap idx $ \i -> do
        prof <- i ^. identityProfileData
        phone prof
