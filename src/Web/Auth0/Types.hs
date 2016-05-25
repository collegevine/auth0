{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Web.Auth0.Types where

import Control.Lens.TH
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, iso8601DateFormat, parseTimeM)

type Token = String

data Auth0 = Auth0 {
    _auth0Application :: String,
    _auth0Token :: String
}

data Query where
    (:=) :: String -> Term -> Query
    (:&) :: Query -> Query -> Query
    (:|) :: Query -> Query -> Query
    QPARENS :: Query -> Query

infixl 9 `QPARENS`
infixl 8 :&
infixl 7 :|

data Term where
    TEQ :: Show a => a -> Term
    (:<->) :: Show a => a -> a -> Term

infixl 6 `TEQ`
infixl 5 :<->

instance Show Query where
    show (k := t) = k ++ ":" ++ show t
    show (q1 :& q2) = show q1 ++ " AND " ++ show q2
    show (q1 :| q2) = show q1 ++ " OR " ++ show q2
    show (QPARENS q) = "(" ++ show q ++ ")"

instance Show Term where
    show (TEQ v) = show v
    show (v1 :<-> v2) = "[" ++ show v1 ++ " TO " ++ show v2 ++ "]"

data Profile = Profile {
    _profileID :: String,
    _profileBlocked :: Maybe Bool,
    _profileCreated :: Maybe UTCTime,
    _profileUpdated :: Maybe UTCTime,
    _profileLastLogin :: Maybe UTCTime,
    _profileEmail :: Maybe String,
    _profileEmailVerified :: Maybe Bool,
    _profilePhoneNumber :: Maybe String,
    _profilePhoneNumberVerified :: Maybe Bool,
    _profileIdentities :: [Identity],
    _profileUsername :: Maybe String,
    _profileName :: Maybe String,
    _profileNickname :: Maybe String,
    _profilePicture :: Maybe String,
    _profileUserMeta :: Maybe (M.Map String Value),
    _profileAppMeta :: Maybe (M.Map String Value)
} deriving Show

data Identity = Identity {
    _identityConnection :: String,
    _identityIsSocial :: Bool,
    _identityProvider :: String,
    _identityUserID :: String
} deriving Show

instance FromJSON Profile where
    parseJSON (Object v) =
        Profile <$> v .: "user_id"
                <*> v .:? "blocked"
                <*> from8601 "created_at"
                <*> from8601 "updated_at"
                <*> from8601 "last_login"
                <*> v .:? "email"
                <*> v .:? "email_verified"
                <*> v .:? "phone_number"
                <*> v .:? "phone_verified"
                <*> v .: "identities"
                <*> v .:? "username"
                <*> v .:? "name"
                <*> v .:? "nickname"
                <*> v .:? "picture"
                <*> v .:? "user_metadata"
                <*> v .:? "app_metadata"
        where
        fmt = iso8601DateFormat (Just "%H:%M:%S%QZ")
        from8601 k = do
            ms <- v .:? k
            return $ do
                s <- ms
                parseTimeM True defaultTimeLocale fmt s
    parseJSON _ = mzero

instance FromJSON Identity where
    parseJSON (Object v) =
        Identity    <$> v .: "connection"
                    <*> v .: "isSocial"
                    <*> v .: "provider"
                    <*> v .: "user_id"
    parseJSON _ = mzero

makeClassy ''Auth0
makeLenses ''Profile
makeLenses ''Identity
