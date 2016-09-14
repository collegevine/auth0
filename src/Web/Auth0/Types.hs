{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Auth0.Types where

import Control.Lens.TH
import Control.Monad (mzero)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, iso8601DateFormat, parseTimeM)
import Network.HTTP.Nano
import Network.HTTP.Types.URI (urlEncode)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M

type Token = String
type HttpM m r e = (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r)

data Auth0 = Auth0 {
    _auth0Application :: String,
    _auth0Token :: String,
    _auth0Secret :: String,
    _auth0ClientID :: String
}

data Query where
    (:=) :: String -> Term -> Query
    (:&) :: Query -> Query -> Query
    (:|) :: Query -> Query -> Query

infixl 2 :&
infixl 3 :|

data Term where
    TEQ :: IsTerm a => a -> Term
    (:<->) :: IsTerm a => a -> a -> Term

newtype Unquoted a = Unquoted a

class IsTerm a where
    renderTerm :: a -> B.Builder

instance IsTerm Int where
    renderTerm = B.intDec

instance IsTerm Float where
    renderTerm = B.floatDec

instance IsTerm Double where
    renderTerm = B.doubleDec

instance IsTerm [Char] where
    renderTerm x = "\"" <> B.stringUtf8 x <> "\""

instance IsTerm (Unquoted [Char]) where
    renderTerm (Unquoted x) = B.stringUtf8 x

buildTerm :: Term -> B.Builder
buildTerm (TEQ x) = renderTerm x
buildTerm (x :<-> y) = "[" <> renderTerm x <> " TO " <> renderTerm y <> "]"

infixl 5 `TEQ`
infixl 4 :<->

buildQuery :: Query -> B.Builder
buildQuery (k := t) = B.stringUtf8 k <> ":" <> buildTerm t
buildQuery (q1 :& q2) = "(" <> buildQuery q1 <> ") AND (" <> buildQuery q2 <> ")"
buildQuery (q1 :| q2) = "(" <> buildQuery q1 <> ") OR ("  <> buildQuery q2 <> ")"

-- | Render and URL encode a 'Query'. This function supports Unicode in
-- 'Term's.

renderQueryUrlEncoded :: Query -> ByteString
renderQueryUrlEncoded =
    urlEncode False . B.toStrict . B.toLazyByteString . buildQuery

type Profile = Profile' (Maybe (M.Map String Value)) (Maybe (M.Map String Value))

data Profile' a b = Profile' {
    _profileID :: String,
    _profileBlocked :: Maybe Bool,
    _profileCreated :: Maybe UTCTime,
    _profileUpdated :: Maybe UTCTime,
    _profileLastLogin :: Maybe UTCTime,
    _profileIdentities :: [Identity],
    _profileUserMeta :: Maybe a,
    _profileAppMeta :: Maybe b,
    _profileData :: ProfileData
} deriving Show

data ProfileData = ProfileData {
    _profileEmail :: Maybe String,
    _profileEmailVerified :: Maybe Bool,
    _profilePhoneNumber :: Maybe String,
    _profilePhoneNumberVerified :: Maybe Bool,
    _profileUsername :: Maybe String,
    _profileName :: Maybe String,
    _profileNickname :: Maybe String,
    _profilePicture :: Maybe String
} deriving Show

data Identity = Identity {
    _identityConnection :: String,
    _identityIsSocial :: Bool,
    _identityProvider :: String,
    _identityUserID :: String,
    _identityProfileData :: Maybe ProfileData
} deriving Show

instance (FromJSON a, FromJSON b) => FromJSON (Profile' a b) where
    parseJSON (Object v) =
        Profile' <$> v .: "user_id"
                <*> v .:? "blocked"
                <*> from8601 "created_at"
                <*> from8601 "updated_at"
                <*> from8601 "last_login"
                <*> v .: "identities"
                <*> v .:? "user_metadata"
                <*> v .:? "app_metadata"
                <*> parseProfileData v
        where
        fmt = iso8601DateFormat (Just "%H:%M:%S%QZ")
        from8601 k = do
            ms <- v .:? k
            return $ do
                s <- ms
                parseTimeM True defaultTimeLocale fmt s
    parseJSON _ = mzero

instance FromJSON ProfileData where
    parseJSON (Object v) = parseProfileData v
    parseJSON _ = mzero

parseProfileData :: HM.HashMap Text Value -> Parser ProfileData
parseProfileData v =
    ProfileData <$> v .:? "email"
                <*> v .:? "email_verified"
                <*> v .:? "phone_number"
                <*> v .:? "phone_verified"
                <*> v .:? "username"
                <*> v .:? "name"
                <*> v .:? "nickname"
                <*> v .:? "picture"

instance FromJSON Identity where
    parseJSON (Object v) =
        Identity    <$> v .: "connection"
                    <*> v .: "isSocial"
                    <*> v .: "provider"
                    <*> v .: "user_id"
                    <*> v .:? "profileData"
    parseJSON _ = mzero

data AuthToken = AuthToken {
    _authTokenIDToken :: String,
    _authTokenAccessToken :: String
} deriving Show

instance FromJSON AuthToken where
    parseJSON (Object v) = AuthToken <$> v .: "id_token" <*> v .: "access_token"
    parseJSON _ = mzero

data NewEmailUser = NewEmailUser {
    _newEmailUserConnection :: String,
    _newEmailUserEmail :: String,
    _newEmailUserPassword :: String,
    _newEmailUserName :: String,
    _newEmailUserNickname :: String
} deriving Show

instance ToJSON NewEmailUser where
    toJSON u =
        object [
            "connection" .= _newEmailUserConnection u,
            "email" .= _newEmailUserEmail u,
            "password" .= _newEmailUserPassword u,
            "name" .= _newEmailUserName u,
            "nickname" .= _newEmailUserNickname u,
            "email_verified" .= True
        ]

instance FromJSON NewEmailUser where
    parseJSON (Object v) =
        NewEmailUser    <$> v .: "connection"
                        <*> v .: "email"
                        <*> v .: "password"
                        <*> v .: "name"
                        <*> v .: "nickname"
    parseJSON _ = mzero

data NewPhoneUser = NewPhoneUser {
    _newPhoneUserPhone :: String
} deriving Show

instance ToJSON NewPhoneUser where
    toJSON u =
        object [
            "connection" .= ("sms" :: String),
            "phone_number" .= _newPhoneUserPhone u
        ]

instance FromJSON NewPhoneUser where
    parseJSON (Object v) = NewPhoneUser <$> v .: "phone"
    parseJSON _ = mzero

data TokenInfo a = TokenInfo {
    _tokenInfoISS :: String,
    _tokenInfoSUB :: String,
    _tokenInfoAUD :: String,
    _tokenInfoEXP :: Int,
    _tokenInfoIAT :: Int,
    _tokenInfoAppMetadata :: Maybe a
} deriving Show

instance FromJSON a => FromJSON (TokenInfo a) where
    parseJSON (Object v) =
        TokenInfo   <$> v .: "iss"
                    <*> v .: "sub"
                    <*> v .: "aud"
                    <*> v .: "exp"
                    <*> v .: "iat"
                    <*> v .:? "app_metadata"
    parseJSON _ = mzero

instance ToJSON a => ToJSON (TokenInfo a) where
    toJSON i =
        object [
            "app_metadata" .= _tokenInfoAppMetadata i,
            "iss" .= _tokenInfoISS i,
            "sub" .= _tokenInfoSUB i,
            "aud" .= _tokenInfoAUD i,
            "exp" .= _tokenInfoEXP i,
            "iat" .= _tokenInfoIAT i
        ]

makeClassy ''Auth0
makeLenses ''Profile'
makeLenses ''ProfileData
makeLenses ''Identity
makeLenses ''AuthToken
makeLenses ''NewEmailUser
makeLenses ''NewPhoneUser
makeLenses ''TokenInfo
