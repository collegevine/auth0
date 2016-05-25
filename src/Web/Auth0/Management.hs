
module Web.Auth0.Management(
    module Web.Auth0.Types,
    searchUsers
) where

import Web.Auth0.Types
import Web.Auth0.Common

import Control.Lens (view)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO)
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Nano
import Network.HTTP.Types.URI (urlEncode)

searchUsers :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, HasAuth0 r) => Query -> m [Profile]
searchUsers q = do
    let path = "api/v2/users?search_engine=v2&q=" ++ (B.unpack . urlEncode False . B.pack $ show q)
    url <- (++path) <$> auth0URL
    tok <- view auth0Token
    httpJSON . addHeaders [("Content-Type", "application/json"),("Authorization", "Bearer " ++ tok)] =<< buildReq GET url NoRequestData
