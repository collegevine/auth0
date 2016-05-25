
module Web.Auth0.Common(
    auth0URL
) where

import Web.Auth0.Types

import Control.Lens (view)
import Control.Monad.Reader (MonadReader)

auth0URL :: (MonadReader r m, HasAuth0 r) => m String
auth0URL = do
    app <- view auth0Application
    return $ "https://" ++ app ++ ".auth0.com/"
