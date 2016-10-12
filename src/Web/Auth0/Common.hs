module Web.Auth0.Common
    ( auth0URL
    , a0Req )
where

import Web.Auth0.Types

import Control.Lens (view)
import Control.Monad.Reader (MonadReader)
import Network.HTTP.Nano

auth0URL :: (MonadReader r m, HasAuth0 r) => m String
auth0URL = do
    app <- view auth0Application
    return $ "https://" ++ app ++ ".auth0.com/"

a0Req :: (HttpM m r e, HasAuth0 r) => HttpMethod -> String -> RequestData -> m Request
a0Req mthd path dta = do
    url <- (++path) <$> auth0URL
    tok <- view auth0Token
    addHeaders [("Content-Type", "application/json"),("Authorization", "Bearer " ++ tok)] <$> buildReq mthd url dta
