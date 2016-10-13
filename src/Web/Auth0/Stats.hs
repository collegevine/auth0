module Web.Auth0.Stats
  ( getActiveUsersCount
  , getDailyStats )
where

import Data.Monoid ((<>))
import Data.Time
import Network.HTTP.Nano
import Web.Auth0.Common
import Web.Auth0.Types

-- | Get number of active users — logged in during the last 30 days.

getActiveUsersCount :: (HttpM m r e, HasAuth0 r) => m Int
getActiveUsersCount =
    let path = "api/v2/stats/active-users"
    in httpJSON =<< a0Req GET path NoRequestData

-- | Get the daily stats for a particular period.

getDailyStats :: (HttpM m r e, HasAuth0 r)
    => Day             -- ^ The first day of the period (inclusive)
    -> Day             -- ^ The last day of the period (inclusive)
    -> m [DailyStats]
getDailyStats from to = do
    let f :: Day -> String
        f = formatTime defaultTimeLocale "%0Y%m%d"
        path = "api/v2/stats/daily?from=" <> f from <> "&to=" <> f to
    httpJSON =<< a0Req GET path NoRequestData
