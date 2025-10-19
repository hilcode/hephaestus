module Hilcode.Clock (
    Handle (..),
    new,
    utcToText,
) where

import Data.Text (Text)
import Data.Text qualified
import Data.Time (
    UTCTime,
    ZonedTime,
 )
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)

data Handle monad
    = Handle
    { getUtcTime :: monad UTCTime
    , getLocalTime :: monad ZonedTime
    }

new :: Handle IO
new =
    Handle
        { getUtcTime = getCurrentTime
        , getLocalTime = getZonedTime
        }

utcToText :: UTCTime -> Text
utcToText utcTime =
    Data.Text.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" utcTime
