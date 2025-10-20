module Hilcode.Clock (
    Handle (..),
    new,
    toText,
) where

import Data.Text (Text, pack)
import Data.Time (
    UTCTime,
    NominalDiffTime, diffUTCTime, nominalDiffTimeToSeconds,  diffTimeToPicoseconds
 )
import Data.Time.Clock (getCurrentTime)

newtype Handle monad
    = Handle
    { getElapsedTime :: monad NominalDiffTime
    }

new :: UTCTime -> Handle IO
new start =
    let
        elapsedTime :: IO NominalDiffTime
        elapsedTime =(`diffUTCTime` start) `fmap` getCurrentTime
    in
        Handle
            { getElapsedTime = elapsedTime
            }

toText :: NominalDiffTime -> Text
toText delta =
    let
        x2 :: Integer
        x2 = diffTimeToPicoseconds $ realToFrac $ nominalDiffTimeToSeconds delta
        x3 :: Integer
        x3 = (x2 + 500000) `div` 1000000
    in
        pack $ show x3
