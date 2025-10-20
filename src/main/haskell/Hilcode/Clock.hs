module Hilcode.Clock (
    Handle (..),
    new,
    toText,
) where

import Data.Text (Text, show)
import Data.Time (
    NominalDiffTime,
    diffTimeToPicoseconds,
    diffUTCTime,
    nominalDiffTimeToSeconds,
 )
import Data.Time.Clock (getCurrentTime)
import TextShow qualified
import Prelude hiding (show)

newtype Handle monad
    = Handle
    { getElapsedTime :: monad NominalDiffTime
    }

new :: IO (Handle IO)
new = do
    start <- getCurrentTime
    pure $ Handle
        { getElapsedTime = (`diffUTCTime` start) `fmap` getCurrentTime
        }

toText :: NominalDiffTime -> Text
toText delta =
    let
        elapsedTimeInPicos :: Integer
        elapsedTimeInPicos = diffTimeToPicoseconds $ realToFrac $ nominalDiffTimeToSeconds delta

        elapsedTimeInMillis :: Integer
        elapsedTimeInMillis = (elapsedTimeInPicos + 500000) `div` 1000000

        hoursElapsed :: (Integer, Integer)
        hoursElapsed = elapsedTimeInMillis `divMod` (1000 * 60 * 60)

        hours :: TextShow.Builder
        hours =
            let
                hours :: Integer
                hours = fst hoursElapsed

                hoursAsText :: TextShow.Builder
                hoursAsText = TextShow.fromText $ show hours
             in
                if hours < 10
                    then TextShow.showbSpace <> TextShow.showbSpace <> hoursAsText
                    else
                        if hours < 100
                            then TextShow.showbSpace <> hoursAsText
                            else hoursAsText

        minutesElapsed :: (Integer, Integer)
        minutesElapsed = snd hoursElapsed `divMod` (1000 * 60)

        minutes :: TextShow.Builder
        minutes =
            let
                minutes :: Integer
                minutes = fst minutesElapsed

                minutesAsText :: TextShow.Builder
                minutesAsText = TextShow.fromText $ show minutes
             in
                if minutes < 10
                    then TextShow.singleton '0' <> minutesAsText
                    else minutesAsText

        secondsElapsed :: (Integer, Integer)
        secondsElapsed = snd minutesElapsed `divMod` 1000

        seconds :: TextShow.Builder
        seconds =
            let
                seconds :: Integer
                seconds = fst secondsElapsed

                secondsAsText :: TextShow.Builder
                secondsAsText = TextShow.fromText $ show seconds
             in
                if seconds < 10
                    then TextShow.singleton '0' <> secondsAsText
                    else secondsAsText

        millis :: TextShow.Builder
        millis =
            let
                millis :: Integer
                millis = snd secondsElapsed

                millisAsText :: TextShow.Builder
                millisAsText = TextShow.fromText $ show millis
             in
                if millis < 10
                    then TextShow.singleton '0' <> TextShow.singleton '0' <> millisAsText
                    else
                        if millis < 100
                            then TextShow.singleton '0' <> millisAsText
                            else millisAsText
     in
        TextShow.toText $ hours <> TextShow.singleton 'h' <> minutes <> TextShow.singleton 'm' <> seconds <> TextShow.singleton '.' <> millis
