module Hilcode.Clock (
    Handle (..),
    new,
    toText,
) where

import Data.Text (Text)
import Data.Text qualified
import Data.Time (
    NominalDiffTime,
    diffTimeToPicoseconds,
    diffUTCTime,
    nominalDiffTimeToSeconds,
 )
import Data.Time.Clock (getCurrentTime)
import TextShow qualified

newtype Handle monad = Handle
    { getElapsedTime :: monad NominalDiffTime
    }

new :: IO (Handle IO)
new = do
    start <- getCurrentTime
    let getElapsedTime :: IO NominalDiffTime = (`diffUTCTime` start) `fmap` getCurrentTime
    pure $
        Handle
            { getElapsedTime
            }

picosPerMilli :: Integer
picosPerMilli = 1_000_000_000

millisPerSecond :: Integer
millisPerSecond = 1_000

secondsPerMinute :: Integer
secondsPerMinute = 60

minutesPerHour :: Integer
minutesPerHour = 60

millisPerMinute :: Integer
millisPerMinute = millisPerSecond * secondsPerMinute

millisPerHour :: Integer
millisPerHour = millisPerMinute * minutesPerHour

toText :: NominalDiffTime -> Text
toText delta =
    let
        elapsedTimeInPicos :: Integer
        elapsedTimeInPicos = diffTimeToPicoseconds $ realToFrac $ nominalDiffTimeToSeconds delta

        elapsedTimeInMillis :: Integer
        elapsedTimeInMillis = (elapsedTimeInPicos + picosPerMilli `div` 2) `div` picosPerMilli

        hoursElapsed :: (Integer, Integer)
        hoursElapsed =
            let
                hours :: Integer
                hours = elapsedTimeInMillis `div` millisPerHour

                millisLeft :: Integer
                millisLeft = elapsedTimeInMillis - millisPerHour * hours
             in
                (hours, millisLeft)

        hours :: TextShow.Builder
        hours =
            let
                hours :: Integer
                hours = fst hoursElapsed

                hoursAsText :: TextShow.Builder
                hoursAsText = TextShow.fromText $ Data.Text.show hours
             in
                if hours < 10
                    then TextShow.showbSpace <> TextShow.showbSpace <> hoursAsText
                    else
                        if hours < 100
                            then TextShow.showbSpace <> hoursAsText
                            else hoursAsText

        minutesElapsed :: (Integer, Integer)
        minutesElapsed =
            let
                minutes :: Integer
                minutes = snd hoursElapsed `div` millisPerMinute

                millisLeft :: Integer
                millisLeft = snd hoursElapsed - millisPerMinute * minutes
             in
                (minutes, millisLeft)

        minutes :: TextShow.Builder
        minutes =
            let
                minutes :: Integer
                minutes = fst minutesElapsed

                minutesAsText :: TextShow.Builder
                minutesAsText = TextShow.fromText $ Data.Text.show minutes
             in
                if minutes < 10
                    then TextShow.singleton '0' <> minutesAsText
                    else minutesAsText

        secondsElapsed :: (Integer, Integer)
        secondsElapsed =
            let
                seconds :: Integer
                seconds = snd minutesElapsed `div` millisPerSecond

                millisLeft :: Integer
                millisLeft = snd minutesElapsed - millisPerSecond * seconds
             in
                (seconds, millisLeft)

        seconds :: TextShow.Builder
        seconds =
            let
                seconds :: Integer
                seconds = fst secondsElapsed

                secondsAsText :: TextShow.Builder
                secondsAsText = TextShow.fromText $ Data.Text.show seconds
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
                millisAsText = TextShow.fromText $ Data.Text.show millis
             in
                if millis < 10
                    then TextShow.singleton '0' <> TextShow.singleton '0' <> millisAsText
                    else
                        if millis < 100
                            then TextShow.singleton '0' <> millisAsText
                            else millisAsText
     in
        TextShow.toText $ hours <> TextShow.singleton 'h' <> minutes <> TextShow.singleton 'm' <> seconds <> TextShow.singleton '.' <> millis
