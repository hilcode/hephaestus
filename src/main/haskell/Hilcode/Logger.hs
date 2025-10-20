module Hilcode.Logger (
    Handle (..),
    LogLevel (..),
    new,
) where

import Data.Text (Text)
import Data.Text.IO qualified
import Hilcode.Clock qualified as Clock

data LogLevel
    = DEBUG
    | INFO
    | OFF
    deriving stock (Eq, Ord)

data Handle monad
    = Handle
    { debug :: Text -> monad ()
    , info :: Text -> monad ()
    }

new :: Clock.Handle IO -> LogLevel -> Handle IO
new clock logLevel =
    let
        debug :: Text -> IO ()
        debug =
            if logLevel <= DEBUG
                then logDebug clock
                else logOff

        info :: Text -> IO ()
        info =
            if logLevel <= INFO
                then logInfo clock
                else logOff
     in
        Handle{debug, info}

logDebug :: Clock.Handle IO -> Text -> IO ()
logDebug clock text = do
    elapsedTime <- clock.getElapsedTime
    Data.Text.IO.putStrLn $ "[" <> Clock.toText elapsedTime <> "] DEBUG " <> text

logInfo :: Clock.Handle IO -> Text -> IO ()
logInfo clock text = do
    elapsedTime <- clock.getElapsedTime
    Data.Text.IO.putStrLn $ "[" <> Clock.toText elapsedTime <> "] INFO  " <> text

logOff :: Text -> IO ()
logOff _ = pure ()
