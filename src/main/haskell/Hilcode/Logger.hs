module Hilcode.Logger (
    Handle(..),
    LogLevel (..),
    new,
) where

import Data.Text (Text)
import Data.Text.IO qualified

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

new :: LogLevel -> Handle IO
new logLevel =
    let
        debug :: Text -> IO ()
        debug =
            if logLevel <= DEBUG
                then logDebug
                else logOff

        info :: Text -> IO ()
        info =
            if logLevel <= INFO
                then logInfo
                else logOff
     in
        Handle{debug, info}

logDebug :: Text -> IO ()
logDebug text = Data.Text.IO.putStrLn $ "DEBUG " <> text

logInfo :: Text -> IO ()
logInfo text = Data.Text.IO.putStrLn $ "INFO  " <> text

logOff :: Text -> IO ()
logOff _ = pure ()
