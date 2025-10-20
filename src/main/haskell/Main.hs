module Main where

import Data.Text qualified
import Hilcode.Clock qualified as Clock
import Hilcode.Glob (mkGlob)
import Hilcode.Logger (LogLevel (..))
import Hilcode.Logger qualified as Logger
import System.Directory.OsPath (canonicalizePath)
import System.OsPath (
    OsPath,
    encodeFS,
    (</>),
 )

main :: IO ()
main =
    do
        clock :: Clock.Handle IO <- Clock.new
        let logger :: Logger.Handle IO = Logger.new clock DEBUG
        currentDir :: OsPath <- encodeFS "."
        currentDir :: OsPath <- canonicalizePath currentDir
        logger.debug (Data.Text.show currentDir)
        logger.info "Hello World!"
        print $ mkGlob "**/*/**/**/**/*/*/?*?*?*.java"
        putStrLn "Okay"

class HasSlash base extension result where
    (/) :: base -> extension -> result

newtype AbsDir
    = AbsDir OsPath

newtype RelDir
    = RelDir OsPath

newtype RelFile
    = RelFile OsPath

newtype AbsFile
    = AbsFile OsPath

instance HasSlash AbsDir RelDir AbsDir where
    (/) :: AbsDir -> RelDir -> AbsDir
    AbsDir lhs / RelDir rhs = AbsDir (lhs </> rhs)

instance HasSlash AbsDir RelFile AbsFile where
    (/) :: AbsDir -> RelFile -> AbsFile
    AbsDir lhs / RelFile rhs = AbsFile (lhs </> rhs)

instance HasSlash RelDir RelDir RelDir where
    (/) :: RelDir -> RelDir -> RelDir
    RelDir lhs / RelDir rhs = RelDir (lhs </> rhs)

instance HasSlash RelDir RelFile RelFile where
    (/) :: RelDir -> RelFile -> RelFile
    RelDir lhs / RelFile rhs = RelFile (lhs </> rhs)
