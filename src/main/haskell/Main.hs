module Main where

import Hilcode.Glob (mkGlob)
import System.Directory.OsPath (canonicalizePath)
import System.OsPath (
    OsPath,
    encodeFS,
    (</>),
 )
import Prelude hiding (fail)

main :: IO ()
main = do
    currentDir <- encodeFS "."
    currentDir <- canonicalizePath currentDir
    print currentDir
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
