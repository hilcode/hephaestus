module Hilcode.FileSystem (
    Handle (..),
    AbsDir (..),
    AbsFile (..),
    RelDir (..),
    RelFile (..),
    (//),
    new,
) where

import Control.Monad qualified
import Data.Vector (Vector)
import Data.Vector qualified
import System.Directory.OsPath qualified
import System.OsPath (
    OsPath,
    (</>),
 )
import Prelude (
    Bool,
    Eq,
    IO,
    Ord,
    Show,
    fst,
    print,
    pure,
    snd,
    ($),
    (<$>),
 )

data Handle monad = Handle
    { getCurrentDir :: monad AbsDir
    , setCurrentDir :: AbsDir -> monad ()
    , getFiles :: RelDir -> monad (Vector RelDir, Vector RelFile)
    }

new :: Handle IO
new =
    let
        getCurrentDir :: IO AbsDir
        getCurrentDir = AbsDir <$> System.Directory.OsPath.getCurrentDirectory

        setCurrentDir :: AbsDir -> IO ()
        setCurrentDir (AbsDir absDir) =
            System.Directory.OsPath.setCurrentDirectory absDir

        getFiles :: RelDir -> IO (Vector RelDir, Vector RelFile)
        getFiles relDir@(RelDir osPath) = do
            AbsDir currentDir <- getCurrentDir
            let directory = currentDir </> osPath
            entries :: [OsPath] <- System.Directory.OsPath.listDirectory directory
            print entries
            x2 :: ([RelDir], [RelFile]) <- split relDir entries
            print x2
            let x3 :: [RelDir] = fst x2
            let x4 :: [RelFile] = snd x2
            let x5 :: Vector RelDir = Data.Vector.fromList x3
            let x6 :: Vector RelFile = Data.Vector.fromList x4
            let x7 = (x5, x6)
            pure x7
     in
        Handle
            { getCurrentDir
            , setCurrentDir
            , getFiles
            }

data OsPathType
    = Directory
    | File
    | Other

ifM :: IO Bool -> IO a -> IO a -> IO a
ifM predicateM ifTrue ifFalse = do
    predicate <- predicateM
    if predicate then ifTrue else ifFalse

toOsPathType :: OsPath -> IO OsPathType
toOsPathType osPath = do
    ifM
        (System.Directory.OsPath.doesDirectoryExist osPath)
        (pure Directory)
        ( ifM
            (System.Directory.OsPath.doesFileExist osPath)
            (pure File)
            (pure Other)
        )

split :: RelDir -> [OsPath] -> IO ([RelDir], [RelFile])
split relDir@(RelDir relDirOsPath) =
    Control.Monad.foldM step ([], [])
  where
    step :: ([RelDir], [RelFile]) -> OsPath -> IO ([RelDir], [RelFile])
    step (directories, files) osPath = do
        osPathType <- toOsPathType $ relDirOsPath </> osPath
        case osPathType of
            Directory ->
                pure ((relDir // RelDir osPath) : directories, files)
            File ->
                pure (directories, (relDir // RelFile osPath) : files)
            Other ->
                pure (directories, files)

class HasSlash base extension result where
    (//) :: base -> extension -> result

newtype AbsDir
    = AbsDir OsPath
    deriving newtype (Eq, Ord, Show)

newtype RelDir
    = RelDir OsPath
    deriving newtype (Eq, Ord, Show)

newtype RelFile
    = RelFile OsPath
    deriving newtype (Eq, Ord, Show)

newtype AbsFile
    = AbsFile OsPath
    deriving newtype (Eq, Ord, Show)

instance HasSlash AbsDir RelDir AbsDir where
    (//) :: AbsDir -> RelDir -> AbsDir
    AbsDir lhs // RelDir rhs = AbsDir (lhs </> rhs)

instance HasSlash AbsDir RelFile AbsFile where
    (//) :: AbsDir -> RelFile -> AbsFile
    AbsDir lhs // RelFile rhs = AbsFile (lhs </> rhs)

instance HasSlash RelDir RelDir RelDir where
    (//) :: RelDir -> RelDir -> RelDir
    RelDir lhs // RelDir rhs = RelDir (lhs </> rhs)

instance HasSlash RelDir RelFile RelFile where
    (//) :: RelDir -> RelFile -> RelFile
    RelDir lhs // RelFile rhs = RelFile (lhs </> rhs)
