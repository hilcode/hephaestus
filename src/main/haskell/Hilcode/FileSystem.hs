module Hilcode.FileSystem (
    Handle (..),
    AbsDir,
    AbsFile,
    RelDir,
    RelFile,
    (//),
    new,
) where

import System.Directory.OsPath qualified
import System.OsPath (
    OsPath,
    (</>),
 )

newtype Handle monad = Handle
    { getCurrentDir :: monad AbsDir
    }

new :: Handle IO
new =
    Handle
        { getCurrentDir = AbsDir <$> System.Directory.OsPath.getCurrentDirectory
        }

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
