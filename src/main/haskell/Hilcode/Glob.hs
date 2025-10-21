module Hilcode.Glob (
    mkGlob,
) where

import Control.Monad qualified
import Data.List (unsnoc)
import Data.Set (Set)
import Data.Set qualified
import Data.Vector (Vector)
import Data.Vector qualified
import Hilcode.Result (
    Result,
    err,
    fromEither,
    mapError,
    ok,
 )
import System.Directory.OsPath qualified
import System.IO (
    utf16,
    utf8,
 )
import System.OsPath (
    OsChar,
    OsPath,
    OsString,
    dropTrailingPathSeparator,
    encodeWith,
    isAbsolute,
    pack,
    splitPath,
    unpack,
    unsafeEncodeUtf,
    unsafeFromChar,
 )
import System.OsString qualified

data GlobPart
    = AnyChars
    | AnySingleChar
    | Verbatim OsString
    deriving stock (Eq, Ord, Show)

data GlobPartFiber
    = GlobPartFiber OsString [GlobPart]
    deriving stock (Eq, Ord)

_matchGlobPartFiber :: GlobPartFiber -> Either (Set GlobPartFiber) Bool
_matchGlobPartFiber (GlobPartFiber name []) =
    Right $ System.OsString.null name
_matchGlobPartFiber (GlobPartFiber name (Verbatim prefix : globParts)) =
    case System.OsString.stripPrefix prefix name of
        Nothing ->
            Right False
        Just tailOsChars ->
            Left $ Data.Set.singleton (GlobPartFiber tailOsChars globParts)
_matchGlobPartFiber (GlobPartFiber name (AnySingleChar : globParts)) =
    case System.OsString.uncons name of
        Nothing ->
            Right False
        Just (_, tailOsChars) ->
            Left $ Data.Set.singleton (GlobPartFiber tailOsChars globParts)
_matchGlobPartFiber (GlobPartFiber name allGlobParts@(AnyChars : globParts)) =
    case System.OsString.uncons name of
        Nothing ->
            Left $ Data.Set.singleton (GlobPartFiber name globParts)
        Just (_, tailOsChars) ->
            let
                skipAnyChars = GlobPartFiber name globParts
                matchAnyCharsOnce = GlobPartFiber tailOsChars globParts
                matchAnyCharsAgain = GlobPartFiber tailOsChars allGlobParts
             in
                Left $ Data.Set.fromList [skipAnyChars, matchAnyCharsOnce, matchAnyCharsAgain]

data DirGlob
    = AnyDirs
    | AnySingleDir
    | DirMatch [GlobPart]
    deriving stock (Eq, Ord, Show)

_matchSingle :: OsString -> DirGlob -> ()
_matchSingle _singleDirectory AnyDirs =
    undefined
_matchSingle _singleDirectory AnySingleDir =
    undefined
_matchSingle _singleDirectory (DirMatch _globParts) =
    undefined

data FileGlob
    = AnySingleFile
    | FileMatch [GlobPart]
    deriving stock (Eq, Ord, Show)

_matchFileGlob :: OsString -> FileGlob -> Bool
_matchFileGlob _ AnySingleFile =
    True
_matchFileGlob name (FileMatch globParts) =
    let
        globPartFiber :: GlobPartFiber
        globPartFiber = GlobPartFiber name globParts
     in
        go $ Data.Set.singleton globPartFiber
  where
    go :: Set GlobPartFiber -> Bool
    go fibers =
        case simplify Data.Set.empty (_matchGlobPartFiber `fmap` Data.Set.elems fibers) of
            Left fibers ->
                go fibers
            Right result ->
                result
      where
        simplify :: (Monoid a) => a -> [Either a b] -> Either a b
        simplify accumulator [] =
            Left accumulator
        simplify accumulator (ab : abs) =
            case ab of
                Left a ->
                    simplify (accumulator <> a) abs
                Right b ->
                    Right b

data Glob
    = Glob [DirGlob] FileGlob
    deriving stock (Eq, Ord, Show)

data GlobFiber
    = GlobFiber OsPath Glob
    deriving stock (Eq, Ord)

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

split :: [OsPath] -> IO ([OsPath], [OsPath])
split =
    Control.Monad.foldM step ([], [])
  where
    step :: ([OsPath], [OsPath]) -> OsPath -> IO ([OsPath], [OsPath])
    step (directories, files) osPath = do
        osPathType <- toOsPathType osPath
        case osPathType of
            Directory ->
                pure (osPath : directories, files)
            File ->
                pure (directories, osPath : files)
            Other ->
                pure (directories, files)

_matchGlobFiber :: GlobFiber -> IO (Set GlobFiber, Vector OsPath)
_matchGlobFiber (GlobFiber osPath glob) = do
    entries <- System.Directory.OsPath.listDirectory osPath
    (_directories, files) <- split entries
    case glob of
        Glob [] fileGlob ->
            let _x1 = (`_matchFileGlob` fileGlob) `filter` files
             in undefined
        Glob (_dirGlob : _dirGlobs) _fileGlob ->
            undefined

_match :: OsPath -> Glob -> IO (Vector OsPath)
_match directory glob =
    let
        fiber :: GlobFiber
        fiber = GlobFiber directory glob

        go :: (Set GlobFiber, Vector OsPath) -> IO (Vector OsPath)
        go (fibers, foundSoFar) =
            if Data.Set.null fibers
                then pure foundSoFar
                else
                    let
                        globFibers :: [GlobFiber]
                        globFibers = Data.Set.elems fibers

                        newState :: IO [(Set GlobFiber, Vector OsPath)]
                        newState = mapM _matchGlobFiber globFibers
                     in
                        do
                            state :: (Set GlobFiber, Vector OsPath) <- mconcat `fmap` newState
                            go state
     in
        go (Data.Set.singleton fiber, Data.Vector.empty)

mkGlob :: String -> Result GlobError Glob
mkGlob text =
    let
        osPath :: Result GlobError OsPath
        osPath = mapError (const EncodingProblem) (fromEither $ encodeWith utf8 utf16 text)

        glob :: Result GlobError Glob
        glob = osPath >>= fromOsString
     in
        fixGlob `fmap` glob

data GlobError
    = AbsoluteGlob
    | EmptyGlob
    | InvalidGlob
    | EncodingProblem
    deriving stock (Show)

fromOsString :: OsString -> Result GlobError Glob
fromOsString osString =
    if isAbsolute osString
        then
            err AbsoluteGlob
        else case unsnoc $ splitPath osString of
            Nothing ->
                err EmptyGlob
            Just (dirs, file) ->
                let
                    fixedDirs :: [OsPath]
                    fixedDirs = dropTrailingPathSeparator `fmap` dirs

                    dirGlobs :: Result GlobError [DirGlob]
                    dirGlobs = mapM mkDirGlob fixedDirs

                    fileGlob :: Result GlobError FileGlob
                    fileGlob = mkFileGlob file
                 in
                    Glob <$> dirGlobs <*> fileGlob

fixGlob :: Glob -> Glob
fixGlob (Glob dirGlobs fileGlob) =
    let
        fixGlobParts :: [GlobPart] -> [GlobPart]
        fixGlobParts globParts =
            case globParts of
                [] ->
                    []
                AnyChars : AnyChars : more ->
                    fixGlobParts (AnyChars : more)
                AnyChars : AnySingleChar : more ->
                    fixGlobParts (AnySingleChar : AnyChars : more)
                AnyChars : more ->
                    AnyChars : fixGlobParts more
                AnySingleChar : more ->
                    AnySingleChar : fixGlobParts more
                verbatim@(Verbatim _) : more ->
                    verbatim : fixGlobParts more

        fixDirGlobs :: [DirGlob] -> [DirGlob]
        fixDirGlobs dirGlobs =
            case dirGlobs of
                [] ->
                    []
                AnyDirs : AnyDirs : more ->
                    fixDirGlobs (AnyDirs : more)
                AnyDirs : AnySingleDir : more ->
                    fixDirGlobs (AnySingleDir : AnyDirs : more)
                AnyDirs : more ->
                    AnyDirs : fixDirGlobs more
                AnySingleDir : more ->
                    AnySingleDir : fixDirGlobs more
                DirMatch globParts : more ->
                    DirMatch (fixGlobParts globParts) : fixDirGlobs more

        fixFileGlob :: FileGlob -> FileGlob
        fixFileGlob fileGlob =
            case fileGlob of
                AnySingleFile ->
                    fileGlob
                FileMatch globParts ->
                    FileMatch (fixGlobParts globParts)
     in
        Glob (fixDirGlobs dirGlobs) (fixFileGlob fileGlob)

sStarStar :: OsString
sStarStar = unsafeEncodeUtf "**"

sStar :: OsString
sStar = unsafeEncodeUtf "*"

cStar :: OsChar
cStar = unsafeFromChar '*'

cQuestionMark :: OsChar
cQuestionMark = unsafeFromChar '?'

mkDirGlob :: OsString -> Result GlobError DirGlob
mkDirGlob dir =
    case dir of
        starStar
            | starStar == sStarStar ->
                ok AnyDirs
        star
            | star == sStar ->
                ok AnySingleDir
        globParts ->
            DirMatch `fmap` mkGlobParts globParts

mkFileGlob :: OsString -> Result GlobError FileGlob
mkFileGlob file =
    case file of
        star
            | star == sStar ->
                ok AnySingleFile
        globParts ->
            FileMatch `fmap` mkGlobParts globParts

mkGlobParts :: OsString -> Result GlobError [GlobPart]
mkGlobParts text =
    let
        isStar :: OsChar -> Bool
        isStar osChar = osChar == cStar

        isQuestionMark :: OsChar -> Bool
        isQuestionMark osChar = osChar == cQuestionMark

        notSpecial :: OsChar -> Bool
        notSpecial osChar = not (isStar osChar || isQuestionMark osChar)

        toGlobParts :: [OsChar] -> [GlobPart]
        toGlobParts osChars =
            case osChars of
                [] ->
                    []
                osChar : rest
                    | isStar osChar ->
                        AnyChars : toGlobParts rest
                osChar : rest
                    | isQuestionMark osChar ->
                        AnySingleChar : toGlobParts rest
                _ ->
                    case notSpecial `span` osChars of
                        (lhs, rhs) ->
                            Verbatim (pack lhs) : toGlobParts rhs

        globParts :: [GlobPart]
        globParts = toGlobParts $ unpack text
     in
        ok globParts
