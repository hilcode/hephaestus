module Hilcode.Glob (
    mkGlob,
    mkGlob2,
    GlobError (..),
) where

import Data.List qualified
import Data.Set (Set)
import Data.Set qualified
import Data.Vector (Vector)
import Data.Vector qualified
import Hilcode.FileSystem (RelDir (..), RelFile (..), (//))
import Hilcode.FileSystem qualified as FileSystem
import Hilcode.Parser (Parser)
import Hilcode.Parser qualified
import Hilcode.Result (Result (..))
import Hilcode.Result qualified
import System.IO qualified
import System.OsPath (OsChar, OsPath, OsString)
import System.OsPath qualified
import System.OsString qualified
import Prelude (
    Bool (..),
    Either (..),
    Eq (..),
    Maybe (..),
    Monad (..),
    Monoid (..),
    Ord,
    Show,
    String,
    const,
    fmap,
    mapM,
    mconcat,
    not,
    pure,
    span,
    undefined,
    ($),
    (<$),
    (<$>),
    (<*),
    (<*>),
    (<>),
    (||),
 )

data GlobPart
    = AnyChars
    | AnySingleChar
    | OsStringMatch OsString
    deriving stock (Eq, Ord, Show)

data GlobPartFiber
    = GlobPartFiber OsString [GlobPart]
    deriving stock (Eq, Ord)

matchGlobPartFiber :: GlobPartFiber -> Either (Set GlobPartFiber) Bool
matchGlobPartFiber (GlobPartFiber name []) =
    Right $ System.OsString.null name
matchGlobPartFiber (GlobPartFiber name (OsStringMatch prefix : globParts)) =
    case System.OsString.stripPrefix prefix name of
        Nothing ->
            Right False
        Just tailOsChars ->
            Left $ Data.Set.singleton (GlobPartFiber tailOsChars globParts)
matchGlobPartFiber (GlobPartFiber name (AnySingleChar : globParts)) =
    case System.OsString.uncons name of
        Nothing ->
            Right False
        Just (_, tailOsChars) ->
            Left $ Data.Set.singleton (GlobPartFiber tailOsChars globParts)
matchGlobPartFiber (GlobPartFiber name allGlobParts@(AnyChars : globParts)) =
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

matchFileGlob :: OsString -> FileGlob -> Bool
matchFileGlob _ AnySingleFile =
    True
matchFileGlob name (FileMatch globParts) =
    let
        globPartFiber :: GlobPartFiber
        globPartFiber = GlobPartFiber name globParts
     in
        go $ Data.Set.singleton globPartFiber
  where
    go :: Set GlobPartFiber -> Bool
    go fibers =
        case simplify Data.Set.empty (matchGlobPartFiber `fmap` Data.Set.elems fibers) of
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
    = GlobFiber RelDir Glob
    deriving stock (Eq, Ord)

matchGlobFiber :: forall monad. (Monad monad) => FileSystem.Handle monad -> GlobFiber -> monad (Set GlobFiber, Vector RelFile)
matchGlobFiber fileSystem (GlobFiber relDir glob) = do
    (_directories, files) :: (Vector RelDir, Vector RelFile) <- fileSystem.getFiles relDir
    case glob of
        Glob [] fileGlob ->
            let
                toOsString :: RelFile -> OsString
                toOsString relFile =
                    case relFile of
                        RelFile osString ->
                            osString

                osStringMatches :: Vector OsString
                osStringMatches = (`matchFileGlob` fileGlob) `Data.Vector.filter` (toOsString <$> files)

                toRelFile :: OsString -> RelFile
                toRelFile osString = relDir // RelFile osString

                fileMatches :: Vector RelFile
                fileMatches = toRelFile <$> osStringMatches
             in
                pure (Data.Set.empty, fileMatches)
        Glob (_dirGlob : _dirGlobs) _fileGlob ->
            undefined

_match :: forall monad. (Monad monad) => FileSystem.Handle monad -> RelDir -> Glob -> monad (Vector RelFile)
_match fileSystem directory glob =
    let
        fiber :: GlobFiber
        fiber = GlobFiber directory glob

        go :: (Set GlobFiber, Vector RelFile) -> monad (Vector RelFile)
        go (fibers, foundSoFar) =
            if Data.Set.null fibers
                then pure foundSoFar
                else
                    let
                        globFibers :: [GlobFiber]
                        globFibers = Data.Set.elems fibers

                        newState :: monad [(Set GlobFiber, Vector RelFile)]
                        newState = mapM (matchGlobFiber fileSystem) globFibers
                     in
                        do
                            state :: (Set GlobFiber, Vector RelFile) <- mconcat `fmap` newState
                            go state
     in
        go (Data.Set.singleton fiber, Data.Vector.empty)

mkGlob2 :: String -> Result GlobError Glob
mkGlob2 originalText =
    let
        text :: OsString
        text = System.OsPath.unsafeEncodeUtf originalText

        star :: OsChar
        star = System.OsPath.unsafeFromChar '*'

        questionMark :: OsChar
        questionMark = System.OsPath.unsafeFromChar '?'

        slash :: OsChar
        slash = System.OsPath.unsafeFromChar '/'

        starStar :: OsString
        starStar = System.OsPath.pack [star, star]

        parseGlobPart :: Parser GlobError GlobPart
        parseGlobPart =
            let
                parseAnyChars :: Parser GlobError GlobPart
                parseAnyChars = AnyChars <$ Hilcode.Parser.osChar star

                parseAnySingleChar :: Parser GlobError GlobPart
                parseAnySingleChar = AnySingleChar <$ Hilcode.Parser.osChar questionMark

                parseOsString :: Parser GlobError OsString
                parseOsString = System.OsPath.pack <$> Hilcode.Parser.repeat Hilcode.Parser.anyOsChar

                parseFileMatch :: Parser GlobError GlobPart
                parseFileMatch = OsStringMatch <$> parseOsString
             in
                Hilcode.Parser.firstOf parseAnyChars $ Hilcode.Parser.firstOf parseAnySingleChar parseFileMatch

        parseDirGlob :: Parser GlobError DirGlob
        parseDirGlob =
            let
                parseAnyDirs :: Parser GlobError DirGlob
                parseAnyDirs = AnyDirs <$ (Hilcode.Parser.osString starStar >> Hilcode.Parser.osChar slash)

                parseAnySingleDir :: Parser GlobError DirGlob
                parseAnySingleDir = AnySingleDir <$ (Hilcode.Parser.osChar star >> Hilcode.Parser.osChar slash)

                parseDirMatch :: Parser GlobError DirGlob
                parseDirMatch = DirMatch <$> (Hilcode.Parser.repeat parseGlobPart <* Hilcode.Parser.osChar slash)
             in
                Hilcode.Parser.firstOf parseAnyDirs $ Hilcode.Parser.firstOf parseAnySingleDir parseDirMatch

        parseFileGlob :: Parser GlobError FileGlob
        parseFileGlob =
            let
                parseAnySingleFile :: Parser e FileGlob
                parseAnySingleFile = AnySingleFile <$ (Hilcode.Parser.osChar star >> Hilcode.Parser.endOfInput)

                parseFileMatch :: Parser GlobError FileGlob
                parseFileMatch = FileMatch <$> Hilcode.Parser.repeat parseGlobPart
             in
                Hilcode.Parser.firstOf parseAnySingleFile parseFileMatch

        parseGlob :: Parser GlobError Glob
        parseGlob = do
            dirGlobs <- Hilcode.Parser.repeat parseDirGlob
            Glob dirGlobs <$> parseFileGlob
     in
        case Hilcode.Parser.runParser parseGlob text of
            Ok (Just (rest, glob))
                | Data.List.null rest ->
                    Ok glob
            Err parseError ->
                Err parseError
            Ok Nothing ->
                Err NoMatch
            Ok (Just (_, _)) ->
                Err Incomplete

mkGlob :: String -> Result GlobError Glob
mkGlob text =
    let
        osPath :: Result GlobError OsPath
        osPath =
            Hilcode.Result.mapError
                (const EncodingProblem)
                (Hilcode.Result.fromEither $ System.OsPath.encodeWith System.IO.utf8 System.IO.utf16 text)

        glob :: Result GlobError Glob
        glob = osPath >>= fromOsString
     in
        fixGlob `fmap` glob

data GlobError
    = AbsoluteGlob
    | EmptyGlob
    | InvalidGlob
    | EncodingProblem
    | Incomplete
    | NoMatch
    deriving stock (Show)

fromOsString :: OsString -> Result GlobError Glob
fromOsString osString =
    if System.OsPath.isAbsolute osString
        then
            Err AbsoluteGlob
        else case Data.List.unsnoc $ System.OsPath.splitPath osString of
            Nothing ->
                Err EmptyGlob
            Just (dirs, file) ->
                let
                    fixedDirs :: [OsPath]
                    fixedDirs = System.OsPath.dropTrailingPathSeparator `fmap` dirs

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
                osStringMatch@(OsStringMatch _) : more ->
                    osStringMatch : fixGlobParts more

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
sStarStar = System.OsPath.unsafeEncodeUtf "**"

sStar :: OsString
sStar = System.OsPath.unsafeEncodeUtf "*"

cStar :: OsChar
cStar = System.OsPath.unsafeFromChar '*'

cQuestionMark :: OsChar
cQuestionMark = System.OsPath.unsafeFromChar '?'

mkDirGlob :: OsString -> Result GlobError DirGlob
mkDirGlob dir =
    case dir of
        starStar
            | starStar == sStarStar ->
                Ok AnyDirs
        star
            | star == sStar ->
                Ok AnySingleDir
        globParts ->
            DirMatch `fmap` mkGlobParts globParts

mkFileGlob :: OsString -> Result GlobError FileGlob
mkFileGlob file =
    case file of
        star
            | star == sStar ->
                Ok AnySingleFile
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
                            OsStringMatch (System.OsPath.pack lhs) : toGlobParts rhs

        globParts :: [GlobPart]
        globParts = toGlobParts $ System.OsPath.unpack text
     in
        Ok globParts
