module Hilcode.Glob (
    mkGlob,
) where

import Data.List (unsnoc)
import Hilcode.Result (
    Result,
    err,
    fromEither,
    mapError,
    ok,
 )
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

data GlobPart
    = AnyChars
    | AnySingleChar
    | Verbatim OsString
    deriving stock (Show)

data DirGlob
    = AnyDirs
    | AnySingleDir
    | DirMatch [GlobPart]
    deriving stock (Show)

data FileGlob
    = AnySingleFile
    | FileMatch [GlobPart]
    deriving stock (Show)

data Glob
    = Glob [DirGlob] FileGlob
    deriving stock (Show)

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
