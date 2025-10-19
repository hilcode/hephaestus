module Hilcode.Parser where

import Data.List qualified
import Hilcode.Prelude
import Hilcode.Result (Result (..))
import System.OsPath (OsChar, OsString)
import System.OsPath qualified
import Prelude (String)

type Source = [OsChar]

{- HLINT ignore "Use newtype instead of data" #-}
data ParseResult e a
    = ParseResult (Result e (Maybe (Source, a)))
    deriving stock (Show)

instance (Eq e, Eq a) => Eq (ParseResult e a) where
    (==) :: ParseResult e a -> ParseResult e a -> Bool
    ParseResult lhs == ParseResult rhs = lhs == rhs

instance (Ord e, Ord a) => Ord (ParseResult e a) where
    (<=) :: ParseResult e a -> ParseResult e a -> Bool
    ParseResult lhs <= ParseResult rhs = lhs <= rhs

instance Functor (ParseResult e) where
    fmap :: (a -> b) -> ParseResult e a -> ParseResult e b
    fmap f (ParseResult result) =
        ParseResult $ (fmap . fmap . fmap) f result

toResult :: ParseResult e a -> Result e (Maybe (Source, a))
toResult (ParseResult result) = result

fromResult :: Result e (Maybe (Source, a)) -> ParseResult e a
fromResult = ParseResult

newtype Parser e a
    = Parser (Source -> ParseResult e a)

instance Show (Parser e a) where
    show :: Parser e a -> String
    show _ = "Parser"

runParser :: forall e a. Parser e a -> OsString -> ParseResult e a
runParser (Parser parse) text =
    parse $ System.OsPath.unpack text

instance Functor (Parser e) where
    fmap :: forall a b. (a -> b) -> Parser e a -> Parser e b
    fmap f (Parser parseA) =
        let
            parse :: Source -> ParseResult e b
            parse source =
                let
                    resultA :: ParseResult e a
                    resultA = parseA source
                 in
                    f `fmap` resultA
         in
            Parser parse

instance Applicative (Parser e) where
    pure :: forall a. a -> Parser e a
    pure a =
        let
            parse :: Source -> ParseResult e a
            parse source = ParseResult (Ok $ Just (source, a))
         in
            Parser parse

    liftA2 :: forall a b c. (a -> b -> c) -> Parser e a -> Parser e b -> Parser e c
    liftA2 f (Parser parseA) (Parser parseB) =
        let
            parse :: Source -> ParseResult e c
            parse source =
                let
                    resultA :: Result e (Maybe (Source, a))
                    resultA = toResult $ parseA source

                    result :: Result e (Maybe (Source, c))
                    result =
                        case resultA of
                            Ok (Just (sourceAfterA, a)) ->
                                let
                                    resultB :: Result e (Maybe (Source, b))
                                    resultB = toResult $ parseB sourceAfterA
                                 in
                                    case resultB of
                                        Ok (Just (sourceAfterB, b)) ->
                                            Ok (Just (sourceAfterB, f a b))
                                        Ok Nothing ->
                                            Ok Nothing
                                        Err parseError ->
                                            Err parseError
                            Ok Nothing ->
                                Ok Nothing
                            Err parseError ->
                                Err parseError
                 in
                    fromResult result
         in
            Parser parse

instance Monad (Parser e) where
    (>>=) :: forall a b. Parser e a -> (a -> Parser e b) -> Parser e b
    (>>=) (Parser parseA) f =
        let
            parse :: Source -> ParseResult e b
            parse source =
                let
                    resultA = toResult $ parseA source
                 in
                    case resultA of
                        Ok (Just (sourceAfterA, a)) ->
                            case f a of
                                Parser parseB ->
                                    parseB sourceAfterA
                        Ok Nothing ->
                            fromResult $ Ok Nothing
                        Err parseError ->
                            fromResult $ Err parseError
         in
            Parser parse

fail :: forall e a. e -> Parser e a
fail parseError =
    let
        parse :: Source -> ParseResult e a
        parse _ =
            fromResult $ Err parseError
     in
        Parser parse

endOfInput :: forall e. Parser e ()
endOfInput =
    let
        parse :: Source -> ParseResult e ()
        parse source =
            fromResult
                $ case source of
                    [] ->
                        Ok (Just (source, ()))
                    _ ->
                        Ok Nothing
     in
        Parser parse

osString :: forall e. OsString -> Parser e OsString
osString expected =
    let
        osChars :: Source
        osChars = System.OsPath.unpack expected

        parse :: Source -> ParseResult e OsString
        parse source =
            fromResult
                $ case Data.List.stripPrefix osChars source of
                    Nothing ->
                        Ok Nothing
                    Just rest ->
                        Ok $ Just (rest, expected)
     in
        Parser parse

osChar :: forall e. OsChar -> Parser e OsChar
osChar expected =
    let
        parse :: Source -> ParseResult e OsChar
        parse source =
            fromResult
                $ case source of
                    firstOsChar : rest
                        | firstOsChar == expected ->
                            Ok $ Just (rest, firstOsChar)
                    _ ->
                        Ok Nothing
     in
        Parser parse

anyOsChar :: forall e. Parser e OsChar
anyOsChar =
    let
        parse :: Source -> ParseResult e OsChar
        parse source =
            fromResult
                $ case source of
                    firstOsChar : rest ->
                        Ok $ Just (rest, firstOsChar)
                    _ ->
                        Ok Nothing
     in
        Parser parse

firstOf :: forall e a. Parser e a -> Parser e a -> Parser e a
firstOf (Parser lhs) (Parser rhs) =
    let
        parse :: Source -> ParseResult e a
        parse source =
            case toResult $ lhs source of
                Ok Nothing ->
                    rhs source
                Ok result ->
                    fromResult $ Ok result
                Err parseError ->
                    fromResult $ Err parseError
     in
        Parser parse

repeat :: forall e a. Parser e a -> Parser e [a]
repeat (Parser parseA) =
    let
        go :: [a] -> Source -> Result e (Maybe (Source, [a]))
        go accumulator source =
            case toResult $ parseA source of
                Ok Nothing ->
                    let
                        results :: [a]
                        results = Data.List.reverse accumulator
                     in
                        Ok $ Just (source, results)
                Ok (Just (nextSource, a)) ->
                    go (a : accumulator) nextSource
                Err parseError ->
                    Err parseError

        parse :: Source -> ParseResult e [a]
        parse source = fromResult $ go [] source
     in
        Parser parse

repeat1 :: forall e a. Parser e a -> Parser e [a]
repeat1 parser =
    let
        parse :: Source -> ParseResult e [a]
        parse source =
            case repeat parser of
                Parser parse ->
                    fromResult
                        $ case toResult $ parse source of
                            Err parseError ->
                                Err parseError
                            Ok (Just (_, [])) ->
                                Ok Nothing
                            Ok tuple ->
                                Ok tuple
     in
        Parser parse

optionally :: forall e a. Parser e a -> Parser e (Maybe a)
optionally (Parser parseA) =
    let
        parse :: Source -> ParseResult e (Maybe a)
        parse source =
            fromResult
                $ case toResult $ parseA source of
                    Ok Nothing ->
                        Ok $ Just (source, Nothing)
                    Ok (Just (rest, a)) ->
                        Ok (Just (rest, Just a))
                    Err parseError ->
                        Err parseError
     in
        Parser parse
