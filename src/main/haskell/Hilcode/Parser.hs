module Hilcode.Parser where

import Data.List qualified
import Hilcode.Result (Result (..))
import System.OsPath (OsChar, OsString)
import System.OsPath qualified
import Prelude (
    Applicative (..),
    Functor (..),
    Maybe (..),
    Monad (..),
    ($),
    (.),
    (==),
 )

type Source = [OsChar]

type ParseResult e a =
    Result e (Maybe (Source, a))

newtype Parser e a
    = Parser {parse :: Source -> ParseResult e a}

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

                    map :: (a -> b) -> ParseResult e a -> ParseResult e b
                    map = fmap . fmap . fmap
                 in
                    f `map` resultA
         in
            Parser{parse}

instance Applicative (Parser e) where
    pure :: forall a. a -> Parser e a
    pure a =
        let
            parse :: Source -> ParseResult e a
            parse source = Ok $ Just (source, a)
         in
            Parser{parse}

    liftA2 :: forall a b c. (a -> b -> c) -> Parser e a -> Parser e b -> Parser e c
    liftA2 f (Parser parseA) (Parser parseB) =
        let
            parse :: Source -> ParseResult e c
            parse source =
                case parseA source of
                    Ok (Just (sourceAfterA, a)) ->
                        case parseB sourceAfterA of
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
            Parser{parse}

instance Monad (Parser e) where
    (>>=) :: forall a b. Parser e a -> (a -> Parser e b) -> Parser e b
    (>>=) (Parser parseA) f =
        let
            parse :: Source -> ParseResult e b
            parse source =
                case parseA source of
                    Ok (Just (sourceAfterA, a)) ->
                        case f a of
                            Parser parseB ->
                                parseB sourceAfterA
                    Ok Nothing ->
                        Ok Nothing
                    Err parseError ->
                        Err parseError
         in
            Parser{parse}

fail :: forall e a. e -> Parser e a
fail parseError =
    let
        parse :: Source -> ParseResult e a
        parse _ =
            Err parseError
     in
        Parser{parse}

endOfInput :: forall e. Parser e ()
endOfInput =
    let
        parse :: Source -> ParseResult e ()
        parse source =
            case source of
                [] ->
                    Ok (Just (source, ()))
                _ ->
                    Ok Nothing
     in
        Parser{parse}

osString :: forall e. OsString -> Parser e OsString
osString expected =
    let
        osString :: Source
        osString = System.OsPath.unpack expected

        parse :: Source -> ParseResult e OsString
        parse source =
            case Data.List.stripPrefix osString source of
                Nothing ->
                    Ok Nothing
                Just rest ->
                    Ok $ Just (rest, expected)
     in
        Parser{parse}

osChar :: forall e. OsChar -> Parser e OsChar
osChar expected =
    let
        parse :: Source -> ParseResult e OsChar
        parse source =
            case source of
                firstOsChar : rest
                    | firstOsChar == expected ->
                        Ok $ Just (rest, firstOsChar)
                _ ->
                    Ok Nothing
     in
        Parser{parse}

anyOsChar :: forall e. Parser e OsChar
anyOsChar =
    let
        parse :: Source -> ParseResult e OsChar
        parse source =
            case source of
                firstOsChar : rest ->
                    Ok $ Just (rest, firstOsChar)
                _ ->
                    Ok Nothing
     in
        Parser{parse}

firstOf :: forall e a. Parser e a -> Parser e a -> Parser e a
firstOf (Parser lhs) (Parser rhs) =
    let
        parse :: Source -> ParseResult e a
        parse source =
            case lhs source of
                Ok Nothing ->
                    rhs source
                Ok result ->
                    Ok result
                Err parseError ->
                    Err parseError
     in
        Parser{parse}

repeat :: forall e a. Parser e a -> Parser e [a]
repeat (Parser parseA) =
    let
        go :: [a] -> Source -> ParseResult e [a]
        go accumulator source =
            case parseA source of
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
        parse = go []
     in
        Parser{parse}

repeat1 :: forall e a. Parser e a -> Parser e [a]
repeat1 parser =
    let
        parse :: Source -> ParseResult e [a]
        parse source =
            case repeat parser of
                Parser parse ->
                    case parse source of
                        Err parseError ->
                            Err parseError
                        Ok (Just (_, [])) ->
                            Ok Nothing
                        Ok tuple ->
                            Ok tuple
     in
        Parser{parse}

optionally :: forall e a. Parser e a -> Parser e (Maybe a)
optionally (Parser parseA) =
    let
        parse :: Source -> ParseResult e (Maybe a)
        parse source =
            case parseA source of
                Ok Nothing ->
                    Ok $ Just (source, Nothing)
                Ok (Just (rest, a)) ->
                    Ok (Just (rest, Just a))
                Err parseError ->
                    Err parseError
     in
        Parser{parse}
