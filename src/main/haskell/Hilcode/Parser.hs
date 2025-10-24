module Hilcode.Parser where

import Data.ByteString (ByteString)
import Data.ByteString qualified
import Data.ByteString.Char8 qualified
import Data.List qualified
import Data.Text (Text)
import Data.Text qualified
import Data.Tuple qualified
import Hilcode.Result (Result (..))
import System.OsPath (OsChar, OsString)
import System.OsPath qualified
import Prelude hiding (repeat)

type ParseResult e a =
    Result e (Maybe (ByteString, a))

newtype Parser e a
    = Parser {parse :: ByteString -> ParseResult e a}

runParser :: forall e a. Parser e a -> Text -> ParseResult e a
runParser (Parser parse) text =
    parse $ Data.ByteString.concat $ Data.ByteString.Char8.singleton <$> Data.Text.unpack text

instance Functor (Parser e) where
    fmap :: forall a b. (a -> b) -> Parser e a -> Parser e b
    fmap f (Parser parseA) =
        let
            parse :: ByteString -> ParseResult e b
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
            parse :: ByteString -> ParseResult e a
            parse source = Ok $ Just (source, a)
         in
            Parser{parse}

    liftA2 :: forall a b c. (a -> b -> c) -> Parser e a -> Parser e b -> Parser e c
    liftA2 f (Parser parseA) (Parser parseB) =
        let
            parse :: ByteString -> ParseResult e c
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
            parse :: ByteString -> ParseResult e b
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
        parse :: ByteString -> ParseResult e a
        parse _ =
            Err parseError
     in
        Parser{parse}

string :: forall e. Text -> Parser e Text
string text =
    let
        byteString :: ByteString
        byteString = Data.ByteString.concat $ Data.ByteString.Char8.singleton <$> Data.Text.unpack text

        parse :: ByteString -> ParseResult e Text
        parse source =
            case Data.ByteString.stripPrefix byteString source of
                Nothing ->
                    Ok Nothing
                Just rest ->
                    Ok $ Just (rest, text)
     in
        Parser{parse}

osString :: forall e. OsString -> Parser e OsString
osString text =
    let
        byteString :: ByteString
        byteString = Data.ByteString.concat (Data.ByteString.Char8.singleton . System.OsPath.toChar <$> System.OsPath.unpack text)

        parse :: ByteString -> ParseResult e OsString
        parse source =
            case Data.ByteString.stripPrefix byteString source of
                Nothing ->
                    Ok Nothing
                Just rest ->
                    Ok $ Just (rest, text)
     in
        Parser{parse}

char :: forall e. Char -> Parser e Char
char ch =
    let
        byteString :: ByteString
        byteString = Data.ByteString.Char8.singleton ch

        parse :: ByteString -> ParseResult e Char
        parse source =
            case Data.ByteString.stripPrefix byteString source of
                Nothing ->
                    Ok Nothing
                Just rest ->
                    Ok $ Just (rest, ch)
     in
        Parser{parse}

osChar :: forall e. OsChar -> Parser e OsChar
osChar ch =
    let
        byteString :: ByteString
        byteString = (Data.ByteString.Char8.singleton . System.OsPath.toChar) ch

        parse :: ByteString -> ParseResult e OsChar
        parse source =
            case Data.ByteString.stripPrefix byteString source of
                Nothing ->
                    Ok Nothing
                Just rest ->
                    Ok $ Just (rest, ch)
     in
        Parser{parse}

anyChar :: forall e. Parser e Char
anyChar =
    let
        parse :: ByteString -> ParseResult e Char
        parse source =
            Ok $ Data.Tuple.swap `fmap` Data.ByteString.Char8.uncons source
     in
        Parser{parse}

firstOf :: forall e a. Parser e a -> Parser e a -> Parser e a
firstOf (Parser lhs) (Parser rhs) =
    let
        parse :: ByteString -> ParseResult e a
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
        go :: [a] -> ByteString -> ParseResult e [a]
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

        parse :: ByteString -> ParseResult e [a]
        parse = go []
     in
        Parser{parse}

repeat1 :: forall e a. Parser e a -> Parser e [a]
repeat1 parser =
    let
        parse :: ByteString -> ParseResult e [a]
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
        parse :: ByteString -> ParseResult e (Maybe a)
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
