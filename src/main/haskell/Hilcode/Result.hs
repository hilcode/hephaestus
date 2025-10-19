{-# LANGUAGE NoImplicitPrelude #-}

module Hilcode.Result (
    Result (..),
    fail,
    mapError,
    fromEither,
    isFailure,
    isOk,
    ok,
    result,
) where

import Control.Applicative (Applicative (pure, (<*>)))
import Control.Monad (Monad ((>>=)))
import Data.Bifoldable (Bifoldable (bifoldMap, bifoldr))
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Bool (Bool (False, True))
import Data.Either (Either (Left, Right))
import Data.Foldable (Foldable (foldMap, foldr, length, null))
import Data.Functor (Functor (fmap), (<$>))
import Data.Int (Int)
import Data.Monoid (Monoid (mempty))
import Data.Semigroup (Semigroup ((<>)))
import Data.Traversable (Traversable (traverse))
import Prelude (Show (show), String)

data Result failure value
    = Ok value
    | Failure failure

instance (Show failure, Show value) => Show (Result failure value) where
    show :: Result failure value -> String
    show (Ok value) = "Ok (" <> show value <> ")"
    show (Failure failure) = "Failure (" <> show failure <> ")"

instance Semigroup (Result failure value) where
    (<>) ::
        Result failure value ->
        Result failure value ->
        Result failure value
    Failure _ <> result = result
    result <> _ = result

instance Functor (Result failure) where
    fmap ::
        (input -> output) ->
        Result failure input ->
        Result failure output
    fmap _ (Failure failure) = Failure failure
    fmap function (Ok value) = Ok (function value)

instance Applicative (Result failure) where
    pure :: value -> Result failure value
    pure = Ok
    (<*>) ::
        Result failure (input -> output) ->
        Result failure input ->
        Result failure output
    Failure failure <*> _ = Failure failure
    _ <*> Failure failure = Failure failure
    Ok function <*> Ok input = Ok (function input)

instance Monad (Result failure) where
    (>>=) ::
        Result failure input ->
        (input -> Result failure output) ->
        Result failure output
    Failure failure >>= _ = Failure failure
    Ok input >>= function = function input

instance Foldable (Result failure) where
    foldMap ::
        (Monoid monoid) =>
        (value -> monoid) ->
        Result failure value ->
        monoid
    foldMap _ (Failure _) = mempty
    foldMap function (Ok value) = function value

    foldr ::
        (input -> output -> output) ->
        output ->
        Result failure input ->
        output
    foldr _ zero (Failure _) = zero
    foldr function zero (Ok value) = function value zero

    length ::
        Result failure value ->
        Int
    length (Failure _) = 0
    length (Ok _) = 1

    null ::
        Result failure value ->
        Bool
    null = isFailure

instance Traversable (Result failure) where
    traverse ::
        (Applicative applicative) =>
        (input -> applicative output) ->
        Result failure input ->
        applicative (Result failure output)
    traverse _ (Failure failure) = pure (Failure failure)
    traverse function (Ok value) = Ok <$> function value

instance Bifunctor Result where
    first ::
        (failure -> output) ->
        Result failure value ->
        Result output value
    first mapFailure result =
        case result of
            Ok value ->
                Ok value
            Failure failure ->
                Failure (mapFailure failure)

    second ::
        (input -> output) ->
        Result failure input ->
        Result failure output
    second mapValue result =
        case result of
            Ok value ->
                Ok (mapValue value)
            Failure failure ->
                Failure failure

    bimap ::
        (failureInput -> failureOutput) ->
        (input -> output) ->
        Result failureInput input ->
        Result failureOutput output
    bimap mapFailure mapValue result =
        case result of
            Ok value ->
                Ok (mapValue value)
            Failure failure ->
                Failure (mapFailure failure)

instance Bifoldable Result where
    bifoldMap ::
        (failure -> monoid) ->
        (value -> monoid) ->
        Result failure value ->
        monoid
    bifoldMap mapFailure mapValue result =
        case result of
            Ok value ->
                mapValue value
            Failure failure ->
                mapFailure failure
    bifoldr ::
        (failure -> output -> output) ->
        (value -> output -> output) ->
        output ->
        Result failure value ->
        output
    bifoldr foldFailure foldValue zero result =
        case result of
            Ok value ->
                foldValue value zero
            Failure failure ->
                foldFailure failure zero

ok ::
    value ->
    Result failure value
ok = Ok

fail ::
    failure ->
    Result failure value
fail = Failure

isOk ::
    Result failure value ->
    Bool
isOk result =
    case result of
        Ok _ ->
            True
        Failure _ ->
            False

isFailure ::
    Result failure value ->
    Bool
isFailure result =
    case result of
        Failure _ ->
            True
        Ok _ ->
            False

result ::
    (failure -> output) ->
    (input -> output) ->
    Result failure input ->
    output
result mapFailure mapInput result =
    case result of
        Ok input ->
            mapInput input
        Failure failure ->
            mapFailure failure

fromEither ::
    Either failure value ->
    Result failure value
fromEither (Left failure) =
    Failure failure
fromEither (Right value) =
    Ok value

mapError ::
    (failure -> output) ->
    Result failure value ->
    Result output value
mapError = first
