module Hilcode.Misc (
    lift,
) where

import Control.Applicative (Applicative, pure, (<$>))
import Control.Monad (Monad)

lift ::
    (Monad monad, Applicative applicative) =>
    monad (a -> b) ->
    monad (applicative a) ->
    monad (applicative b)
lift mf mv = do
    f <- mf
    v <- mv
    pure (f <$> v)
