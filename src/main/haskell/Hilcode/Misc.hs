module Hilcode.Misc
( lift
) where

lift :: (Monad monad, Applicative applicative)
     => monad (a -> b)
     -> monad (applicative a)
     -> monad (applicative b)
lift mf mv = do
    f <- mf
    v <- mv
    pure (f <$> v)
