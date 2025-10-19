module Hilcode.Checks where

import Hedgehog
import Hilcode.Prelude

assertNot :: Bool -> PropertyT IO ()
assertNot predicate = assert (not predicate)

propertyCheckEq :: (Show a, Eq a) => Gen a -> PropertyT IO ()
propertyCheckEq generator = do
    (lft, rgt) <- generateTuple generator
    assert $ lft == lft
    assert $ rgt == rgt
    assert $ (lft == rgt) == (rgt == lft)
    footnote $ "     lft == rgt      --> " <> show (lft == rgt)
    footnote $ "show lft == show rgt --> " <> show (show lft == show rgt)
    assert $ (lft == rgt) == (show lft == show rgt)
    assert $ (rgt /= lft) == (lft /= rgt)
    assert $ (lft /= rgt) == (show lft /= show rgt)

propertyCheckOrd :: (Show a, Ord a) => Gen a -> Property
propertyCheckOrd generator = property $ do
    (lft, rgt) <- generateTuple generator
    assertNot $ lft < lft
    assertNot $ lft > lft
    assert $ rgt <= rgt
    assert $ rgt >= rgt
    assert $ lft `compare` rgt /= rgt `compare` lft || lft == rgt
    assert $ lft `min` rgt < rgt `max` lft || lft == rgt
    assert $ lft `max` rgt > rgt `min` lft || lft == rgt
    assert $ lft `min` rgt == rgt `min` lft
    assert $ lft `max` rgt == rgt `max` lft
    assert $ lft `min` rgt /= rgt `max` lft || lft == rgt
    assert $ lft `max` rgt /= rgt `min` lft || rgt == lft
    assert $ (lft > rgt) == (rgt < lft)
    assert $ (lft < rgt) == (rgt > lft)
    assert $ (lft >= rgt) == (rgt <= lft)
    assert $ (lft <= rgt) == (rgt >= lft)

-- propertyCheckSemigroup :: (Show a, Eq a, Semigroup a) => Gen a -> Property
-- propertyCheckSemigroup generator = property $ do
--     (one, two, three) <- generateTriple generator
--     assert $ (one <> two) <> three == one <> (two <> three)
--     assert $ (one <> three) <> two == one <> (three <> two)
--     assert $ (two <> one) <> three == two <> (one <> three)
--     assert $ (two <> three) <> one == two <> (three <> one)
--     assert $ (three <> one) <> two == three <> (one <> two)
--     assert $ (three <> two) <> one == three <> (two <> one)

generateTuple :: (Show a) => Gen a -> PropertyT IO (a, a)
generateTuple generator = do
    lft <- Hedgehog.forAll generator
    footnote $ "lft = '" <> show lft <> "'"
    rgt <- Hedgehog.forAll generator
    footnote $ "rgt = '" <> show rgt <> "'"
    pure (lft, rgt)

-- generateTriple :: Show a => Gen a -> PropertyT IO (a, a, a)
-- generateTriple generator = do
--     one <- Hedgehog.forAll generator
--     footnote $ "one = '" <> show one <> "'"
--     two <- Hedgehog.forAll generator
--     footnote $ "two = '" <> show two <> "'"
--     three <- Hedgehog.forAll generator
--     footnote $ "three = '" <> show three <> "'"
--     pure (one, two, three)
