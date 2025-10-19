module Hilcode.MiscSpec (spec) where

import Test.Hspec

import Hilcode.Misc

increment :: Int -> Int
increment v = v + 1

ints :: [Int]
ints = [10, 20, 30]

spec :: Spec
spec = describe "Misc::lift" $ do
    it "lift Nothing Nothing --> Nothing" $
        lift Nothing          Nothing     `shouldBe` (Nothing :: Maybe [Int])
    it "lift Nothing (Just [10, 20, 30]) --> Nothing" $
        lift Nothing          (Just ints) `shouldBe` (Nothing :: Maybe [Int])
    it "lift (Just increment) Nothing --> Nothing" $
        lift (Just increment) Nothing     `shouldBe` (Nothing :: Maybe [Int])
    it "lift (Just increment) (Just [10, 20, 30]) --> Just [11, 21, 31]" $
        lift (Just increment) (Just ints) `shouldBe` Just [11, 21, 31]
