module Test where

import Core
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec
import Test.Hspec.Hedgehog (
    PropertyT,
    diff,
    forAll,
    hedgehog,
    (/==),
    (===),
 )

spec :: Spec
spec = do
    describe "mylength" $ do
        it "should give zero for an empty list" $
            mylength [] `shouldBe` (0 :: Int)
        it "should give 1 for a list with one element" $
            mylength [1 :: Int] `shouldBe` (1 :: Integer)
    describe "hedgehog test" $ do
        it "should work" $
            hedgehog $ do
                x <- forAll $ Gen.integral (Range.linear 0 1000)
                y <- forAll $ Gen.integral (Range.linear 0 5000)
                diff (x + y + x) (==) (x + x + y)
