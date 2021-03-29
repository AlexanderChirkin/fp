-- | Test Haskell tr implementation.
--
-- We provide a few very simple tests here as a demonstration. You should add
-- more of your own tests!
--
-- Run the tests with `stack test`.
module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Tr

type CharSet' = NonEmptyList Char

tr' :: CharSet -> CharSet -> String -> String
tr' inset outset = tr inset (Just outset)

-- | Test harness.
main :: IO ()
main = hspec $ describe "Testing tr" $ do
    describe "single translate" $
      it "a -> b" $
        tr' "a" "b" "a" `shouldBe` "b"

    describe "stream translate" $
      it "a -> b" $
        tr' "a" "b" "aaaa" `shouldBe` "bbbb"

    describe "extend input set" $
      it "abc -> d" $
        tr' "abc" "d" "abcd" `shouldBe` "dddd"

    describe "no translate" $
      it "qwe -> xyz" $
        tr' "qwe" "xyz" "abcd" `shouldBe` "abcd"

    describe "no translate 2" $
      it "abc -> abc" $
        tr' "abc" "abc" "abcd" `shouldBe` "abcd"

    describe "long 'to'" $
      it "abc -> xyzzxcvbn" $
        tr' "abc" "xyzzxcvbn" "abcdxcv" `shouldBe` "xyzdxzv"

    describe "upper vowel" $
      it "aoeiu -> AOEIU" $
        tr' "aoeiu" "AOEIU" "a neighborhood of infinity" `shouldBe` "A nEIghbOrhOOd Of InfInIty"

    describe "tr quick-check" $
      it "empty input is identity" $ property prop_empty_id

    describe "delete a" $
      it "-d -> a -> abracadabra" $
        tr "a" Nothing "abracadabra" `shouldBe` "brcdbr"

    describe "delete all" $
      it "-d -> abcdr -> abracadabra" $
        tr "abcdr" Nothing "abracadabra" `shouldBe` ""

-- | An example QuickCheck test. Tests the invariant that `tr` with an empty
-- input string should produce and empty output string.
prop_empty_id :: CharSet' -> CharSet' -> Bool
prop_empty_id (NonEmpty set1) (NonEmpty set2)
  = tr' set1 set2 "" == ""
