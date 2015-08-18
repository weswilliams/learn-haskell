module Fizzbuzz.FizzbuzzSpec where


import SpecHelper
import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "fizzBuzz" $ do
    it "should return Fizz when number divisible by 3" $ do
      fizzBuzz [3] `shouldBe` "Fizz"

    it "should return Buzz when number divisible by 5" $ do
      fizzBuzz [5] `shouldBe` "Buzz"

    it "should return FizzBuzz when number divisible by 3 and 5" $ do
      fizzBuzz [15] `shouldBe` "FizzBuzz"

    it "should return the number when not divisible by 3 or 5" $ do
      fizzBuzz [2] `shouldBe` "2"

    it "should handle many numbers" $ do
      fizzBuzz [1..25] `shouldBe` "12Fizz4BuzzFizz78FizzBuzz11Fizz1314FizzBuzz1617Fizz19BuzzFizz2223FizzBuzz"
