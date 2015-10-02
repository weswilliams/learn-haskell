module Calculator.CalcSpec where

import SpecHelper
import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "reduce by" $ do
    context "exact amount" $ do
      it "should do simple subtraction" $ do
        reduce 15 by 4 Exactly `shouldBe` 11
    context "percentage" $ do
      it "should reduce number by a percentage" $ do
        reduce 15 by 10 Percent `shouldBe` 13.5
    context "fractional amount" $ do
      it "should reduce number by a fractional amount" $ do
        reduce 15 by (1/3) Fractionally `shouldBe` 10

main :: IO ()
main = hspec spec