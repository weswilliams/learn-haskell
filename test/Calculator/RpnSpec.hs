module Calculator.RpnSpec where

import SpecHelper
import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "solveRPN" $ do
    context "with text input" $ do
      it "should do simple add of two numbers" $ do
        let rpnString = "1 2 +"
        solveRPN rpnString `shouldBe` 3

      it "should use the value of the previous operation in a following operation" $ do
        let rpnString = "1 2 + 4 -"
        solveRPN rpnString `shouldBe` -1.0

      it "should require two number prior to an operation" $ do
        let rpnString = "10 +"
        evaluate (solveRPN rpnString) `shouldThrow` anyException

      it "should require a result and another number for an operator" $ do
        let rpnString = "10 5 + -"
        evaluate (solveRPN rpnString) `shouldThrow` anyException

main :: IO ()
main = hspec spec