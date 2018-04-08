import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)


takeFiveInts :: [Int] -> [Int]
takeFiveInts = take 5


main = hspec $ do
  describe "head" $ do
    it "returns the first element of a list" $ do
      head ['a' ..] `shouldBe` 'a'

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

  describe "takeFiveInts" $ do
    it "always returns a list with 5 or fewer elements" $
      property (\xs -> length (takeFiveInts xs) <= 5)
