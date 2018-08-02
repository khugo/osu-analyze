import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "root" $ do
    it "works" $ 1 `shouldBe` 1
    it "asd" $ 1 `shouldBe` 1
