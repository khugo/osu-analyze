import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "root" $ do
    it "works" $ do
      1 `shouldBe` 1
    it "asd" $ do
      1 `shouldBe` 1
