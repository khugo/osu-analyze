import           Test.Hspec
import Lib (extractSection)

main :: IO ()
main = hspec $ do
    describe "extractSection" $ do
        let beatmapLines = ["osu file format v14"
                           , ""
                           , "[General]"
                           , "a: 1"
                           , "b: 2"
                           , ""
                           , "[Editor]"
                           , "c: 1"
                           , ""
                           , "[Empty]"]

        it "extracts relevant lines" $ do
            let result = extractSection "General" beatmapLines 
            result `shouldBe` ["a: 1", "b: 2"]

        describe "when section does not exist" $ do
            it "returns empty list" $ do
                let result = extractSection "NonExistant" beatmapLines
                result `shouldBe` []

        describe "when section is empty" $ do
            it "returns empty list" $ do
                let result = extractSection "Empty" beatmapLines
                result `shouldBe` []
