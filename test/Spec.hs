import Test.Hspec
import Lib (extractSectionLines, extractSection)
import qualified Data.Map.Lazy as M

main :: IO ()
main = hspec $ do
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


    describe "extractSectionLines" $ do
        it "extracts relevant lines" $ do
            let result = extractSectionLines "General" beatmapLines 
            result `shouldBe` ["a: 1", "b: 2"]

        describe "when section does not exist" $ do
            it "returns empty list" $ do
                let result = extractSectionLines "NonExistant" beatmapLines
                result `shouldBe` []

        describe "when section is empty" $ do
            it "returns empty list" $ do
                let result = extractSectionLines "Empty" beatmapLines
                result `shouldBe` []

    describe "extractSection" $ do
        it "extracts section as a map" $ do
            let result = extractSection "General" beatmapLines
            result `shouldBe` M.fromList [("a","1"),("b","2")]
