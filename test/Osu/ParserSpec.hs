module Osu.ParserSpec
    (
      spec
    )
where

import Test.Hspec
import Osu.Parser
import Osu.Beatmap
import qualified Data.Map.Lazy as M

spec :: Spec
spec = do
    let beatmapLines = ["osu file format v14"
                       , ""
                       , "[General]"
                       , "a: 1"
                       , "b: 2"
                       , "invalid"
                       , ""
                       , "[Editor]"
                       , "c: 1"
                       , ""
                       , "[Empty]"]


    describe "extractSectionLines" $ do
        it "extracts relevant lines" $ do
            let result = extractSectionLines "General" beatmapLines 
            result `shouldBe` ["a: 1", "b: 2", "invalid"]

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

        describe "when section does not exist" $ do
            it "returns an empty map" $ do
                let result = extractSection "NonExistant" beatmapLines
                result `shouldBe` M.empty

        describe "when section is empty" $ do
            it "returns an empty map" $ do
                let result = extractSection "Empty" beatmapLines
                result `shouldBe` M.empty

    describe "parseBeatmapMetadata" $ do
        let metadataLines = ["[Metadata]"
                            ,"BeatmapID: 1"
                            ,"BeatmapSetID: 2"
                            ,"TitleUnicode: title"
                            ,"ArtistUnicode: artist"
                            ,"Creator: creator"
                            ,"Version: version"
                            ,"Source:"]

        it "parses the metadata" $ do
            let result = parseBeatmapMetadata metadataLines
            let expected = Metadata { beatmapId = 1
                                             , beatmapSetId = 2
                                             , title = "title"
                                             , artist = "artist"
                                             , creator = "creator"
                                             , version = "version"
                                             , source = "" }
            result `shouldBe` expected
