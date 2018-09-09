module Osu.Beatmap.ParserSpec
    (
      spec
    )
where

import Test.Hspec
import Osu.Beatmap
import Osu.Beatmap.HitObject
import Osu.Beatmap.Parser
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

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
        describe "when valid data" $ do
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
                let expected = Just Metadata { beatmapId = 1
                                             , beatmapSetId = 2
                                             , title = "title"
                                             , artist = "artist"
                                             , creator = "creator"
                                             , version = "version"
                                             , source = "" }
                result `shouldBe` expected

        describe "when missing fields" $ do
            let metadataLines = ["[Metadata]"
                                ,"BeatmapID: 1"
                                ,"TitleUnicode: title"
                                ,"ArtistUnicode: artist"
                                ,"Creator: creator"
                                ,"Version: version"
                                ,"Source:"]
            
            it "returns Nothing" $ do
                parseBeatmapMetadata metadataLines `shouldBe` Nothing

        
        describe "when invalid data" $ do
            let metadataLines = ["[Metadata]"
                                ,"BeatmapID: NaN"
                                ,"BeatmapSetID: 2"
                                ,"TitleUnicode: title"
                                ,"ArtistUnicode: artist"
                                ,"Creator: creator"
                                ,"Version: version"
                                ,"Source:"]

            it "returns Nothing" $ do
                parseBeatmapMetadata metadataLines `shouldBe` Nothing
                
    describe "parseBeatmapHitObjects" $ do
        it "parses all hit objects" $ do
            lines <- map T.pack <$> lines <$> readFile "test/fixtures/beatmap.osu"
            let result = parseBeatmapHitObjects lines
            (result :: Maybe [HitObject]) `shouldNotBe` (Just [])
        
        it "returns Nothing if any line is invalid" $ do
            let lines = ["[HitObjects]"
                        ,"129,89,20041,1,10,0:2:0:0:"
                        ,"invalid"
                        ]
            let result = parseBeatmapHitObjects lines
            result `shouldBe` Nothing
