module Osu.Beatmap.Parser
    (
      parseBeatmapMetadata 
    , parseBeatmapHitObjects
    , extractSection
    , extractSectionLines
    , foldMaybies
    )
where

import qualified Data.Map.Lazy as M
import Data.Text (Text, strip, unpack, pack, splitOn)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Osu.Beatmap
import Osu.Beatmap.HitObject (HitObject(..), parseHitObject)
import Osu.Utils (foldMaybies)

parseBeatmapMetadata :: [Text] -> Maybe Metadata
parseBeatmapMetadata allLines = do
    let metadata = extractSection "Metadata" allLines
    beatmapId <- readMaybe . unpack =<< metadata M.!? "BeatmapID" 
    beatmapSetId <- readMaybe . unpack =<< metadata M.!? "BeatmapSetID"
    title <- metadata M.!? "TitleUnicode"
    artist <- metadata M.!? "ArtistUnicode"
    creator <- metadata M.!? "Creator"
    version <- metadata M.!? "Version"
    source <- metadata M.!? "Source"
    return Metadata { beatmapId = beatmapId
                    , beatmapSetId = beatmapSetId
                    , title = title
                    , artist = artist
                    , creator = creator
                    , version = version
                    , source = source
                    }

parseBeatmapHitObjects :: [Text] -> Maybe [HitObject]
parseBeatmapHitObjects allLines = foldMaybies $ map parseHitObject hitObjectLines
    where hitObjectLines = extractSectionLines "HitObjects" allLines


extractSection :: Text -> [Text] -> M.Map Text Text
extractSection sectionName allLines = makeMap $ extractSectionLines sectionName allLines
    where makeMap = M.fromList . pairs
          pairs = mapMaybe (toTuple . splitLine)
          toTuple [k,v] = Just (k,v)
          toTuple _ = Nothing
          splitLine = map strip . splitOn ":"
 
extractSectionLines :: Text -> [Text] -> [Text]
extractSectionLines sectionName = map pack . stripHeader . sectionLines . map unpack
    where stripHeader [] = []
          stripHeader xs = tail xs
          sectionLines = takeWhile (/= "") . dropWhile (/= sectionHeader)
          sectionHeader = "[" ++ unpack sectionName ++ "]"

beatmapLines :: Text -> [Text]
beatmapLines = splitOn "\r\n"

