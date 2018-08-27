module Osu.Parser
    (
      parseBeatmapMetadata 
    , extractSection
    , extractSectionLines
    )
where

import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Osu.Beatmap

parseBeatmapMetadata :: [String] -> Metadata
parseBeatmapMetadata allLines = Metadata { beatmapId = read $ metadata M.! "BeatmapID"
                                                  , beatmapSetId = read $ metadata M.! "BeatmapSetID"
                                                  , title = metadata M.! "TitleUnicode"
                                                  , artist = metadata M.! "ArtistUnicode"
                                                  , creator = metadata M.! "Creator"
                                                  , version = metadata M.! "Version"
                                                  , source = metadata M.! "Source" }
    where
        metadata = extractSection "Metadata" allLines

extractSection :: String -> [String] -> M.Map String String
extractSection sectionName allLines = makeMap $ extractSectionLines sectionName allLines
    where makeMap = M.fromList . pairs
          pairs = mapMaybe (toTuple . splitLine)
          toTuple [k,v] = Just (k,v)
          toTuple _ = Nothing
          splitLine = map (T.unpack . T.strip . T.pack) . splitOn ":"
 
extractSectionLines :: String -> [String] -> [String]
extractSectionLines sectionName = stripHeader . sectionLines
    where stripHeader [] = []
          stripHeader xs = tail xs
          sectionLines = takeWhile (/= "") . dropWhile (/= "[" ++ sectionName ++ "]")

beatmapLines :: String -> [String]
beatmapLines = splitOn "\r\n"

