module Lib
    ( 
          extractSectionLines
        , extractSection
        , parseBeatmapMetadata
        , BeatmapMetadata (..)
    )
where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Lazy as M
import Data.List.Split (splitOn)
import Codec.Archive.Zip (withArchive, getEntries, getEntryName, getEntry, EntrySelector)

data BeatmapMetadata = MkBeatmapMetadata { beatmapId :: Int
                                         , beatmapSetId :: Int
                                         , title :: String
                                         , artist :: String
                                         , creator :: String
                                         , version :: String
                                         , source :: String
                                         } deriving (Show, Eq)

getBeatmapStrings :: String -> IO [String]
getBeatmapStrings zipPath = do
    entries <- withArchive zipPath (M.keys <$> getEntries)
    let beatmapEntries = filter isBeatmapEntry entries
    byteStrings <- withArchive zipPath (mapM getEntry beatmapEntries)
    return $ map BS.unpack byteStrings 

isBeatmapEntry :: EntrySelector -> Bool
isBeatmapEntry entry = ".osu" `T.isSuffixOf` entryName
    where entryName = getEntryName entry

parseBeatmapMetadata :: [String] -> BeatmapMetadata
parseBeatmapMetadata allLines = MkBeatmapMetadata {beatmapId=read $ metadata M.! "BeatmapID"
                                                  ,beatmapSetId=read $ metadata M.! "BeatmapSetID"
                                                  ,title=metadata M.! "TitleUnicode"
                                                  ,artist=metadata M.! "ArtistUnicode"
                                                  ,creator=metadata M.! "Creator"
                                                  ,version=metadata M.! "Version"
                                                  ,source=metadata M.! "Source"} 
    where
        metadata = extractSection "Metadata" allLines

extractSection :: String -> [String] -> M.Map String String
extractSection sectionName allLines = makeMap $ extractSectionLines sectionName allLines
    where makeMap = M.fromList . pairs
          pairs = map (toPair . splitLine)
          toPair [k,v] = (k,v)
          splitLine = map (T.unpack . T.strip . T.pack) . splitOn ":"
 
extractSectionLines :: String -> [String] -> [String]
extractSectionLines sectionName = stripHeader . sectionLines
    where stripHeader [] = []
          stripHeader xs = tail xs
          sectionLines = takeWhile (/= "") . dropWhile (/= "[" ++ sectionName ++ "]")

beatmapLines :: String -> [String]
beatmapLines = splitOn "\r\n"
