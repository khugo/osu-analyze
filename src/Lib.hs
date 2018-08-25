module Lib
    ( 
        extractSection
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
                                         } deriving (Show)

getBeatmapStrings :: String -> IO [String]
getBeatmapStrings zipPath = do
    entries <- withArchive zipPath (M.keys <$> getEntries)
    let beatmapEntries = filter isBeatmapEntry entries
    byteStrings <- withArchive zipPath (mapM getEntry beatmapEntries)
    return $ map BS.unpack byteStrings 

isBeatmapEntry :: EntrySelector -> Bool
isBeatmapEntry entry = ".osu" `T.isSuffixOf` entryName
    where entryName = getEntryName entry

parseBeatmap :: String -> Int
parseBeatmap definition = 1
    where
        beatmapLines = splitOn "\r\n" definition

parseBeatmapMetadata :: [String] -> Int
parseBeatmapMetadata beatmapLines = 1
    where
        lines = extractSection "Metadata" beatmapLines
        
extractSection :: String -> [String] -> [String]
extractSection sectionName = stripHeader . sectionLines
    where stripHeader [] = []
          stripHeader xs = tail xs
          sectionLines = takeWhile (/= "") . dropWhile (/= "[" ++ sectionName ++ "]")

beatmapLines :: String -> [String]
beatmapLines = splitOn "\r\n"
