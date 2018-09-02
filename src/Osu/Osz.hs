module Osu.Osz

where

import qualified Data.Map.Lazy as M
import Data.Text (Text, isSuffixOf)
import Data.Text.Encoding (decodeUtf8)
import Codec.Archive.Zip (withArchive, getEntries, getEntryName, getEntry, EntrySelector)

getBeatmapStrings :: String -> IO [Text]
getBeatmapStrings zipPath = map decodeUtf8 <$> withArchive zipPath archiveM 
    where archiveM = beatmapEntries >>= mapM getEntry
          beatmapEntries = filter isBeatmapEntry . M.keys <$> getEntries

isBeatmapEntry :: EntrySelector -> Bool
isBeatmapEntry entry = ".osu" `isSuffixOf` entryName
    where entryName = getEntryName entry
