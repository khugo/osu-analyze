module Osu.Osz

where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import Codec.Archive.Zip (withArchive, getEntries, getEntryName, getEntry, EntrySelector)

getBeatmapStrings :: String -> IO [String]
getBeatmapStrings zipPath = map BS.unpack <$> withArchive zipPath archiveM 
    where archiveM = beatmapEntries >>= mapM getEntry
          beatmapEntries = filter isBeatmapEntry . M.keys <$> getEntries

isBeatmapEntry :: EntrySelector -> Bool
isBeatmapEntry entry = ".osu" `T.isSuffixOf` entryName
    where entryName = getEntryName entry
