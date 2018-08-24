module Lib
    ( 
        getBeatmapEntries
    )
where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Map.Lazy as M
import Codec.Archive.Zip (withArchive, getEntries, getEntryName, EntrySelector)

getBeatmapEntries :: String -> IO [EntrySelector]
getBeatmapEntries zipPath = do
    entries <- withArchive zipPath (M.keys <$> getEntries)
    return $ filter isBeatmapEntry entries

isBeatmapEntry :: EntrySelector -> Bool
isBeatmapEntry entry = L.isSuffixOf ".osu" entryName
    where entryName = T.unpack $ getEntryName entry


