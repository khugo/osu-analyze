module Osu.Beatmap
    (
        Metadata(..)
    )

where

data Metadata = Metadata { beatmapId :: Int
                         , beatmapSetId :: Int
                         , title :: String
                         , artist :: String
                         , creator :: String
                         , version :: String
                         , source :: String
                         } deriving (Show, Eq)


