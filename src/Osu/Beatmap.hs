module Osu.Beatmap
    (
        Metadata(..)
    )

where

import Data.Text (Text)

data Metadata = Metadata { beatmapId :: Int
                         , beatmapSetId :: Int
                         , title :: Text
                         , artist :: Text
                         , creator :: Text
                         , version :: Text
                         , source :: Text
                         } deriving (Show, Eq)


