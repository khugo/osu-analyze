module Osu.Utils
    ( foldMaybies
    )
where

import Control.Applicative (liftA2)

foldMaybies :: [Maybe a] -> Maybe [a]
foldMaybies = foldl (liftA2 (\acc m -> acc ++ [m])) (Just [])
