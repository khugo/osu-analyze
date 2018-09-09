module Osu.Utils
    ( foldMaybies
    )
where

import Control.Applicative (liftA2)
import Debug.Trace

foldMaybies :: Show a => [Maybe a] -> Maybe [a]
foldMaybies ms = foldl (liftA2 (\acc m -> acc ++ [m])) (Just []) $ traceShow ms ms
