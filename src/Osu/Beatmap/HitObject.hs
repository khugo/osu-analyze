module Osu.Beatmap.HitObject
    (
        HitObject
    )

where

import Data.Text (Text, splitOn, unpack)
import Text.Read (readMaybe)
import Data.Bits (testBit)

data Transform = Transform { x :: Int
                           , y :: Int
                           , time :: Int
                           }

data HitObject = HitCircle Transform

parseHitObject :: Text -> Maybe HitObject
parseHitObject definition = do
    x <- getX
    y <- getY
    time <- getTime
    type' <- getType
    let transform = Transform { x = x
                              , y = y
                              , time = time
                              }
    if | testBit (1 :: Int) type' -> Just (HitCircle transform)
       | otherwise -> Nothing
    where 
        getX = parseParts >>= (\(x,_,_,_) -> readMaybe x :: Maybe Int)
        getY = parseParts >>= (\(_,y,_,_) -> readMaybe y :: Maybe Int)
        getTime = parseParts >>= (\(_,_,time,_) -> readMaybe time :: Maybe Int)
        getType = parseParts >>= (\(_,_,_,type') -> readMaybe type' :: Maybe Int)
        parseParts = case splitParts of
                       (x:y:time:type':_) -> Just (x,y,time,type') 
                       _ -> Nothing
        splitParts = map unpack $ splitOn "," definition
