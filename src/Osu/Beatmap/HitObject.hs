module Osu.Beatmap.HitObject
    (
        HitObject(..),
        Transform(..),
        parseHitObject
    )

where

import Data.Text (Text, splitOn, unpack)
import Text.Read (readMaybe)
import Data.Bits (testBit)

data Transform = Transform { x :: Int
                           , y :: Int
                           , time :: Int
                           } deriving (Show, Eq)

type Point = (Int,Int)
data SliderPath = Linear { end :: Point }
                | Perfect { start :: Point, passThrough :: Point, end :: Point }
                | Bezier { points :: [Point] } deriving (Show, Eq)

data HitObject = HitCircle Transform
               | Slider { transform :: Transform
                        , path :: SliderPath
                        , repeat :: Int
                        } deriving (Show, Eq)

parseHitObject :: Text -> Maybe HitObject
parseHitObject definition = do
    (type',transform,rest) <- parseParts
    if | testBit type' 0 -> Just (HitCircle transform)
       | otherwise -> Nothing
    where 
        parseParts = case splitParts of
                       (xStr:yStr:timeStr:typeStr:rest) -> do
                           x <- readMaybe xStr :: Maybe Int
                           y <- readMaybe yStr :: Maybe Int
                           time <- readMaybe timeStr :: Maybe Int
                           type' <- readMaybe typeStr :: Maybe Int
                           Just (type',Transform x y time,rest)
                       _ -> Nothing
        splitParts = map unpack $ splitOn "," definition

parseSlider :: Transform -> [String] -> HitObject
parseSlider transform rest = undefined
