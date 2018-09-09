module Osu.Beatmap.HitObject
    (
        HitObject(..),
        Transform(..),
        SliderPath(..),
        parseHitObject
    )

where

import Data.Text (Text, splitOn, unpack)
import Text.Read (readMaybe)
import Data.Bits (testBit)
import Osu.Utils (foldMaybies)

data Transform = Transform { x :: Int
                           , y :: Int
                           , time :: Int
                           } deriving (Show, Eq)

type Point = (Int,Int)
data SliderPath = Linear { end :: Point }
                | Perfect { passThrough :: Point, end :: Point }
                | Bezier { points :: [Point] } deriving (Show, Eq)

data HitObject = HitCircle Transform
               | Slider { transform :: Transform
                        , path :: SliderPath
                        , sliderRepeat :: Int
                        , pixelLength :: Float
                        , duration :: Int
                        } deriving (Show, Eq)

parseHitObject :: Text -> Maybe HitObject
parseHitObject definition = do
    (type',transform,rest) <- parseParts
    if | testBit type' 0 -> Just (HitCircle transform)
       | testBit type' 1 -> parseSlider transform rest
       | otherwise -> Nothing
    where 
        parseParts = case splitParts of
                       (xStr:yStr:timeStr:typeStr:rest) -> do
                           x <- readMaybe $ unpack xStr :: Maybe Int
                           y <- readMaybe $ unpack yStr :: Maybe Int
                           time <- readMaybe $ unpack timeStr :: Maybe Int
                           type' <- readMaybe $ unpack typeStr :: Maybe Int
                           Just (type',Transform x y time,rest)
                       _ -> Nothing
        splitParts = splitOn "," definition

parseSlider :: Transform -> [Text] -> Maybe HitObject
parseSlider transform rest = do
    (sliderType,path,sliderRepeat,pixelLength) <- parts
    pathPoints <- foldMaybies $ map parsePathPoint path
    sliderPath <- makeSliderPath sliderType pathPoints
    Just (Slider { transform = transform
                 , path = sliderPath
                 , sliderRepeat = sliderRepeat
                 , pixelLength = pixelLength
                 , duration = 0
                 })
    where
        parts = case rest of (_:pathDef:repeatStr:pixelLengthStr:_) -> do
                                       sliderRepeat <- readMaybe $ unpack repeatStr :: Maybe Int
                                       pixelLength <- readMaybe $ unpack pixelLengthStr :: Maybe Float
                                       (sliderType,path) <- splitPathDef pathDef
                                       Just (sliderType,path,sliderRepeat,pixelLength)
                             _ -> Nothing
        splitPathDef def = case splitOn "|" def of
                             (sliderType:path) -> Just (sliderType,path)
                             _ -> Nothing
        

makeSliderPath :: Text -> [Point] -> Maybe SliderPath
makeSliderPath "L" [end] = Just (Linear end)
makeSliderPath "P" [passThrough,end] = Just (Perfect passThrough end)
makeSliderPath "B" points = Just (Bezier points)
makeSliderPath _ _ = Nothing

parsePathPoint :: Text -> Maybe Point
parsePathPoint def = case splitOn ":" def of
                       [xStr,yStr] -> do
                           x <- readMaybe $ unpack xStr :: Maybe Int
                           y <- readMaybe $ unpack yStr :: Maybe Int
                           Just (x,y)
                       _ -> Nothing

