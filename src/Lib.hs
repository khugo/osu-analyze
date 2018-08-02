module Lib
    ( someFunct'
    , test
    )
where

someFunct' :: IO ()
someFunct' = putStrLn "someFunccc"

test :: Int -> Int
test = \case
    1 -> 8
