module Osu.Beatmap.HitObjectSpec
    (
      spec
    )
where

import Test.Hspec
import Osu.Beatmap.HitObject

spec :: Spec
spec = do
    describe "parseHitObject" $ do
        describe "parsing hit circles" $ do
            let line = "129,89,20041,1,10,0:2:0:0:"
            
            it "parses hit circle" $ do
                let result = parseHitObject line
                result `shouldBe` Just (HitCircle (Transform 129 89 20041))

            describe "with invalid data" $ do
                let invalidLine = "aa,0,0,1"

                it "returns Nothing" $ do
                    let result = parseHitObject invalidLine
                    result `shouldBe` Nothing
                
