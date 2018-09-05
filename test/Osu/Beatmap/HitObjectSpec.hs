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
        
        describe "parsing sliders" $ do
            it "parses linear slider" $ do
                let line = "436,64,26760,6,0,L|428:148,1,85,12|0,0:2|1:2,0:0:0:0:"
                let result = parseHitObject line
                result `shouldBe` Just (Slider { transform = Transform 436 64 26760
                                               , path = Linear (428,148)
                                               , sliderRepeat = 1
                                               , pixelLength = 85
                                               , duration = 0
                                               })

            it "parses perfect slider" $ do
                let line = "145,173,33635,6,0,P|181:214|177:265,1,85,2|2,1:2|1:2,0:0:0:0:"
                let result = parseHitObject line
                result `shouldBe` Just (Slider { transform = Transform 145 173 33635
                                               , path = Perfect (181,214) (177,265)
                                               , sliderRepeat = 1
                                               , pixelLength = 85
                                               , duration = 0
                                               })
