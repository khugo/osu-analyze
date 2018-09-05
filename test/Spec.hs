import Test.Hspec
import qualified Osu.Beatmap.ParserSpec
import qualified Osu.Beatmap.HitObjectSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Parser" Osu.Beatmap.ParserSpec.spec
    describe "HitObject" Osu.Beatmap.HitObjectSpec.spec
