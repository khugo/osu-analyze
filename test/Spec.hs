import Test.Hspec
import qualified Osu.Beatmap.ParserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Parser" Osu.Beatmap.ParserSpec.spec
