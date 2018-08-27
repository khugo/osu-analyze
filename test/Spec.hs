import Test.Hspec
import qualified Osu.ParserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Parser" Osu.ParserSpec.spec
