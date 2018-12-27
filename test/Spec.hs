
import Test.Hspec
import qualified TimelineSpec

main :: IO ()
main = hspec spec


spec :: Spec
spec = describe "Timeline"  TimelineSpec.spec
