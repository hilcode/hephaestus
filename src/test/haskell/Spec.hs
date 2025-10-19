import Test.Hspec

import qualified Hilcode.MiscSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Misc" Hilcode.MiscSpec.spec
