import Test.Hspec (Spec, describe, hspec)

import Hilcode.MiscSpec qualified

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Misc" Hilcode.MiscSpec.spec
