import Test.Hspec (Spec, describe, hspec)

import Hilcode.MiscSpec qualified
import Prelude (IO)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Misc" Hilcode.MiscSpec.spec
