import Test.Hspec (Spec, describe, hspec)

import Hilcode.MiscSpec qualified
import Hilcode.ParserSpec qualified
import Prelude (IO)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Misc" Hilcode.MiscSpec.spec
    describe "Parser" Hilcode.ParserSpec.spec
