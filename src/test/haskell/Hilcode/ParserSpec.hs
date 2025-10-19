module Hilcode.ParserSpec (
    spec,
) where

import Hedgehog (Gen)
import Hedgehog.Gen qualified
import Hedgehog.Range qualified
import Hilcode.Checks (propertyCheckEq)
import Hilcode.Parser
import Hilcode.Prelude
import Hilcode.Result (Result (..))
import System.OsPath (OsChar, OsString)
import System.OsString qualified
import Test.Hspec (
    Spec,
    SpecWith,
    describe,
    it,
    shouldBe,
 )
import Test.Hspec.Hedgehog qualified

data DummyParseError
    = Dummy1
    | Dummy2
    deriving stock (Eq, Ord, Show)

spec :: Spec
spec = describe "Parser" $ do
    testParseResult
    testFail
    testEndOfInput
    testOsString
    testOsChar
    testAnyOsChar
    testFirstOf
    testRepeat
    testRepeat1
    testOptionally

genOsChar :: Gen OsChar
genOsChar = System.OsString.unsafeFromChar <$> Hedgehog.Gen.alphaNum

genSource :: Gen Source
genSource = Hedgehog.Gen.list (Hedgehog.Range.constantFrom 0 0 10) genOsChar

genOsString :: Gen OsString
genOsString = System.OsString.pack <$> Hedgehog.Gen.list (Hedgehog.Range.constantFrom 0 0 10) genOsChar

genOk :: Gen (Maybe (Source, OsString))
genOk =
    Hedgehog.Gen.maybe $ (,) <$> genSource <*> genOsString

genErr :: Gen DummyParseError
genErr = Hedgehog.Gen.choice [Hedgehog.Gen.constant Dummy1, Hedgehog.Gen.constant Dummy2]

genResult :: Gen (Result DummyParseError (Maybe (Source, OsString)))
genResult = Hedgehog.Gen.choice [Err <$> genErr, Ok <$> genOk]

genParseResult :: Gen (ParseResult DummyParseError OsString)
genParseResult = ParseResult <$> genResult

testParseResult :: Spec
testParseResult =
    let
        testShow :: SpecWith ()
        testShow =
            let
                ok :: ParseResult DummyParseError Int
                ok = ParseResult (Ok Nothing)
             in
                describe "Show" $ do
                    it "ok"
                        $ show [ok]
                        == "[ParseResult Ok (Nothing)]"

        testEq :: SpecWith ()
        testEq =
            it "Eq (ParseResult e a)" $ do
                Test.Hspec.Hedgehog.hedgehog $ propertyCheckEq genParseResult
     in
        describe "ParseResult" $ do
            testShow
            testEq

testFail :: SpecWith ()
testFail =
    let
        actualParser :: Parser DummyParseError ()
        actualParser = fail Dummy1
     in
        it "fail"
            $ runParser actualParser System.OsString.empty
            `shouldBe` fromResult (Err Dummy1)

testEndOfInput :: SpecWith ()
testEndOfInput =
    let
        parser :: Parser DummyParseError ()
        parser = endOfInput
     in
        describe "endOfInput" $ do
            it "empty"
                $ runParser parser System.OsString.empty
                `shouldBe` fromResult (Ok $ Just ([], ()))
            it "not empty"
                $ runParser parser (System.OsString.unsafeEncodeUtf "...")
                `shouldBe` fromResult (Ok Nothing)

testOsString :: SpecWith ()
testOsString =
    let
        dotDotDot :: OsString
        dotDotDot = System.OsString.unsafeEncodeUtf "..."

        hello :: OsString
        hello = System.OsString.unsafeEncodeUtf "Hello"

        world :: OsString
        world = System.OsString.unsafeEncodeUtf " World!"

        helloWorld :: OsString
        helloWorld = hello <> world

        parser :: Parser DummyParseError OsString
        parser = osString hello
     in
        describe "osString" $ do
            it "empty"
                $ runParser parser System.OsString.empty
                `shouldBe` fromResult (Ok Nothing)
            it "not empty"
                $ runParser parser dotDotDot
                `shouldBe` fromResult (Ok Nothing)
            it "Hello"
                $ runParser parser hello
                `shouldBe` fromResult (Ok $ Just ([], hello))
            it "Hello World"
                $ runParser parser helloWorld
                `shouldBe` fromResult (Ok $ Just (System.OsString.unpack world, hello))

testOsChar :: SpecWith ()
testOsChar =
    let
        a :: OsChar
        a = System.OsString.unsafeFromChar 'a'

        b :: OsChar
        b = System.OsString.unsafeFromChar 'b'

        ab :: OsString
        ab = System.OsString.unsafeEncodeUtf "ab"

        parser :: Parser DummyParseError OsChar
        parser = osChar a
     in
        describe "osChar" $ do
            it "empty"
                $ runParser parser System.OsString.empty
                `shouldBe` fromResult (Ok Nothing)
            it "not empty"
                $ runParser parser (System.OsString.pack [b])
                `shouldBe` fromResult (Ok Nothing)
            it "a"
                $ runParser parser (System.OsString.pack [a])
                `shouldBe` fromResult (Ok $ Just ([], a))
            it "ab"
                $ runParser parser ab
                `shouldBe` fromResult (Ok $ Just ([b], a))

testAnyOsChar :: SpecWith ()
testAnyOsChar =
    let
        a :: OsChar
        a = System.OsString.unsafeFromChar 'a'

        b :: OsChar
        b = System.OsString.unsafeFromChar 'b'

        x :: OsChar
        x = System.OsString.unsafeFromChar 'x'

        xab :: OsString
        xab = System.OsString.unsafeEncodeUtf "xab"

        parser :: Parser DummyParseError OsChar
        parser = anyOsChar
     in
        describe "anyOsChar" $ do
            it "empty"
                $ runParser parser System.OsString.empty
                `shouldBe` fromResult (Ok Nothing)
            it "a"
                $ runParser parser (System.OsString.pack [a])
                `shouldBe` fromResult (Ok $ Just ([], a))
            it "b"
                $ runParser parser (System.OsString.pack [b])
                `shouldBe` fromResult (Ok $ Just ([], b))
            it "xab"
                $ runParser parser xab
                `shouldBe` fromResult (Ok $ Just ([a, b], x))

testFirstOf :: SpecWith ()
testFirstOf =
    let
        a :: OsChar
        a = System.OsString.unsafeFromChar 'a'

        b :: OsChar
        b = System.OsString.unsafeFromChar 'b'

        x :: OsChar
        x = System.OsString.unsafeFromChar 'x'

        ab :: OsString
        ab = System.OsString.unsafeEncodeUtf "ab"

        ba :: OsString
        ba = System.OsString.unsafeEncodeUtf "ba"

        xa :: OsString
        xa = System.OsString.unsafeEncodeUtf "xa"

        parser :: Parser DummyParseError OsChar
        parser = firstOf (osChar a) (osChar b)

        parserWithErrorLeft :: Parser DummyParseError OsChar
        parserWithErrorLeft = firstOf (fail Dummy2) (osChar a)

        parserWithErrorRight :: Parser DummyParseError OsChar
        parserWithErrorRight = firstOf (osChar a) (fail Dummy2)
     in
        describe "firstOf" $ do
            it "empty"
                $ runParser parser System.OsString.empty
                `shouldBe` fromResult (Ok Nothing)
            it "a"
                $ runParser parser (System.OsString.pack [a])
                `shouldBe` fromResult (Ok $ Just ([], a))
            it "b"
                $ runParser parser (System.OsString.pack [b])
                `shouldBe` fromResult (Ok $ Just ([], b))
            it "x"
                $ runParser parser (System.OsString.pack [x])
                `shouldBe` fromResult (Ok Nothing)
            it "ab"
                $ runParser parser ab
                `shouldBe` fromResult (Ok $ Just ([b], a))
            it "ba"
                $ runParser parser ba
                `shouldBe` fromResult (Ok $ Just ([a], b))
            it "xa"
                $ runParser parser xa
                `shouldBe` fromResult (Ok Nothing)
            it "error on left"
                $ runParser parserWithErrorLeft ab
                `shouldBe` fromResult (Err Dummy2)
            it "error on right - okay"
                $ runParser parserWithErrorRight ab
                `shouldBe` fromResult (Ok $ Just ([b], a))
            it "error on right - not okay"
                $ runParser parserWithErrorRight ba
                `shouldBe` fromResult (Err Dummy2)

testRepeat :: SpecWith ()
testRepeat =
    let
        a :: OsChar
        a = System.OsString.unsafeFromChar 'a'

        b :: OsChar
        b = System.OsString.unsafeFromChar 'b'

        aa :: OsString
        aa = System.OsString.unsafeEncodeUtf "aa"

        aaa :: OsString
        aaa = System.OsString.unsafeEncodeUtf "aaa"

        ba :: OsString
        ba = System.OsString.unsafeEncodeUtf "ba"

        ab :: OsString
        ab = System.OsString.unsafeEncodeUtf "ab"

        aab :: OsString
        aab = System.OsString.unsafeEncodeUtf "aab"

        aaab :: OsString
        aaab = System.OsString.unsafeEncodeUtf "aaab"

        parser :: Parser DummyParseError [OsChar]
        parser = repeat (osChar a)

        parserWithError :: Parser DummyParseError [OsChar]
        parserWithError = repeat (fail Dummy2)
     in
        describe "repeat" $ do
            it "empty"
                $ runParser parser System.OsString.empty
                `shouldBe` fromResult (Ok $ Just ([], []))
            it "a"
                $ runParser parser (System.OsString.pack [a])
                `shouldBe` fromResult (Ok $ Just ([], [a]))
            it "b"
                $ runParser parser (System.OsString.pack [b])
                `shouldBe` fromResult (Ok $ Just ([b], []))
            it "aa"
                $ runParser parser aa
                `shouldBe` fromResult (Ok $ Just ([], [a, a]))
            it "aaa"
                $ runParser parser aaa
                `shouldBe` fromResult (Ok $ Just ([], [a, a, a]))
            it "ba"
                $ runParser parser ba
                `shouldBe` fromResult (Ok $ Just ([b, a], []))
            it "ab"
                $ runParser parser ab
                `shouldBe` fromResult (Ok $ Just ([b], [a]))
            it "aab"
                $ runParser parser aab
                `shouldBe` fromResult (Ok $ Just ([b], [a, a]))
            it "aaab"
                $ runParser parser aaab
                `shouldBe` fromResult (Ok $ Just ([b], [a, a, a]))
            it "error"
                $ runParser parserWithError ab
                `shouldBe` fromResult (Err Dummy2)

testRepeat1 :: SpecWith ()
testRepeat1 =
    let
        a :: OsChar
        a = System.OsString.unsafeFromChar 'a'

        b :: OsChar
        b = System.OsString.unsafeFromChar 'b'

        aa :: OsString
        aa = System.OsString.unsafeEncodeUtf "aa"

        aaa :: OsString
        aaa = System.OsString.unsafeEncodeUtf "aaa"

        ba :: OsString
        ba = System.OsString.unsafeEncodeUtf "ba"

        ab :: OsString
        ab = System.OsString.unsafeEncodeUtf "ab"

        aab :: OsString
        aab = System.OsString.unsafeEncodeUtf "aab"

        aaab :: OsString
        aaab = System.OsString.unsafeEncodeUtf "aaab"

        parser :: Parser DummyParseError [OsChar]
        parser = repeat1 (osChar a)

        parserWithError :: Parser DummyParseError [OsChar]
        parserWithError = repeat1 (fail Dummy2)
     in
        describe "repeat1" $ do
            it "empty"
                $ runParser parser System.OsString.empty
                `shouldBe` fromResult (Ok Nothing)
            it "a"
                $ runParser parser (System.OsString.pack [a])
                `shouldBe` fromResult (Ok $ Just ([], [a]))
            it "b"
                $ runParser parser (System.OsString.pack [b])
                `shouldBe` fromResult (Ok Nothing)
            it "aa"
                $ runParser parser aa
                `shouldBe` fromResult (Ok $ Just ([], [a, a]))
            it "aaa"
                $ runParser parser aaa
                `shouldBe` fromResult (Ok $ Just ([], [a, a, a]))
            it "ba"
                $ runParser parser ba
                `shouldBe` fromResult (Ok Nothing)
            it "ab"
                $ runParser parser ab
                `shouldBe` fromResult (Ok $ Just ([b], [a]))
            it "aab"
                $ runParser parser aab
                `shouldBe` fromResult (Ok $ Just ([b], [a, a]))
            it "aaab"
                $ runParser parser aaab
                `shouldBe` fromResult (Ok $ Just ([b], [a, a, a]))
            it "error"
                $ runParser parserWithError ab
                `shouldBe` fromResult (Err Dummy2)

testOptionally :: SpecWith ()
testOptionally =
    let
        a :: OsChar
        a = System.OsString.unsafeFromChar 'a'

        b :: OsChar
        b = System.OsString.unsafeFromChar 'b'

        ab :: OsString
        ab = System.OsString.unsafeEncodeUtf "ab"

        ba :: OsString
        ba = System.OsString.unsafeEncodeUtf "ba"

        parser :: Parser DummyParseError (Maybe OsChar)
        parser = optionally (osChar a)

        parserWithError :: Parser DummyParseError (Maybe OsChar)
        parserWithError = optionally (fail Dummy2)
     in
        describe "optionally" $ do
            it "empty"
                $ runParser parser System.OsString.empty
                `shouldBe` fromResult (Ok $ Just ([], Nothing))
            it "a"
                $ runParser parser (System.OsString.pack [a])
                `shouldBe` fromResult (Ok $ Just ([], Just a))
            it "b"
                $ runParser parser (System.OsString.pack [b])
                `shouldBe` fromResult (Ok $ Just ([b], Nothing))
            it "ab"
                $ runParser parser ab
                `shouldBe` fromResult (Ok $ Just ([b], Just a))
            it "ba"
                $ runParser parser ba
                `shouldBe` fromResult (Ok $ Just ([b, a], Nothing))
            it "error"
                $ runParser parserWithError ab
                `shouldBe` fromResult (Err Dummy2)
