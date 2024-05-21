{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bayeux
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import Prettyprinter
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit
import Text.Megaparsec

main :: IO ()
main = defaultMain $ testGroup "Bayeux"
  [ testGroup "R. Smullyan \"First-order Logic\"" $ mkTestCase <$> smullyan
  , testGroup "Parse" parseTests
  , testGroup "Hedgehog" hedgehogTests
  ]

mkTestCase :: Eq a => Pretty a => (Lp a, Bool, String) -> TestTree
mkTestCase (lp, expected, description) =
  let display = unwords [description, T.unpack $ prettyLp lp]
  in testCase display $ prove lp @?= expected

smullyan
  :: [(Lp String, Bool, String)]
smullyan =
  [ ( ("p" ==> ("q" ==> "r")) ==> (("p" ==> "q") ==> ("p" ==> "r"))
    , True
    , "pg. 19"
    )
  , ( ("p" \/ ("q" /\ "r")) ==> (("p" \/ "q") /\ ("p" \/ "r"))
    , True
    , "pg. 16"
    )
  , ( "q" ==> "p" ==> "q"
    , True
    , "pg. 24"
    )
  , ( (("p" ==> "q") /\ ("q" ==> "r")) ==> ("p" ==> "r")
    , True
    , "pg. 24"
    )
  , ( (("p" ==> "q") /\ ("p" ==> "r")) ==> ("p" ==> ("q" /\ "r"))
    , True
    , "pg. 24"
    )
  , ( ((("p" ==> "r") /\ ("q" ==> "r")) /\ ("p" \/ "q")) ==> "r"
    , True
    , "pg. 24"
    )
  , ( Bar ("p" /\ "q") ==> ("~p" \/ "~q")
    , True
    , "pg. 24"
    )
  , ( Bar ("p" \/ "q") ==> ("~p" /\ "~q")
    , True
    , "pg. 24"
    )
  , ( ("~p" \/ "~q") ==> Bar ("p" /\ "q")
    , True
    , "pg. 24"
    )
  , ( ("p" \/ "q") ==> ("p" /\ "q")
    , False
    , "pg. 28"
    )
  , ( "p" \/ "~p"
    , True
    , "Law of excluded middle"
    )
  , ( ("p" ==> "q") ==> ("~q" ==> "~p")
    , True
    , "Contraposition"
    )
  , ( (("p" ==> "q") ==> "p") ==> "p"
    , True
    , "Peirce's law"
    )
  ]

parseTests :: [TestTree]
parseTests =
  [ testCase "" $ parseMaybe parseLp "a"  @?= Just "a"
  , testCase "" $ parseMaybe parseLp "~a" @?= Just "~a"
  , testGroup "Smullyan parse after pretty" $ mkParsePrettyTestCase <$> smullyan
  ]
  where
    mkParsePrettyTestCase (lp, _, _) = testCase (T.unpack $ prettyLp lp) $ (parseMaybe parseLp . prettyLp) lp @?= Just (fromString <$> lp)

genName :: MonadGen m => m Text
genName = Gen.text (Range.linear 1 10) Gen.alphaNum

genLp :: MonadGen m => m (Lp Text)
genLp = Gen.recursive Gen.choice [Bv <$> genName]
  [ Gen.subterm  genLp       Bar
  , Gen.subterm2 genLp genLp Conj
  , Gen.subterm2 genLp genLp Disj
  , Gen.subterm2 genLp genLp Impl
  ]

hedgehogTests :: [TestTree]
hedgehogTests =
  [ testProperty "parse . pretty = id" $ property $ do
      lp <- forAll genLp
      (fromJust . parseMaybe parseLp . prettyLp) lp === lp
  ]
