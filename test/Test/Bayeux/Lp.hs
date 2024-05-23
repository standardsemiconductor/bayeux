{-# LANGUAGE OverloadedStrings #-}

module Test.Bayeux.Lp
  ( tests
  ) where

import Bayeux.Lp
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit
import Text.Megaparsec hiding (parse)

tests :: [TestTree]
tests =
  [ testGroup "R. Smullyan \"First-order Logic\"" $ mkTestCase <$> smullyan
  , testGroup "Parse" parseTests
  , testGroup "Hedgehog" hedgehogTests
  ]

mkTestCase :: (Lp Text, Bool, String) -> TestTree
mkTestCase (lp, expected, description) =
  let display = unwords [description, T.unpack $ render lp]
  in testGroup display
       [ testCase "prove" $ prove lp @?= expected
       , testCase "parse . pretty" $ (parseMaybe parse . render) lp @?= Just (lp)
       ]

smullyan :: [(Lp Text, Bool, String)]
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
  [ tc  "a"  "a"
  , tc "~a" "~a"
  , tc "a =>  b => c"  $ "a" ==> "b" ==> "c"
  , tc "a => (b => c)" $ "a" ==> "b" ==> "c"
  , tc "(a => b) => c" $ ("a" ==> "b") ==> "c"
  , tc "~a \\/ ~b" $ "~a" \/ "~b"
  ]
  where
    tc t lp = testCase (T.unpack t) $ parseMaybe parse t @?= Just lp

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
      (fromJust . parseMaybe parse . render) lp === lp
  ]
