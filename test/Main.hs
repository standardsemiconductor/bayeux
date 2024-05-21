{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bayeux
import Data.String
import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit
import Text.Megaparsec

main :: IO ()
main = defaultMain $ testGroup "Bayeux"
  [ testGroup "R. Smullyan \"First-order Logic\"" $ mkTestCase <$> smullyan
  , testGroup "Parse" parseTests
  ]

mkTestCase :: Eq a => Show a => (Lp a, Bool, String) -> TestTree
mkTestCase (lp, expected, description) =
  let display = unwords [description, prettyLp lp]
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
    mkParsePrettyTestCase (lp, _, _) = testCase (prettyLp lp) $ (parseMaybe parseLp . fromString . prettyLp . fmap fromString) lp @?= Just (fromString <$> lp)
{-
hedgehogTests :: [TestTree]
hedgehogTests =
  [ testProperty "parse . pretty = id" undefined
  ]

genName :: MonadGen m => m String
genName = Gen.string Gen.alphaNum
-}
