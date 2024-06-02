{-# LANGUAGE OverloadedStrings #-}

module Test.Bayeux.L
  ( tests
  ) where

import Bayeux.L
import Bayeux.Tableaux
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup "R. Smullyan \"First-order Logic\"" $ mkTestCase <$> smullyan
  ]

mkTestCase :: (L Text, Bool, String) -> TestTree
mkTestCase (l, expected, description) =
  let display = unwords [description, T.unpack $ render l]
  in testGroup display
       [ testCase "prove" $ prove l @?= expected
--       , testCase "parse . pretty" $ (parseMaybe parse . render) lp @?= Just (lp)
       ]

smullyan :: [(L Text, Bool, String)]
smullyan =
  [ ( All "x" (Fun "P" ["x"] ==> Fun "Q" ["x"]) ==> All "x" (Fun "P" ["x"]) ==> All "x" (Fun "Q" ["x"])
    , True
    , "pg. 55 exp 1"
    )
  , ( Exist "y" (Exist "x" (Fun "P" ["x"] ==> Fun "P" ["y"]))
    , True
    , "pg. 55, 56 exp 2"
    )
  , ( All "y" (All "x" (Fun "P" ["x"] ==> Fun "P" ["y"]))
    , True
    , "pg 56 exr 1"
    )
  , ( All "x" (Fun "P" ["x"] ==> Exist "x" (Fun "P" ["x"]))
    , True
    , "pg 56 exr 2"
    )
  , ( Exist "y" (Fun "P" ["y"] ==> All "x" (Fun "P" ["x"]))
    , True
    , "pg 56 exr 3"
    )
  ]

--{-
testE :: L String
testE = Exist "y" (Fun "P" ["y"] ==> All "x" (Fun "P" ["x"]))

testT :: Tableaux (L (Node String))
testT = unfold 0 mempty $ S.singleton $ fmap Var $ Bar testE

renderTestT :: IO ()
renderTestT = putStrLn $ renderTableaux $ T.unpack . render <$> testT

testProof = prove testE
---}
