{-# LANGUAGE OverloadedStrings #-}

module Test.Bayeux.L
  ( tests
  ) where

import Bayeux.L
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
    , "pg. 55, ex 1"
    )
--  , ( Exist "y" (Exist "x" (Fun "P" ["x"] ==> Fun "P" ["y"]))
--    , True
--    , "pg. 55, 56 ex2"
--    )
--  , ( All "y" (All "x" (Fun "P" ["x"] ==> Fun "P" ["y"]))
--    , True -- ?
--    , "pg 56. Ex 1"
--    )
--  , ( All "x" (Fun "P" ["x"] ==> Exist "x" (Fun "P" ["x"]))
--    , True
--    , "pg 56. Ex2"
--    )
  ]
