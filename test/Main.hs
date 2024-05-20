{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bayeux
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Bayeux"
  [ lpTautology $ Disj (Bar "x") "x"
  , -- smullyan FOL pg 24 ex 1
    lpTautology $ Impl "q" $ Impl "p" "q"
  ]

lpTautology :: Eq a => Show a => Lp a -> TestTree
lpTautology lp = testCase (prettyLp lp) $ assertBool "" $ prove lp
