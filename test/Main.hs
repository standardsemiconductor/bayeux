{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bayeux.Lp
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Bayeux.L  as L
import qualified Test.Bayeux.Lp as Lp
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit
import Text.Megaparsec hiding (parse)

main :: IO ()
main = defaultMain $ testGroup "Bayeux"
  [ testGroup "Lp" Lp.tests
  , testGroup "L"  L.tests
  ]

