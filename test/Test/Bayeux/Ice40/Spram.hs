{-# LANGUAGE OverloadedStrings #-}

module Test.Bayeux.Ice40.Spram
  ( tests
  ) where

import Bayeux.Ice40.Spram
import Bayeux.Rtl
import Bayeux.Signal
import Prettyprinter
import System.FilePath
import Test.Bayeux.Rtl (prettyTest)
import Test.Tasty

tests :: [TestTree]
tests =
  [ testGroup "pretty"
      [ prettyTest' "spram" $ handleErr $ compile $ spram
        (Sig "14'00000000000000")
        (Sig "16'0000000000000000")
        (Sig "4'0000")
        (sig False)
        (sig False)
        (sig False)
        (sig False)
        (sig False)
--  spram :: Sig (Array (Finite 14) Bool) -- ^ address
--        -> Sig Word16                  -- ^ data in
--        -> Sig (Array (Finite 4) Bool) -- ^ mask write enable
--        -> Sig Bool                    -- ^ write enable
--        -> Sig Bool                    -- ^ chip select
--        -> Sig Bool                    -- ^ stand by
--        -> Sig Bool                    -- ^ sleep
--        -> Sig Bool                    -- ^ poweroff
--        -> m (Sig Word16)              -- ^ data out
      ]
  ]

handleErr :: Either Err File -> File
handleErr = either (error . show) id

prettyTest' :: Pretty a => TestName -> a -> TestTree
prettyTest' = prettyTest $ "test" </> "Test" </> "Bayeux" </> "Ice40" </> "Spram" </> "golden"
