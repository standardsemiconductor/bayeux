{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bayeux
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Bayeux"
  [ testGroup "R. Smullyan \"First-order Logic\"" $ mkTestCase <$> smullyan
  ]

mkTestCase :: Eq a => Show a => (Lp a, Bool, String) -> TestTree
mkTestCase (lp, expected, description) =
  let display = unwords [description, prettyLp lp]
  in testCase display $ prove lp @=? expected

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
