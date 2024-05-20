{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bayeux
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Bayeux"
  [ lpTautology $ "~x" \/ "x"
  , -- smullyan FOL pg 24 ex 1
    lpTautology $ "q" ==> "p" ==> "q"
  ]

lpTautology :: Eq a => Show a => Lp a -> TestTree
lpTautology lp = testCase (prettyLp lp) $ assertBool "" $ prove lp
{-
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
  , ( "q" ==> ("p" ==> "q")
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
  , ( bar ("p" /\ "q") ==> (bar "p" \/ bar "q")
    , True
    , "pg. 24"
    )
  , ( bar ("p" \/ "q") ==> (bar "p" /\ bar "q")
    , True
    , "pg. 24"
    )
  , ( (bar "p" \/ bar "q") ==> Bar ("p" /\ "q")
    , True
    , "pg. 24"
    )
  , ( ("p" \/ "q") ==> ("p" /\ "q")
    , False
    , "pg. 28"
    )
  , ( "p" \/ bar "p"
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
-}
