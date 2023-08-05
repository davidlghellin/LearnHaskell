module Cap02.MyFunctionsTest where

import Test.Tasty
import Test.Tasty.HUnit
import Cap02.MyFunctions

misTestFuns :: TestTree
misTestFuns = testGroup "MistestTriple" [tripleSuite]

tripleSuite :: TestTree
tripleSuite = testGroup "Triple"
                [ testCase "Triple" $ triple 1 @?= 3
                ]