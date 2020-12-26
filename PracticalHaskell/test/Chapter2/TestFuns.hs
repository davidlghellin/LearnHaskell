module Chapter2.TestFuns where 

import Test.Tasty
import Test.Tasty.HUnit 

import Chapter2.Funs

misTestFuns :: TestTree
misTestFuns = testGroup "MistestSuma" [sumaSuite]

sumaSuite :: TestTree
sumaSuite = testGroup "Sumass"
                [ testCase "1 + 1" $ mySuma 1 1 @?= 2
                ]