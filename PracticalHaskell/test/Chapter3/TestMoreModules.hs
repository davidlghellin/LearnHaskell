module Chapter3.TestMoreModules where


import Test.Tasty
import Test.Tasty.HUnit 

import Chapter3.MoreModules


misPermu :: TestTree
misPermu = testGroup "Mis test permutaciones" 
                       [ myPermuSuite
                       ]
myPermuSuite :: TestTree
myPermuSuite = testGroup "Mis permutaciones"
           [ testCase "Fijo 1 dos permuto" $ permutationsStartingWith 'a' "abc" @?= ["abc", "acb"]
           ]