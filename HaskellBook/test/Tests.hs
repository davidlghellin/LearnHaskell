module Main where


import Test.Tasty
import Test.Tasty.HUnit

import Cap02.MyFunctionsTest

misTest :: TestTree
misTest = testGroup "Mis test totales" [ misTestFuns
                                       ]

main = defaultMain misTest
