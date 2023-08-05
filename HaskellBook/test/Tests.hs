module Main where


import Test.Tasty
import Test.Tasty.HUnit

import Cap02.MyFunctionsTest
import Cap06.MyTypesTest

misTest :: TestTree
misTest = testGroup "Mis test totales" [ misTestFuns
                                       , misTestTypes
                                       ]

main = defaultMain misTest