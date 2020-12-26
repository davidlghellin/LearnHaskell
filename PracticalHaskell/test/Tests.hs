module Main where 

import Test.Tasty
import Test.Tasty.HUnit 

import Chapter2.TestFuns
import Chapter2.TestMyData
import Chapter2.TestSimpleFunctions

misTest :: TestTree
misTest = testGroup "Mis test totales" [ misTestFuns
                                       , misTestMyData
                                       , misTestSimpleFunctions
                                       ]

main = defaultMain misTest