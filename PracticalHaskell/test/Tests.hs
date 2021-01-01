module Main where 

import Test.Tasty
import Test.Tasty.HUnit 

import Chapter2.TestFuns
import Chapter2.TestMyData
import Chapter2.TestSimpleFunctions
import Chapter3.TestParamPoly
import Chapter3.TestMoreModules
import Chapter4.TestMyTypeClass

misTest :: TestTree
misTest = testGroup "Mis test totales" [ misTestFuns
                                       , misTestMyData
                                       , misTestSimpleFunctions
                                       , misPolysh
                                       , misPermu
                                       , misTypeClass
                                       ]

main = defaultMain misTest