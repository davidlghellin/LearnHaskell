module Main where 

import Test.Tasty
import Test.Tasty.HUnit 

import Chapter2.TestFuns
import Chapter2.TestMyData
import Chapter2.TestSimpleFunctions
import Chapter3.TestParamPoly
import Chapter3.TestMoreModules
import Chapter4.TestMyTypeClass
import Otro.Web.TestTennis
import Otro.Web.TestMaquinaEstados

misTest :: TestTree
misTest = testGroup "Mis test totales" [ misTestFuns
                                       , misTestMyData
                                       , misTestSimpleFunctions
                                       , misPolysh
                                       , misPermu
                                       , misTypeClass
                                       , myTenisSuite
                                       , miMaquinaClass
                                       ]

main = defaultMain misTest