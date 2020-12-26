module Chapter2.TestSimpleFunctions where 

import Test.Tasty
import Test.Tasty.HUnit 

import Chapter2.SimpleFunctions

misTestSimpleFunctions :: TestTree
misTestSimpleFunctions = testGroup "Mis test SimpleFunctions" 
                       [ firstOrEmptySuite
                       , concatenarSuite
                       , reverse2Suite
                       ]

----------------------------------------------------------------------------------------------
firstOrEmptySuite :: TestTree
firstOrEmptySuite = testGroup "firstOrEmpty"
                  [ testCase "Lista vacia" $ firstOrEmpty ([])                      @?= "empty"
                  , testCase "Lista datos" $ firstOrEmpty (["1","2"])               @?= "1"
                  , testCase "Lista datos" $ firstOrEmpty (["hola", "qué", "tal?"]) @?= "hola"
                  ]

concatenarSuite :: TestTree
concatenarSuite = testGroup "(+++)"
                [ testCase "Listas vacias"    $ null ([] +++ [])    @?= True
                , testCase "Listas vacia 1"   $ []  +++ [1]         @?= [1]
                , testCase "Listas vacia 2"   $ [1] +++ []          @?= 1:[]
                , testCase "Listas no vacias" $ (1:1:[]) +++ (1:[]) @?= 1:1:1:[]
                ]

reverse2Suite :: TestTree
reverse2Suite =  testGroup "reverse2"
              [ testCase "Lista vacia" $ null (reverse2 ([])) @?= True
              , testCase "Lista 1 ele" $ reverse2 ([1])       @?= 1:[]
              , testCase "Lista varia" $ reverse2 ([1,2,3,4]) @?= 4:3:2:1:[]
              , testCase "Lista Chars" $ reverse2 ("Hola")    @?= "aloH"
              ]