module Chapter3.TestParamPoly where

import Test.Tasty
import Test.Tasty.HUnit 

import Chapter3.ParamPoly

misPolysh :: TestTree
misPolysh = testGroup "Mis test ploy" 
                       [ myFstSuite
                       , myClientPolySuite 
                       , mySuccSuite
                       , myApply3f2Suite
                       , sayHelloSuite
                       , multplyByNSuite
                       , filterOnesSuite
                       ]
myFstSuite :: TestTree
myFstSuite = testGroup "mis func poly"
           [ testCase "Tupla int" $ myFst (1,2)     @?= 1
           , testCase "Tupla str" $ myFst ("1","b") @?= "1" 
           , testCase "Tupla **2" $ myFst ((1,2),"b") @?= (1,2) 
           ]

myClientPolySuite :: TestTree
myClientPolySuite = testGroup "cliente poly"
             [ testCase "Show Persona"      $ show (Person "David" "Lopez")                @?= "Person {firstName = \"David\", lastName = \"Lopez\"}"
             , testCase "Individual int"    $ show (Individual 1 (Person "David" "Lopez")) @?= "Individual {clientId = 1, person = Person {firstName = \"David\", lastName = \"Lopez\"}}"
             , testCase "Show Cliente char" $ show (GovOrg 'n' "EU") @?= "GovOrg {clientId = 'n', clienteName = \"EU\"}"
             , testCase "Show Cliente int"  $ show (GovOrg 1 "EU")   @?= "GovOrg {clientId = 1, clienteName = \"EU\"}"
             ]

mySuccSuite :: TestTree
mySuccSuite = testGroup "cliente poly"
             [ testCase "Suma int 1" $ mySuccInt 2 @?= 3
             ]

myApply3f2Suite :: TestTree
myApply3f2Suite = testGroup "Funcion de 3f(x + 2)"
             [ testCase "succ"   $ apply3f2 succ 7 @?= 30
             , testCase "lambda" $ apply3f2 (\x -> x + 2) 8 @?= 36
             ]

sayHelloSuite :: TestTree
sayHelloSuite = testGroup "Saludos"
              [ testCase "Saluda David" $ sayHello  ["David"] @?= ["Hola David"]
              , testCase "Saluda Cesar" $ sayHello  ["Cesar"] @?= ["Welcome Cesar"]
              , testCase "Saluda David" $ sayHello2 ["David"] @?= ["Hola David"]
              , testCase "Saluda Cesar" $ sayHello2 ["Cesar"] @?= ["Welcome Cesar"]
              ]


multplyByNSuite :: TestTree
multplyByNSuite = testGroup "Funcion de multiplicar por n"
             [ testCase "Mult [1,2,3] por 5" $ map (multplyByN 5) [1,2,3] @?= [5, 10, 15]
             ]

filterOnesSuite :: TestTree
filterOnesSuite = testGroup "Ejer"
             [ testCase "Filtros de 1"   $ filterOnes [1,2,3,1]              @?= [1,1]
             , testCase "Filtros de 1"   $ filterNumber 3 [1,2,3,1]          @?= [3]
             , testCase "Filtros de not" $ filterNot (\x-> x == 1) [1,2,3,1] @?= [2,3]
             ]
