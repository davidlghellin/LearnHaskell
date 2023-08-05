module Cap06.MyTypesTest where

import Test.Tasty
import Test.Tasty.HUnit
import Cap06.MyTypes



misTestTypes :: TestTree
misTestTypes = testGroup "Mis Test de Type Class" [ dayWeekSuite
                                                  , dateSuite
                                                  , identitySuite
                                                  , ejer06EqSuite
                                                  ]
dayWeekSuite :: TestTree
dayWeekSuite = testGroup "Day Eq"
                [ testCase "Mon"  $ (==) Mon Mon   @?= True
                , testCase "Tue"  $ (==) Tue Tue   @?= True
                , testCase "Weds" $ (==) Weds Weds @?= True
                , testCase "Thu"  $ (==) Thu Thu   @?= True
                , testCase "Fri"  $ (==) Fri Fri   @?= True
                , testCase "Sat"  $ (==) Sat Sat   @?= True
                , testCase "Fals" $ (==) Mon Thu   @?= False
                ]

dateSuite :: TestTree
dateSuite = testGroup "Date Eq"
                [ testCase "Mon"  $ (==) (Date Sun 1) (Date Sun 1) @?= True
                , testCase "Fal"  $ (==) (Date Mon 1) (Date Mon 2) @?= False
                , testCase "Otro" $ Date Thu 10 == Date Thu 10     @?= True
                ]

identitySuite :: TestTree
identitySuite = testGroup "Identity"
              [ testCase "Sunday" $ Identity Mon == Identity Mon @?= True
              ]

ejer06EqSuite :: TestTree
ejer06EqSuite = testGroup "Ejer Eq"
              [ testGroup "TisAnInteger" 
                [ testCase "True " $ (==) (TisAn 1) (TisAn 1) @?= True
                , testCase "False" $ (==) (TisAn 2) (TisAn 1) @?= False
                ]
              , testGroup "TwoIntegers" 
                [ testCase "True " $ (==) (Two 1 1) (Two 1 1) @?= True
                , testCase "False" $ (==) (Two 2 1) (Two 1 1) @?= False
                ]
              , testGroup "StringOrInt" 
                [ testCase "True  Int" $ (==) (TisAnInt 1 ) (TisAnInt 1) @?= True
                , testCase "False Int" $ (==) (TisAnInt 1 ) (TisAnInt 2) @?= False 
                , testCase "True  Int" $ (==) (TisAString "1") (TisAString "1") @?= True
                , testCase "False Int" $ (==) (TisAString "1") (TisAString "2") @?= False
                ]
              , testGroup "Pair" 
                [ testCase "True  " $ (==) (Pair 1 1) (Pair 1 1) @?= True
                , testCase "False " $ (==) (Pair 1 2) (Pair 2 1) @?= False 
                ]
              , testGroup "Tuple" 
                [ testCase "True  " $ (==) (Tuple 1 2) (Tuple 1 2) @?= True
                , testCase "False " $ (==) (Tuple 1 2) (Tuple 2 1) @?= False 
                ]
              , testGroup "Which" 
                [ testCase "True  " $ (==) (ThisOne 1) (ThisOne 1) @?= True
                , testCase "True  " $ (==) (ThatOne 1) (ThatOne 1) @?= True
                , testCase "False " $ (==) (ThisOne 1) (ThisOne 2) @?= False 
                , testCase "False " $ (==) (ThatOne 1) (ThisOne 2) @?= False 
                , testCase "False " $ (==) (ThatOne 1) (ThatOne 2) @?= False 
                ]
              ]
 
