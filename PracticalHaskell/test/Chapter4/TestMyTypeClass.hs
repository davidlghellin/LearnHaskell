module Chapter4.TestMyTypeClass where

import Test.Tasty
import Test.Tasty.HUnit 

import Chapter3.ParamPoly
import Chapter4.MyTypeClass


misTypeClass :: TestTree
misTypeClass = testGroup "Mis test type class" 
                       [ myTypeSuite
                       , myPriceableItemSuite
                       , myPriceableBookSuite
                       , myEqSuite
                       , mytreeFind1Suite
                       , myPolymorphicTreesSuite
                       ]
myTypeSuite :: TestTree
myTypeSuite = testGroup "Types clases"
           [ testCase "Individual nameable" $ name (Individual 1 (Person "David" "Lopez"))                 @?= "David-Lopez"
           , testCase "General nameable"    $ name (GovOrg 1 "DavidLopez")                                 @?= "DavidLopez"
           , testCase "Company nameable"    $ name (Company 1 "DavidLopez" (Person "David" "Lopez") "DLG") @?= "DavidLopez"
           ]

myPriceableItemSuite :: TestTree
myPriceableItemSuite = testGroup "Suma de precios Item"
                 [ testCase "Sumamos uno"   $ totalPrice ([Item "Carne" 3 3])                 @?= 9.0
                 , testCase "Sumamos 2List" $ totalPrice (Item "Carne" 3 3:[])                @?= 9.0
                 , testCase "Sumamos 2List" $ totalPrice (Item "Carne" 3 3:Item "pan" 1 1:[]) @?= 10.0
                 ]
myPriceableBookSuite :: TestTree
myPriceableBookSuite = testGroup "Suma de libros"
                 [ testCase "Un libro de Harry"  $ totalPrice ([Book "Harry" 33 "nº3"])                 @?= 33.0
                 ]
                
myEqSuite :: TestTree
myEqSuite = testGroup "Eq Type class"
           [ testCase "Genero Hombre OK" $ Hombre == Hombre @?= True
           , testCase "Genero Mujer  OK" $ Mujer  == Mujer  @?= True
           , testCase "Genero KO "       $ Hombre == Mujer  @?= False
           , testCase "Persona2 OK"      $ Persona2 "David" "Lopez" Hombre == Persona2 "David" "Lopez" Hombre @=? True
           , testCase "Persona2 KO"      $ Persona2 "David" "Lopes" Hombre == Persona2 "David" "Lopez" Hombre @=? False
           ]
                 
mytreeFind1Suite :: TestTree
mytreeFind1Suite = testGroup "Ejemplo travelGuide"
                 [ testCase "Ejemplo 1" $ TravelGuide "Harry" ["??"] 20.2                            @=? TravelGuide "Harry" ["??"] 20.2
                 , testCase "Ejemplo 2" $ show (TravelGuide "Harry" ["??"] 20.2)                     @=? "TravelGuide {title = \"Harry\", autors = [\"??\"], price = 20.2}"
                 , testCase "Ejemplo 3" $ show (Node1 (TravelGuide "Harry" ["??"] 20.2) Leaf1 Leaf1) @=? "Node1 (TravelGuide {title = \"Harry\", autors = [\"??\"], price = 20.2}) Leaf1 Leaf1"
                 
                 , testCase "Buscar fallida" $ treeFind1 (TravelGuide "Harry" ["??"] 20.2) (Node1 (TravelGuide "Otro" ["??","aa"] 20.2) Leaf1 Leaf1) @=? Nothing
                 , testCase "Buscar OK"      $ treeFind1 (TravelGuide "Harry" ["??"] 20.2) (Node1 (TravelGuide "Harry" ["??"] 20.2) Leaf1 Leaf1)     @=? Just (TravelGuide "Harry" ["??"] 20.2)
                 -- creamos la instancia de Eq 
                 , testCase "Comparacion hojas1" $ (==) Leaf1 Leaf1                                                 @?= True
                 , testCase "Comparacion hojas2" $ (==) Leaf1 (Node1 (TravelGuide "Harry" ["??"] 20.2) Leaf1 Leaf1) @?= False
                 , testCase "Comparacion hojas3" $ (==) (Node1 (TravelGuide "Harry" ["??"] 20.2) Leaf1 Leaf1) Leaf1 @?= False
                 , testCase "Comparacion hojas4" $ (==) (Node1 (TravelGuide "Harry" ["??"] 20.2) Leaf1 Leaf1) (Node1 (TravelGuide "Harry" ["??"] 20.2) Leaf1 Leaf1) @?= True

                 , testCase "Insert show"    $ show (treeInsert1 (TravelGuide "Practical" ["Haskell"] 20.2) Leaf1) @=? "Node1 (TravelGuide {title = \"Practical\", autors = [\"Haskell\"], price = 20.2}) Leaf1 Leaf1"
                 , testCase "Insert"         $ treeInsert1 (TravelGuide "Practical" ["Haskell"] 20.2) Leaf1        @=? Node1 (TravelGuide {title = "Practical", autors = ["Haskell"], price = 20.2}) Leaf1 Leaf1
                 ]

myPolymorphicTreesSuite :: TestTree
myPolymorphicTreesSuite = testGroup "Alboles polymorphic"
                        [ testCase "Buscar entero KO" $ treeFind2 (2) (Node2 3 Leaf2 Leaf2)     @=? Nothing
                        , testCase "Buscar entero OK" $ treeFind2 (2) (Node2 2 Leaf2 Leaf2)     @=? Just 2
                        , testCase "Buscar char KO"   $ treeFind2 ("2") (Node2 "3" Leaf2 Leaf2) @=? Nothing
                        , testCase "Buscar char OK"   $ treeFind2 ("2") (Node2 "2" Leaf2 Leaf2) @=? Just "2"
                        ]