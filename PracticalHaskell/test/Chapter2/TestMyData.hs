module Chapter2.TestMyData where 

import Test.Tasty
import Test.Tasty.HUnit 

import Chapter2.MyData

misTestMyData :: TestTree
misTestMyData = testGroup "Mis test MyData" [ nombreCompanyiaSuite
                                            , nombreClienteSuite
                                            ]

nombreClienteSuite :: TestTree
nombreClienteSuite = testGroup "Nombre Cliente"
                [ testCase "Nombre Org"     $ nombreCliente (Org "David S.L.")                                            @?= "David S.L."
                , testCase "Nombre Company" $ nombreCliente (Company "Empresa" 12  (Persona "david" "lopez" Hombre) "ok") @?= "Empresa"
                , testCase "Nombre Persona" $ nombreCliente (Individual (Persona "David" "Lopez" Hombre ) True)           @?= "David Lopez" 
                ]

nombreCompanyiaSuite :: TestTree
nombreCompanyiaSuite = testGroup "Nombre companyia"
                [ testCase "Companyia Nothing" $ nombreCompanyia (Org "David S.L.")                                               @?= Nothing
                , testCase "Companyia Nothing" $ nombreCompanyia (Individual (Persona "David" "Lopez" Hombre ) True)              @?= Nothing
                , testCase "Empresa Just"      $ nombreCompanyia (Company "Empresa" 12  (Persona "david" "lopez" Hombre) "S.L.")  @?= Just "Empresa"
                ]