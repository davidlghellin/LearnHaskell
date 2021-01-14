module Otro.Web.TestMaquinaEstados where


import Test.Tasty
import Test.Tasty.HUnit
import Otro.Web.MaquinaEstados

miMaquinaClass :: TestTree
miMaquinaClass = testGroup "Mi Maquina estados" 
                       [ myMaquinaEstadosSuite
                       ]

myMaquinaEstadosSuite :: TestTree
myMaquinaEstadosSuite = testGroup "Maquina"
                 [ testCase "Creacion" $ (show crearMaquinaEstado)                                                 @?= "A"
                 , testCase "A-B"      $ (show . mover Forward)  crearMaquinaEstado                                @?= "B"
                 , testCase "A-B-A"    $ (show . mover Forward . mover Back . mover Forward ) crearMaquinaEstado   @?= "B"
                 , testCase "A-B-C"    $ (show . mover Forward . mover Forward ) crearMaquinaEstado                @?= "C"
                 , testCase "D-C"      $ (show . mover Back ) Fin                                                  @?= "C"
                 , testCase "Reset"    $ (show . mover Reset . mover Forward . mover Forward ) crearMaquinaEstado  @?= "A"
                 ]