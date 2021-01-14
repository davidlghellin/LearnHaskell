module Otro.Web.TestTennis where

import Test.Tasty
import Test.Tasty.HUnit
import Otro.Web.Tennis

misTennisClass :: TestTree
misTennisClass = testGroup "Mis lentes" 
                       [ myTenisSuite
                       ]


myTenisSuite :: TestTree
myTenisSuite = testGroup "Tennis"
             [ testGroup "Empate"
                 [ testCase "Creacion" $ (show newGame)                                         @?= "Love - Love"
                 
                 , testCase "15-15"    $ (show . score A . score B) newGame                     @?= "15 - 15"
                 ]
             , testGroup "Gana A"
                 [ testCase "15-0"     $ (show . score A) newGame                               @?= "15 - Love"
                 , testCase "30-0"     $ (show . score A . score A) newGame                     @?= "30 - Love"
                 , testCase "40-0"     $ (show . score A . score A . score A) newGame           @?= "40 - Love"
                 , testCase "wins-0"   $ (show . score A . score A . score A . score A) newGame @?= "A Wins"
                 ]
             , testGroup "Gana B"
                 [ testCase "0-15"     $ (show . score B) newGame                               @?= "Love - 15"
                 , testCase "0-30"     $ (show . score B . score B) newGame                     @?= "Love - 30"
                 , testCase "0-40"     $ (show . score B . score B . score B) newGame           @?= "Love - 40"
                 , testCase "0-wins"   $ (show . score B . score B . score B . score B) newGame @?= "B Wins"
                 ]
             , testGroup "Contexto de empate"
                 [ testCase "Empate"    $ (show Deuce)                     @?= "Deuce" 
                 , testCase "ventaja A" $ (show . score A) Deuce           @?= "Advantage A"
                 , testCase "ventaja B" $ (show . score B) Deuce           @?= "Advantage B"
                 , testCase "wins A"    $ (show . score A . score A) Deuce @?= "A Wins"
                 , testCase "wins B"    $ (show . score B . score B) Deuce @?= "B Wins"
                 , testCase "Empata A"  $ (show . score B . score A) Deuce @?= "Deuce"
                 , testCase "Empata B"  $ (show . score A . score B) Deuce @?= "Deuce"
                 ]
             ]