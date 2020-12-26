module Main where 

import Test.Tasty
import Test.Tasty.HUnit 

import Chapter2.TestFuns

misTest :: TestTree
misTest = testGroup "Mis test totales" [misTestFuns]

main = defaultMain misTest