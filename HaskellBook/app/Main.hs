module Main where

import qualified MyLib (someFunc)
import qualified Cap02.MyFunctions as MyFunctions (someFunc,triple)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
  MyFunctions.someFunc
