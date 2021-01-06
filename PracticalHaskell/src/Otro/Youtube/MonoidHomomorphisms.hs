module Otro.Youtube.MonoidHomomorphisms where

import Text.Printf
import Criterion.Main
import Data.Monoid
import Data.Foldable


-- MONOID HOMOMORPHISM
-- Monoid [a] -> Monoid Sum Int
wordCount :: String -> Sum Int
wordCount t =
  Sum $ length $ words t


page :: String -> Int -> String
page word numberOfTimes =
  unwords $ replicate numberOfTimes word


pageHello :: Int -> String
pageHello = page "Hello"


document :: [String]
document = replicate 1000 $ pageHello 1000


mapThenAddCounts :: [String] -> Int
mapThenAddCounts = getSum . foldMap wordCount


joinThenCount :: [String] -> Int
joinThenCount = getSum . wordCount . mconcat


main :: IO ()
main = do
  printf "The word count is %6i\n" $ getSum $ wordCount $ page "Hello" 1000
  defaultMain [
    bgroup "wordcount" [ bench "map then count" $ whnf mapThenAddCounts document -- menos tiempos
                       , bench "join then count" $ whnf joinThenCount document
                       ]
              ]
