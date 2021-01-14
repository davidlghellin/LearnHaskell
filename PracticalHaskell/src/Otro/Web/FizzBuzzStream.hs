module Otro.Web.FizzBuzzStream where

import Data.Maybe
-- https://blog.ploeh.dk/2019/12/30/semigroup-resonance-fizzbuzz/

fizzes :: [Maybe String]
fizzes = cycle [Nothing, Nothing, Just "Fizz"]

buzzes :: [Maybe String]
buzzes = cycle [Nothing, Nothing, Nothing, Nothing, Just "Buzz"]

-- zipWith :: (a->b->c) -> [a]->[b]->[c]
-- zipWith f = go
--   where
--     go [] _ = []
--     go _ [] = []
--     go (x:xs) (y:ys) = f x y : go xs ys
fizzBuzzes :: [Maybe String]
fizzBuzzes = zipWith (<>) fizzes buzzes


numbers :: [String]
numbers = show <$> [1..100]

-- The fromMaybe function takes a default value and and Maybe value. If the Maybe is Nothing, it returns the default values; otherwise, it returns the value contained in the Maybe.
-- fromMaybe     :: a -> Maybe a -> a
-- fromMaybe d x = case x of {Nothing -> d;Just v  -> v}

elements :: [String]
elements = zipWith fromMaybe numbers fizzBuzzes

main :: IO ()
main =
  mapM_ putStrLn $
  zipWith fromMaybe (show <$> [1..100]) $
  zipWith
    (<>)
    (cycle [Nothing, Nothing, Just "Fizz"])
    (cycle [Nothing, Nothing, Nothing, Nothing, Just "Buzz"])


