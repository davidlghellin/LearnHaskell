module Otro.Youtube.T02FuncionsLists where

import Data.Char
import Data.List

-- Queremos tener el conteo de cada caracter en mayusculas
run :: String -> [(Char, Int)]
run = display . group . sort . canonical 
-- la funcion anterior se ha desgranado en funciones pequeñas y se ha ido componiendo
-- vamos de derecha a izquierda, por lo que los tipos encajan hasta llegar al final

-- λ> :t sort
-- sort :: Ord a => [a] -> [a]
-- λ> :t group
-- group :: Eq a => [a] -> [[a]]

canonical :: String -> String --[Char]
canonical = filter (/= ' ') .map normalise 

normalise :: Char -> Char
normalise c | isUpper c = c
            | isLower c = toUpper c
            | otherwise = ' '

display :: [[a]] -> [(a, Int)]
display = map (\x -> (head x, length x))