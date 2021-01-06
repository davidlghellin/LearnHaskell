module Otro.Youtube.MonoidsInPractice where

import Text.Printf

data OrderLine = OrderLine { productCode :: String
                           , quantity    :: Int
                           , total       :: Float
                           }

instance Show OrderLine where
    show orderLine = 
        printf "%-10s %5i %6g" olCode olQuantity olTotal
        where
            OrderLine olCode olQuantity olTotal = orderLine

sampleLines :: [OrderLine]
sampleLines = 
    [ OrderLine { productCode = "AAA", quantity = 2, total = 2.99 }
    , OrderLine { productCode = "BBB", quantity = 1, total = 1.99 }
    , OrderLine { productCode = "CCC", quantity = 3, total = 3.99 }
    ]
moreSampleLines :: [OrderLine]
moreSampleLines = 
    [ OrderLine { productCode = "DDD", quantity = 4, total = 4.99 }
    , OrderLine { productCode = "BBB", quantity = 1, total = 1.99 }
    , OrderLine { productCode = "EEE", quantity = 2, total = 2.00 }
    ]

addLine :: OrderLine -> OrderLine -> OrderLine
addLine (OrderLine "" _ _) line = line
addLine line (OrderLine "" _ _) = line
addLine line1 line2 =
    OrderLine { productCode = "TOTAL"
              , quantity    = quantity line1 + quantity line2
              , total       = total line1 + total line2
              }

emptyLine :: OrderLine
emptyLine = OrderLine "" 0 0
-- como tenenmos ya lo que tiene el monoid podemos hacer el fold
-- elemento neutro
-- suma de dos elementos de 'a' que da otro elemento de 'a' --> cerrada
-- asociativo
totalLine :: OrderLine
totalLine =  foldr addLine emptyLine sampleLines

-- pasamos un array para que calcule el total
totalLine1 :: [OrderLine] -> OrderLine
totalLine1 =  foldr addLine emptyLine

-- runhaskell src/Otro/Youtube/MonoidsInPractice.hs
main :: IO ()
main = do 
    putStrLn "--------"
    -- print totalLine -- funcion vieja
    mapM_ print sampleLines
    putStrLn "--------"
    print subtotal
    putStrLn "--------"
    mapM_ print moreSampleLines
    putStrLn "--------"
    print bigtotal
    where
      subtotal = totalLine1 sampleLines
      bigtotal = totalLine1 $ moreSampleLines ++ [subtotal]