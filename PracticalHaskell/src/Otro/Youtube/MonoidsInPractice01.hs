module Otro.Youtube.MonoidsInPractice01 where

import Text.Printf

data OrderLine = ProductLine { productCode :: String, productQuantity :: Int,price :: Float, lineTotal :: Float}
               | TotalLine   { totalQuantity :: Int, orderTotal :: Float} -- conjunto monoidal
               | EmptyLine


instance Show OrderLine where
    show (ProductLine plCode plQuantity plPrice plTotal) =
      printf "%-10s %5i @%4g each %6g" plCode plQuantity plPrice plTotal
    show (TotalLine tlQuantity tlTotal) = 
      printf "%-10s %5i            %6g" "TOTAL" tlQuantity tlTotal
    
        

sampleLines :: [OrderLine]
sampleLines = 
    [ ProductLine { productCode = "AAA", productQuantity = 2, price = 2.99, lineTotal = 5.98 }
    , ProductLine { productCode = "BBB", productQuantity = 1, price = 1.99, lineTotal = 1.99 }
    , ProductLine { productCode = "CCC", productQuantity = 3, price = 3.99, lineTotal = 11.97}
    ]
moreSampleLines :: [OrderLine]
moreSampleLines = 
    [ ProductLine { productCode = "DDD", productQuantity = 4, price = 4.99, lineTotal = 19.96}
    , ProductLine { productCode = "BBB", productQuantity = 1, price = 1.99, lineTotal = 1.99 }
    , ProductLine { productCode = "EEE", productQuantity = 2, price = 2.00, lineTotal = 4.00 }
    ]

addLine :: OrderLine -> OrderLine -> OrderLine
addLine EmptyLine line = line
addLine line EmptyLine = line
addLine (ProductLine _ plQuantity _ plTotal) (TotalLine tlQuantity tlTotal) =
    TotalLine { totalQuantity = plQuantity + tlQuantity
              , orderTotal    = plTotal + tlTotal
              }
addLine (TotalLine tlQuantity tlTotal) (ProductLine _ plQuantity _ plTotal) =
    TotalLine { totalQuantity = plQuantity + tlQuantity
              , orderTotal    = plTotal + tlTotal
              }
addLine (TotalLine tlQuantity1 tlTotal1) (TotalLine tlQuantity2 tlTotal2)  =
    TotalLine { totalQuantity = tlQuantity1 + tlQuantity2
              , orderTotal    = tlTotal1 + tlTotal2
              }
addLine line1 line2 = 
    TotalLine { totalQuantity = productQuantity line1 + productQuantity line2
              , orderTotal    = lineTotal line1 + lineTotal line2
              }


-- como tenenmos ya lo que tiene el monoid podemos hacer el fold
-- elemento neutro
-- suma de dos elementos de 'a' que da otro elemento de 'a' --> cerrada
-- asociativo
totalLine :: OrderLine
totalLine =  foldr addLine EmptyLine sampleLines

-- pasamos un array para que calcule el total
totalLine1 :: [OrderLine] -> OrderLine
totalLine1 =  foldr addLine EmptyLine

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