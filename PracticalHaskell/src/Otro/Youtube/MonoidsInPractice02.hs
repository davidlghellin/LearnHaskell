module Otro.Youtube.MonoidsInPractice02 where

import Text.Printf

data OrderLine = ProductLine { productCode :: String, productQuantity :: Int,price :: Float, lineTotal :: Float}
               | TotalLine   { totalQuantity :: Int, orderTotal :: Float} -- conjunto monoidal
               | EmptyLine


instance Show OrderLine where
    show (ProductLine plCode plQuantity plPrice plTotal) =
      printf "%-10s %5i @%4g each %6g" plCode plQuantity plPrice plTotal
    show (TotalLine tlQuantity tlTotal) = 
      printf "%-10s %5i            %6g" "TOTAL" tlQuantity tlTotal
    
--definicion monoid
{-|
class Monoid m where
    mempty  :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty
-}


instance Monoid OrderLine where
    mempty  = EmptyLine
    mappend = addLine

-- Hemos tenido que poner tb Semigrupo porque salia error
instance Semigroup OrderLine where
    (<>) = addLine 

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


-- runhaskell src/Otro/Youtube/MonoidsInPractice02.hs
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
      subtotal = mconcat sampleLines
      bigtotal = mconcat $ moreSampleLines ++ [subtotal]