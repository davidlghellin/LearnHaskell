module Otro.Youtube.MonoidsInPractice03 where

import Prelude hiding (mapM_)
import Text.Printf
import Data.Monoid
import Data.Foldable


-- otros datos
data OrderLine = OrderLine { productCode :: String, productQuantity :: Int, price :: Float, lineTotal :: Float }
-- conjunto monoidal
data TotalLine = TotalLine { totalQuantity :: Int, orderTotal :: Float }
{- 
Se puede usar cuando:
  Tengamos una estructura de datos, que podamos sacar una substructura que tenga estructura de Monoide
  - definas el monoide con ella
  - definir el elemento neutro 
  - y como se agregan
-}
instance Monoid TotalLine where
  mempty = TotalLine 0 0
  -- como tenemos semigrupo no implementamos aqui
  -- mappend line1 line2 =
  --   TotalLine { totalQuantity = totalQuantity line1 + totalQuantity line2
  --             , orderTotal    = orderTotal    line1 + orderTotal line2
  --             }
instance Semigroup TotalLine where
    (<>) line1 line2 =  -- sumaremos TotalLine 
         TotalLine { totalQuantity = totalQuantity line1 + totalQuantity line2
                   , orderTotal    = orderTotal    line1 + orderTotal line2
                   } 
-- como convertir un conjunto a nuestro monoide
toTotalLine :: OrderLine -> TotalLine
toTotalLine line =
  TotalLine { totalQuantity = productQuantity line, orderTotal = lineTotal line }

----- para la parte de consola y los datos
instance Show OrderLine where
  show (OrderLine plCode plQuantity plPrice plTotal) =
    printf "%-10s %5i @%4g each %6g" plCode plQuantity plPrice plTotal

instance Show TotalLine where
  show (TotalLine tlQuantity tlTotal) =
    printf "%-10s %5i            %6g" "TOTAL" tlQuantity tlTotal

sampleLines :: [OrderLine]
sampleLines =
  [ OrderLine { productCode = "AAA", productQuantity = 2, price = 2.99, lineTotal = 5.98 }
  , OrderLine { productCode = "BBB", productQuantity = 1, price = 1.99, lineTotal = 1.99 }
  , OrderLine { productCode = "CCC", productQuantity = 3, price = 3.99, lineTotal = 11.97 }
  ]

moreSampleLines :: [OrderLine]
moreSampleLines =
  [ OrderLine { productCode = "DDD", productQuantity = 4, price = 4.99, lineTotal = 19.96 }
  , OrderLine { productCode = "BBB", productQuantity = 1, price = 1.99, lineTotal = 1.99 }
  , OrderLine { productCode = "EEE", productQuantity = 2, price = 2.00, lineTotal = 4.00 }
  ]

-- funcion principal para consola
main :: IO()
main = do
  mapM_ print sampleLines
  putStrLn "----------------------------------"
  print subtotal
  putStrLn "----------------------------------"
  mapM_ print moreSampleLines
  putStrLn "----------------------------------"
  print bigTotal
  where -- tengo que parar los OrderLines a TotalLines, para eso tenemos la funcion toTotalLine
 -- subtotal = mconcat $ map toTotalLine sampleLines -- alternativa a lo siguiente
    subtotal = foldMap toTotalLine sampleLines 
    -- foldMap recibe una funcion de un elemento que no es monoide y lo transforma en un elemento que es monoide
    -- convierte de un conjunto a un monoide
    -- foldMap :: Monoid m => (a -> m) -> t a -> m
    bigTotal = foldMap toTotalLine moreSampleLines <> subtotal --mappend <>
