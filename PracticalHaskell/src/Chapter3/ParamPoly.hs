{-# LANGUAGE LambdaCase #-}
module Chapter3.ParamPoly where

-----------------------------
-- Parametric Polymorphism --
-----------------------------

myFst :: (a,b) -> a
myFst (a,b) = a 

-- :t maybeStr
-- -> maybeStr :: Maybe a -> [Char] 
maybeStr (Just _) = "Just"
maybeStr Nothing  = "Nothing"

data Client i = GovOrg  { clientId    :: i
                        , clienteName :: String 
                        }
              | Company { clientId    :: i
                        , clienteName :: String
                        , person      :: Person
                        , dity        :: String 
                        }
              | Individual { clientId :: i
                           , person   :: Person 
                           }
              deriving (Show, Eq, Ord)

data Person = Person { firstName :: String
                     , lastName  :: String
                     }
              deriving (Show, Eq, Ord)

-----------------------------
-- Functions As Parameters --
-----------------------------


----------------------------
-- Higher-Order Functions --
----------------------------
mySuccInt :: Integer -> Integer
mySuccInt a = a + 1 

apply3f2 :: (Integer -> Integer) -> Integer -> Integer
apply3f2 f x = 3 * f(x + 2)


-------------------------
-- Anonymous Functions --
-------------------------
sayHello :: [String] -> [String]
sayHello names = map (\nam -> case nam of
                              "David" -> "Hola David"
                              _       -> "Welcome " ++ nam
                     ) names

-- Al principio {-# LANGUAGE LambdaCase #-}
sayHello2 :: [String] -> [String]
sayHello2 names = map (\case 
                         "David" -> "Hola David"
                         nam      -> "Welcome " ++ nam
                     ) names

multplyByN :: Integer -> (Integer -> Integer)
multplyByN n = \x -> n * x


----------
-- Ejer --
----------
filterOnes :: [Integer] -> [Integer]
filterOnes = filter (\x-> x == 1)

filterNumber :: Integer -> [Integer] -> [Integer]
filterNumber n = filter (\x-> x == n)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot pred = filter (\x -> not $ pred x)


---------------------------------------
-- Partial Application of a Function --
---------------------------------------