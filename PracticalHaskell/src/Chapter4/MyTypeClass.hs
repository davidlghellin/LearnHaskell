module Chapter4.MyTypeClass where

import Chapter3.ParamPoly
--import Data.Map as M

class Nameable n where
    name :: n -> String

instance Nameable (Client i) where
    name Individual { person = Person { firstName = f,lastName = n } }
            = f ++ "-" ++ n
    name c = clienteName c 

----------
-- Ejer --
----------

class Priceable p where
    totalPrice :: [p] -> Double

data Item = Item { nameItem   :: String
                 , priceItem  :: Double
                 , nIntem     :: Double
                 }
              deriving (Show, Eq, Ord)

sumaItemNormal :: Item -> Double
sumaItemNormal (Item _ p c ) = p * c 

instance Priceable(Item) where
  totalPrice []  = 0
  totalPrice (x:[]) = sumaItemNormal x 
  totalPrice (x:xs) = sumaItemNormal x + totalPrice xs

data Book = Book { nameBook  :: String
                 , priceBook :: Double
                 , editoBook :: String
                 }

instance Priceable(Book) where
    totalPrice [] = 0
    totalPrice ((Book _ p _):[]) = p
    totalPrice ((Book _ p _):xs) = p + totalPrice xs

------------
-- Ejer Eq--
------------

data Genero2 = Hombre | Mujer deriving (Show)
data Persona2 = Persona2 String String Genero2 deriving Show

instance Eq Genero2 where
    (==) Hombre Hombre = True 
    (==) Mujer Mujer   = True
    (==) _ _           = False

instance Eq Persona2 where
    --    Persona2 a1 b1 g1 == Persona2 a2 b2 g2 = a1 == a2 && b1 == b2 && g1 == g2
    (==) (Persona2 a1 b1 g1) (Persona2 a2 b2 g2) = a1 == a2 && b1 == b2 && g1 == g2


--
data Client2 i = GovOrg2 { clientId2    :: i
                         , clienteName2 :: String 
                         }
              | Individual2 { clientId2 :: i
                           , person2   :: Person2 
                           }

data Person2 = Person2 { firstName2 :: String
                      , lastName2  :: String
                      }


instance Eq Person2 where
   (==) (Person2 a1 b1) (Person2 a2 b2) = a1 == a2 && b1 == b2

-- TODO no funciona
--instance Eq (Client2 Int) where
--  (==) (GovOrg2 n name)  (GovOrg2 n2 name2)  = n == n2 && name == name2
--  (==) (Individual2 n p) (Individual2 n2 p2) = n == n2 && p == p2


-----------------
-- Binary Tree --
-----------------
data TravelGuide = TravelGuide { title  :: String
                               , autors :: [String]
                               , price  :: Double
                               }
                    deriving (Show, Eq, Ord)


data BinaryTree1 = Node1 TravelGuide BinaryTree1 BinaryTree1 
                 | Leaf1 deriving (Show)




treeFind1 :: TravelGuide -> BinaryTree1 -> Maybe TravelGuide
treeFind1 t (Node1 v l r) = case compare t v of
  EQ -> Just v
  LT -> treeFind1 t l
  GT -> treeFind1 t r
treeFind1 _ Leaf1 = Nothing

-- Creamos la instancia para la Type Class de Eq para comparar
instance Eq BinaryTree1 where
    (==) (Node1(TravelGuide t1 a1 p1)_ _ ) (Node1(TravelGuide t2 a2 p2)_ _ )  = t1 == t2 && a1 == a2 && p1 == p2
    (==) Leaf1 Leaf1 = True 
    (==) Leaf1 _ = False 
    (==) _ Leaf1 = False 

treeInsert1 :: TravelGuide -> BinaryTree1 -> BinaryTree1
treeInsert1 t n@(Node1 v l r) = case compare t v of
  EQ -> n
  LT -> Node1 v (treeInsert1 t l) r
  GT -> Node1 v l (treeInsert1 t r)
treeInsert1 t Leaf1 = Node1 t Leaf1 Leaf1

-- polymorphic Trees

data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a) 
                   | Leaf2 deriving Show

treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a
treeFind2 t (Node2 v l r) = case compare t v of
  EQ -> Just v
  LT -> treeFind2 t l
  GT -> treeFind2 t r
treeFind2 _ Leaf2 = Nothing


treeInsert2 :: Ord a => a -> BinaryTree2 a -> BinaryTree2 a
treeInsert2 t n@(Node2 v l r) = case compare t v of
  EQ -> n
  LT -> Node2 v (treeInsert2 t l) r
  GT -> Node2 v l (treeInsert2 t r)
treeInsert2 t Leaf2 = Node2 t Leaf2 Leaf2

---
newtype TGByPrice = TGByPrice TravelGuide deriving Eq

instance Ord TGByPrice where
  (TGByPrice (TravelGuide t1 a1 p1)) <= (TGByPrice (TravelGuide t2 a2 p2)) =
     p1 < p2 || (p1 == p2 && (t1 < t2 || (t1 == t2 && a1 <= a2)))

-------------
-- Functor --
-------------

modifyTravelGuidePrice :: Double -> [TravelGuide] -> [TravelGuide]
modifyTravelGuidePrice m = map (\tg -> tg { price= m * price tg})

--modifyTravelGuidePriceMap :: Double -> M.Map a TravelGuide -> M.Map a TravelGuide
--modifyTravelGuidePriceMap m =M.map (\tg -> tg { price= m * price tg})

modifyTravelGuidePrice' :: Functor f => Double -> f TravelGuide -> f TravelGuide
modifyTravelGuidePrice' m = fmap (\tg -> tg { price = m * price tg})

modifyTravelGuidePrice2' :: Functor f => Double -> f TravelGuide -> f TravelGuide
modifyTravelGuidePrice2' m = (<$>) (\tg -> tg { price = m * price tg})

-- tenemos una estructura recursiva como un arbol
data Tree a = Leaf a 
            | Branch (Tree a) (Tree a) 
            deriving (Show)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x)            = Leaf (f x)
treeMap f (Branch left right) = Branch (treeMap f left) (treeMap f right)

instance Functor Tree where
    fmap f (Leaf x)            = Leaf   (f x)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)


-- Otras --
-----------
steep :: Int -> Maybe Int
steep a = Just (a + 1) 

