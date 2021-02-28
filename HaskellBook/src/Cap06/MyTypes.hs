module Cap06.MyTypes where

data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun 
    deriving Ord -- add Ord para poder instanciar Eq usando Ord

data Date = 
    Date DayOfWeek Int

instance Eq DayOfWeek where
    (==) Mon Mon   = True
    (==) Tue Tue   = True
    (==) Weds Weds = True
    (==) Thu Thu   = True
    (==) Fri Fri   = True
    (==) Sat Sat   = True
    (==) Sun Sun   = True
    (==) _   _     = False

-- con la opcion en la REPL
-- :set -Wall
-- diremos en la compilación que se comprueben todos los casos, en caso contrario fallará

instance Eq Date where
    (==) (Date weekday dayOfMont) (Date weekday' dayOfMont') = 
        weekday == weekday' 
        && dayOfMont == dayOfMont'


data Identity a =
              Identity a

-- instance Eq (Identity a) where
--    (==) (Identity v) (Identity v') = v == v'
-- lo de antes da error porque no sabemos nada de a

-- Ahora funcionará, porque sabemos que debe tener una instancia de Eq. Además,
-- Haskell se asegurará de que no intentemos verificar la igualdad con valores que no tengan una instancia de Eq en el momento de la compilación:
--instance Eq a => Eq (Identity a) where
--  (==) (Identity v) (Identity v') = v == v'

instance Ord a => Eq (Identity a) where 
    (==) (Identity v) (Identity v') = compare v v' == EQ

----------
-- Ejer --
----------

-- Escribe las instancias de Eq
data TisAnInteger = TisAn Integer

data TwoIntegers = Two Integer Integer

data StringOrInt = TisAnInt Int
                 | TisAString String

data Pair a = Pair a a

data Tuple a b = Tuple a b

data Which a = ThisOne a | ThatOne a

data EitherOr a b = Hello a
                  | Goodbye b
----------
instance Eq (TisAnInteger) where
    (==) (TisAn a) (TisAn b) = a == b

instance Eq (TwoIntegers) where
    (==) (Two n1 m1) (Two n2 m2) = n1 == n2 && m1 == m2

instance Eq (StringOrInt) where
    (==) (TisAnInt n1)   (TisAnInt n2)   = n1 == n2
    (==) (TisAString s1) (TisAString s2) = s1 == s2

instance Eq a => Eq (Pair a) where
    (==) (Pair a1 b1) (Pair a2 b2) = a1 == a2 && b1 == b2

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a1 b1) (Tuple a2 b2) = a1 == a2 && b1 == b2 

instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne b) = a == b
    (==) (ThatOne a) (ThatOne b) = a == b
    (==) (ThisOne a) (ThatOne b) = a == b
    (==) (ThatOne a) (ThisOne b) = a == b

instance (Eq a, Eq b) => Eq (EitherOr a b) where 
    (==) (Hello n)   (Hello m)   = n == m
    (==) (Goodbye n) (Goodbye m) = n == m
