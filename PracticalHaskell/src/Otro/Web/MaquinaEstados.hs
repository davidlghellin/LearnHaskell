module Otro.Web.MaquinaEstados where
-- https://www.luisllamas.es/maquina-de-estados-finitos-arduino/

{-
https://blog.ploeh.dk/2020/08/31/properties-for-all/
La mayoría de los procesos comerciales se pueden modelar como máquinas de estados finitos. 
Una vez que haya entendido el problema lo suficientemente bien como para enumerar los posibles estados,
puede aplicar ingeniería inversa a un tipo de datos algebraicos a partir de esa enumeración.

Si bien los datos transportados en la máquina de estado pueden no estar restringidos,
el número de estados y transiciones de estado suele ser limitado. Modele los estados como valores enumerables y podrá cubrir todos los valores de entrada con combinatoria simple.
-}
-- circulos A B C D
data Estado = Inicial
            | B
            | C
            | Fin
            deriving (Enum)

-- Posibles moviemientos reset
data Movimientos = Reset
                 | Forward
                 | Back
                 deriving Show

instance Show Estado where
    show Inicial = "A"
    show B       = "B"
    show C       = "C"
    show Fin     = "D"

crearMaquinaEstado :: Estado
crearMaquinaEstado =  Inicial

mover :: Movimientos -> Estado -> Estado
mover  Reset      _       = Inicial
mover  Forward    Inicial = B
mover  _          Inicial = Inicial
-- mover  Back       B       = Inicial
mover  Back       a       = pred a
-- mover  Forward    B       = C
mover  Forward    a       = succ a
-- al tener los enums nos ahorramos poner los casos que se pueden definir con el sucesor y predecesor en general
-- cuando tiene el orden
-- mover  Back       C       = B
-- mover  Back       D       = C
-- mover  Forward    C       = Fin