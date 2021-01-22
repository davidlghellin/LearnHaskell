module Otro.Web.MaquinaCompraEstados where

-- estados
data EstadosSuper = Vacio
                  | Comprar
                  | FueraSuper
                  deriving Show

data MiCompra = MiCompra { cantidadCompra :: Integer
                         , preciosCompra  :: Integer
                         } 
                         deriving Show

entrarSuper :: MiCompra
entrarSuper =  MiCompra 0 0

sumarCompra :: MiCompra -> MiCompra -> MiCompra
sumarCompra (MiCompra cantidadAntiguo preciosAntiguo) (MiCompra cantidadB preciosB) = 
                  MiCompra { cantidadCompra = cantidadAntiguo + cantidadB 
                           , preciosCompra  = preciosAntiguo  + cantidadB * preciosB
                           }


irComprar :: EstadosSuper
irComprar = Vacio


empezarComprar :: EstadosSuper -> MiCompra -> MiCompra -> MiCompra
empezarComprar Vacio _  _  = entrarSuper
empezarComprar Comprar a b = sumarCompra a b

-- let c1 = empezarComprar Vacio entrarSuper entrarSuper
-- empezarComprar Comprar  MiCompra {cantidadCompra = 2, preciosCompra = 2} c1
-- sumarCompra MiCompra {cantidadCompra = 0, preciosCompra = 0} MiCompra {cantidadCompra = 2, preciosCompra = 2}
-- sumarCompra (sumarCompra  MiCompra {cantidadCompra = 0, preciosCompra = 0} MiCompra {cantidadCompra = 2, preciosCompra = 2})  MiCompra {cantidadCompra = 1, preciosCompra = 1}