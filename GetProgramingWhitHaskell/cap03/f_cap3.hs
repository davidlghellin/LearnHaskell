sumCuadradosOrCuadradosSuma x y = if sumaCuadrados > cuadradosSuma
                                    then sumaCuadrados
                                    else cuadradosSuma
    where sumaCuadrados = x^2 + y^2
          cuadradosSuma = ( x + y ) ^ 2

-- sumCuadradosOrCuadradosSuma 4 3
-- 49
----------------------------------------------------------------------



-- Otra forma de verlo, podria ser pasar las funciones como parametros

body sumaCuadrados cuadradosSuma = if sumaCuadrados > cuadradosSuma
        then sumaCuadrados
        else cuadradosSuma

sumaCuadradosOrCuadradosSumaParam x y = body (x^2 + y^2) (( x + y ) ^ 2)
-- sumaCuadradosOrCuadradosSumaParam 3 4 
-- 49

----------------------------------------------------------------------

dobleDoble x = dobleX * 2
        where dobleX = x * 2

-- Ejercicio, escribir de nuevo dobleDoble usando lambdas
dobleDobleLambda x = (\doble -> doble * 2) (x*2)

----------------------------------------------------------------------
-- podemos hacer lo mismo usando let


sumCuadradosOrCuadradosSumaLet x y = let sumaCuadrados = x^2 + y^2
                                         cuadradosSuma = ( x + y ) ^ 2
                                in --cuerpo
                                   if sumaCuadrados > cuadradosSuma
                                        then sumaCuadrados
                                        else cuadradosSuma