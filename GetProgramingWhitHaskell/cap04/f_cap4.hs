siParInc x = if even x
            then x + 1
            else x

-- Ahora suponemos que queremos hacer si es par el doble, el cuadrado ...
siParDoble x = if even x
    then x * 2
    else x

siParCuadrado x = if even x
    then x * x
    else x

-- aunque son faciles de escribir es codigo similar, la unica diferencia es el calculo de la funcion de retorno

siPar myFun x = if even x
                then myFun x
                else x
            
inc n = n + 1
doble n = 2 * n
cuadrado n = n * n                

-- vamos a reescribir las funciones de arriba usando estas de aqui
siParIncHO n = siPar inc n
siParDobleHO n = siPar doble n
siParCuadradoHO n = siPar cuadrado n

-- Ahora facilmente podemos añadir nuevas
cubo n = n ^ 3
siParCuboHO n = siPar cubo n 

-- Tambien podemos usar los lambdas para crear estas funciones
siParSuma6 n = siPar (\x -> x + 6) n 