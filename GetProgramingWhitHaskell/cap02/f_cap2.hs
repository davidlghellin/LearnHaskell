simple x = x

-- Todas las funciones en Haskell tienen 3 cosas
--  Toman un argumento
--  Retornan un valor
--  Cualquier funcion llamada con el mismo parametro, 
--      deben devolver el mismo valor de otras veces llamadas con el mismo parametro (REFERENTIAL TRANSPARENCY)

calcChange_v1 owed given = if given - owed > 0
                        then given - owed
                        else 0
-- En este caso tenemos errores, como eque la resta la calculamos dos veces en este caso es de complejidad pequeña
-- 

---------------------------------------------

calcChange_v2 owed given = if change > 0
                            then change
                            else 0
    where change = given - owed
-- vemos que estamos usando una variable que se calculara cuando la necesitemos y es inmutable
-- veamos otro ejemplo

-- OJO con la tabulacion del where => al mismo nivel

dobleMasDos x = dobleX + 2
    where dobleX = x * 2 