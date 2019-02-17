-- hello.hs mi primer programa haskell

main = do
    print "Hola mundo"

-- Para compilar ghc hello.hs
--   Genera 3 archivos
--   hello ejecutable
--   hello.hi
--   hello.o

-- Para ejecutar ./hello

-- Para crear el ejecutable con nuestros propios nombres
-- ghc hello.hs -o nombre_ejecutable

--------------------
-- Para cargar el modulo en la consola 
-- ghci hello.hs

-- ghci entramos al modo interactivo
-- :l nombre_fichero.hs --> cargamos el modulo