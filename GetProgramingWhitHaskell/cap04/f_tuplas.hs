import Data.List

nombres = [
    ("david","lopez"),
    ("cesar","davia"),
    ("ramon","perez")] 

compararApellido name1 name2 = if lastName1 > lastName2
                                then GT
                                else if lastName1 < lastName2
                                    then LT
                                else EQ
                        where lastName1 = snd name1
                              lastName2 = snd name2

compararApellidoNombre name1 name2 = if lastName1 > lastName2
                                        then GT
                                        else if lastName1 < lastName2
                                          then LT
                                          else if firstName1 > firstName2
                                            then GT
                                            else if firstName1 < firstName2
                                              then LT
                                              else EQ
                                    where lastName1 = snd name1
                                          lastName2 = snd name2
                                          firstName1 = fst name2
                                          firstName2 = fst name2

----------------------------------------------------------------------
addresLetter name location = nameText ++ " - " ++ location
                            where nameText = (fst name) ++ " " ++ (snd name)
-- *Main> addresLetter ("David", "lopez") "ctr de Madrid 123"
-- "David lopez - ctr de Madrid 123"

-- Veamos otro punto de vista, e imaginemos que tenemos que tratar los datos segun unas condiciones
--  por ejemplo de ciudades
albaOffice nombre = if apellido < "L"
                    then nameText ++ " - CP 1111 Albacete"
                  else nameText ++ " - CP 0000 Albacete"
                where apellido = snd nombre
                      nameText = (fst nombre) ++ " " ++ apellido

cuencaOffice nombre = nameText ++ " - CP 1231254 Cuenca"
                where nameText = (fst nombre) ++ " " ++ (snd nombre)

getLocationFun location = case location of
                "ab" ->  albaOffice
                "cu" ->  cuencaOffice  
                _    ->  (\name -> (fst name) ++ " " ++ (snd name))

addresLetterMatch name location = locationFun name
        where locationFun = getLocationFun location

--        *Main> addresLetterMatch ("david","lopez") "ab"
--        "david lopez - CP 0000 Albacete"
--        *Main> addresLetterMatch ("david","lopez") "cu"
--        "david lopez - CP 1231254 Cuenca"
--        *Main> addresLetterMatch ("david","lopez") "mu"
--        "david lopez"        