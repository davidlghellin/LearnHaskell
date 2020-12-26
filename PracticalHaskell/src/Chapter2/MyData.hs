module Chapter2.MyData where

data Client = Org        String
            | Company    String Integer Persona String 
            | Individual Persona Bool
              deriving Show
-- Individual (Persona "david" "lopez" Hombre) True
-- Company "Empresa" 12  (Persona "david" "lopez" Hombre) "ok"

data Genero = Hombre | Mujer deriving (Show, Eq)

data Persona = Persona String String Genero deriving Show

nombreCliente :: Client -> String
nombreCliente cliente = case cliente of
                           Org name             -> name
                           Company name _ _ _   -> name
                           Individual persona _ ->
                             case persona of
                                Persona name surname _  -> name ++ " " ++ surname

nombreCompanyia :: Client -> Maybe String
nombreCompanyia cliente = case cliente of
                            Company nombre _ _ _ -> Just nombre
                            _                    -> Nothing
