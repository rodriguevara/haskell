module Simulacro where
{-Ejercicio 1-}
{-
problema relacionesValidas(relaciones:seq{String x String}) : Bool {
    requiere: {True}
    asegura: {{res = true} <=> no hay tuplas en relaciones con ambas componentes iguales ni tuplas repetidas (sin considerar el orden)}
    } 
-}
--Comprobante : x4TDKdXd
relacionesValidas :: [(String,String)] -> Bool
relacionesValidas relaciones = not(tuplasTienenMismosComponentes relaciones) && not(tuplasRepetidas relaciones)


tuplasTienenMismosComponentes :: [(String,String)] -> Bool
tuplasTienenMismosComponentes []     = False
tuplasTienenMismosComponentes (x:xs) = (fst(x) == snd(x)) || tuplasTienenMismosComponentes xs


tuplasRepetidas :: [(String,String)] -> Bool
tuplasRepetidas []     = False
tuplasRepetidas (x:xs) = ((pertenece x xs) || (pertenece (snd(x),fst(x)) xs)) || tuplasRepetidas xs


pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs


{-Ejercicio 2-}
{-
problema personas (relaciones: seq<String x String>) : seq<String> {
    requiere: {relacionesValidas(relaciones)}
    asegura: {resu tiene exactamente los elementos que figuran en alguna tupla de relaciones en cualquiera de las dos posiciones, sin repetir}
}
-}

personas :: [(String,String)] -> [String]
personas relaciones = personasAux relaciones [] 


personasAux :: [(String,String)] -> [String] -> [String]
personasAux   []   ps  = ps
personasAux (x:xs) ps | not(pertenece (fst(x)) ps) && not(pertenece (snd(x)) ps) = personasAux xs (fst(x) : snd(x) : ps)
                      | not(pertenece (fst(x)) ps) =  personasAux xs (fst(x) : ps)
                      | not(pertenece (snd(x)) ps) = personasAux xs (snd(x) : ps)
                      | otherwise = personasAux xs ([] ++ ps)

{-Ejercicio 3-}
{-
problema amigosDe (persona: String, relaciones: seq<String,String>) : seq<String>{
    requiere: {relacionesValidas(relaciones)}
    asegura: {resu tiene exactamente los elementos que figuran en alguna tupla de relacones en las que alguna de las componentes es persona}
}
-}

amigosDe :: String -> [(String,String)] -> [String]
amigosDe persona (x:relaciones) = amigosDeAux persona (x:relaciones) []

amigosDeAux :: String -> [(String,String)] -> [String] -> [String]
amigosDeAux persona [] amigos = amigos
amigosDeAux persona (x:relaciones) amigos | persona == fst(x) = amigosDeAux persona relaciones (snd(x):amigos)
                                          | persona == snd(x) = amigosDeAux persona relaciones (fst(x):amigos)
                                          | otherwise         = amigosDeAux persona relaciones ([] ++ amigos)


{-Ejercicio 4-}
{-
-}

personaConMasAmigos :: [(String,String)] -> String
personaConMasAmigos [] = undefined
personaConMasAmigos relaciones = mayorApariciones (personas(relaciones)) relaciones ""


mayorApariciones :: [String] -> [(String,String)] -> String -> String
mayorApariciones [] relaciones resu = resu
mayorApariciones (x:personas) relaciones resu | (cantidadDeApariciones x relaciones) >= (cantidadDeApariciones resu relaciones) = mayorApariciones personas relaciones x
                                              | otherwise = mayorApariciones personas relaciones resu

cantidadDeApariciones :: String -> [(String,String)] -> Integer
cantidadDeApariciones x [] = 0
cantidadDeApariciones x (y:relaciones) | (x == fst(y)) || (x == snd(y)) = 1 + cantidadDeApariciones x relaciones
                                       | otherwise                      = 0 + cantidadDeApariciones x relaciones