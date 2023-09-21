{-
relacionesValidas seq(String X String) : Bool {
    asegura: res = true sii no hay tuplas con ambos componentes iguales ni tuplas repetedias, sin importar el orden
}
-}

relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas (x : xs) = (fst x /= snd x) && not(elem x xs) && not(elem (snd x, fst x) xs) && relacionesValidas xs

{-problema personas (relaciones: seq⟨String × String⟩) : seq⟨String⟩ {
requiere: {relacionesV alidas(relaciones)}
asegura: {resu tiene exactamente los elementos que figuran en alguna tupla de relaciones en cualquiera de las dos
posiciones, sin repetir}
}-}
personas :: [(String,String)] -> [String]
personas xs = eliminarRepetidos (tuplasALista xs)



quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos n [] = []
quitarTodos n xs | n == head xs = [] ++ quitarTodos n (tail xs)
                 | otherwise = [head xs] ++ quitarTodos n (tail xs)

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece x xs = x : quitarTodos x (eliminarRepetidos xs)
                         | otherwise = x : eliminarRepetidos xs

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece n [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise = pertenece n xs

tuplasALista :: [(a,a)] -> [a]
tuplasALista ((a,b):xs) = a : b : tuplasALista xs
tuplasALista [] = []

{-
problema amigosDe (persona: String, relaciones: seq⟨String × String⟩) : seq⟨String⟩ {
requiere: {relacionesV alidas(relaciones)}
asegura: {resu tiene exactamente los elementos que figuran en alguna tupla de relaciones en las que alguna de las
componentes es persona}
}-}

amigosDe :: String -> [(String,String)] -> [String]
amigosDe x [] = []
amigosDe x ((a,b) : ys) | x == a = [b] ++ amigosDe x ys
                        | x == b = [a] ++ amigosDe x ys
                        | otherwise = [] ++ amigosDe x ys

{-
problema personaConMasAmigos (relaciones: seq⟨String × String⟩) : String {
requiere: {relaciones no vac´ıa}
requiere: {relacionesV alidas(relaciones)}
asegura: {resu es el Strings que aparece m´as veces en las tuplas de relaciones (o alguno de ellos si hay empate)}
}-}
personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos xs = personaConMasAmigosAux (personas xs) xs -- aca entra como datos la lista plana sin repetidos y la lista de duplas

personaConMasAmigosAux :: [String] -> [(String,String)] -> String -- compara
personaConMasAmigosAux [x] _ = x
personaConMasAmigosAux (x:y:xs) relaciones | cantDeAmigos x relaciones > cantDeAmigos y relaciones = personaConMasAmigosAux(x:xs) relaciones
                                           | otherwise = personaConMasAmigosAux(y:xs) relaciones

cantDeAmigos :: String -> [(String,String)] -> Integer
cantDeAmigos p rel = longitud(amigosDe p rel)

longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


{-Si queres usar hunit aca tenes un template

    Ejercicio 1
    Para empezar a diseñar la novedosa y rupturista red social Y el famoso Elio Mark nos ha pedido que desarrollemos algunas funciones básicas, que tendrán como objetido representar algunas relaciones e interacciones entre los usuarios. Para esto nos envió las siguientes especificaciones en lenguaje semiformal y nos pidió que hagamos el desarrollo enteramente en Haskell, utilizando los tipos requeridos y solamente las funciones que se ven en Introducción a la Programación de Exactas-UBA.

    problema relacionesValidas (relaciones: seq⟨String x String⟩) : Bool {
      requiere: {True}
      asegura: {(res = true) <=> relaciones no contiene ni tuplas repetidas1, ni tuplas con ambas componentes iguales}
    }
    1 A los fines de este problema consideraremos que dos tuplas son iguales si el par de elementos que las componen (sin importar el orden) son iguales.

    problema personas (relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
      requiere: {relacionesValidas(relaciones)}
      asegura: {res no tiene elementos repetidos}
      asegura: {res tiene exactamente los elementos que figuran en alguna tupla de relaciones, en cualquiera de sus posiciones}
    }

    problema amigosDe (persona: String, relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
      requiere: {relacionesValidas(relaciones)}
      asegura: {res tiene exactamente los elementos que figuran en las tuplas de relaciones en las que una de sus componentes es persona}
    }

    problema personaConMasAmigos (relaciones: seq⟨String x String⟩) : String {
      requiere: {relaciones no vacía}
      requiere: {relacionesValidas(relaciones)}
      asegura: {res es el Strings que aparece más veces en las tuplas de relaciones (o alguno de ellos si hay empate)}
    }-}
