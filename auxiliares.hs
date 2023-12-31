--productoria :: [Int] -> Int que devuelve la productoria de los elementos
--sumarN :: Int -> [Int] -> [Int] que dado un numero N y una lista xs, suma a N a cada elementos de xs
--sumarElPrimero :: [Int] -> [Int] que dada una lista no vacia xs, suma el primer elemento a casa elemento de xs
--ej sumaElPrimero [1,2,3] -> [2,3,4]
--sumarElUltimo :: [Int] -> [Int] que dada una lista no vacia xs, suma el ultimo elemento a cada elemento de xs
--pares :: [Int] -> [Int] que devuelve una lista con los elementos pares de la lista original, ej [1,2,3,5,8] -> [2,8]
--multiploDeN :: Int -> [Int] -> [Int] que dado un N y una lista xs, devuelve una lista con los elementos multiplos N de xs
--quitar :: Int -> [Int] -> [Int] que elimina la primera aparicion del elemento en la lista (de haberla)
--hayRepetidos :: [Int] -> Bool que indica si una lista tiene elementos repetidos
--eliminarRepetidos :: [Int] -> [Int] que deja en la lista una unica aparicion de cada elemento.
--maximo :: [Int] -> Int que calcula el maximo elemento de una lista no vacia
--ordenar :: [Int] -> [Int] que ordena los elementos de forma creciente
--reverso :: [Int] -> [Int] que dada una lista invierte su orden

productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

sumarN :: Int -> [Int] -> [Int]
sumarN n [] = []
sumarN n (x:xs) = (n+x) : sumarN n xs

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero [] = []  -- Caso base: lista vacía, retorna lista vacía
sumarElPrimero (x:xs) = (x + head xs) : sumarElPrimero xs
