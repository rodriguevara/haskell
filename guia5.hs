--ej 2.4   
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | pertenece x xs = True
                      | otherwise = hayRepetidos xs

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece n [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise = pertenece n xs

--ej 3
maximo :: [Int] -> Int
maximo [x] = x
maximo (x:y:xs) | x > y = maximo (x:xs) -- x es el 1°elem de la lista, y es el 2°elem, xs es el resto de la lista
                | otherwise = maximo (y:xs)

--3.9
ordenar :: [Int] -> [Int]
ordenar [x] = [x]
ordenar (x:y:xs) | x > y = insertarOrdenado y (ordenar (x:xs))
                 | otherwise = insertarOrdenado x (ordenar (y:xs))

insertarOrdenado :: Int -> [Int] -> [Int]
insertarOrdenado n [] = [n]
insertarOrdenado n (x:xs) | n <= x = (n:x:xs)
                          | otherwise = x : (insertarOrdenado n xs)