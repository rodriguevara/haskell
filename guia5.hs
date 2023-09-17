-- 1.1
longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--1.2
ultimo :: [t] -> t
ultimo (x:xs) | longitud (x:xs) == 1 = x
              | otherwise = ultimo xs

--1.3
principio :: [t] -> [t]
principio (x:xs) | longitud (x:xs) == 1 = []
                 | otherwise = x : principio xs
--1.4
reverso :: [t] -> [t]
reverso t | longitud t == 0 = []
          | otherwise = (ultimo t) : reverso (principio t)

--2.1
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece n [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise = pertenece n xs

--2.2
todosIguales :: (Eq t) => [t] -> Bool
todosIguales (x:xs) | longitud xs == 0 = True
                    | x /= head xs = False
                    | otherwise = todosIguales xs

--2.3
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos (x:xs) | longitud xs == 0 = True
                      | pertenece x xs = False
                      | otherwise = todosDistintos xs

--ej 2.4
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | pertenece x xs = True
                    | otherwise = hayRepetidos xs

--2.5
quitar :: (Eq t) => t -> [t] -> [t]
quitar x [] = []
quitar x xs | x == head xs = [] ++ tail xs
            | otherwise = [head xs] ++ quitar x (tail xs)

-- 2.6
quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos n [] = []
quitarTodos n xs | n == head xs = [] ++ quitarTodos n (tail xs)
                 | otherwise = [head xs] ++ quitarTodos n (tail xs)

-- 2.7
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece x xs = x : quitarTodos x (eliminarRepetidos xs)
                         | otherwise = x : eliminarRepetidos xs
-- 2.8
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos (x:xs) (y:ys) | x == y && xs == [] && ys == [] = True
                              | pertenece x (y:ys) && pertenece y (x:xs) = mismosElementos xs ys
                              | otherwise = False
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
