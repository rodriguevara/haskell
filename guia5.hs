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


--3.1

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria(xs)

--3.2
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria(xs)

--3.3
maximo :: [Int] -> Int
maximo [x] = x
maximo (x:y:xs) | x > y = maximo(x:xs)
                | otherwise = maximo(y:xs)

--3.4
sumarN :: Int -> [Int] -> [Int]
sumarN n [] = []
sumarN n [x] = [n+x]
sumarN n (x:xs) = [n+x] ++ sumarN n xs

--3.5
sumarElPrimero :: [Int] -> [Int]
sumarElPrimero [x] = [2*x]
sumarElPrimero (x:xs) = sumarN x (x:xs)

--3.6
sumarElUltimo :: [Int] -> [Int]
sumarElUltimo [x] = [2*x]
sumarElUltimo (xs) = sumarN (last xs) xs

--3.7
pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | mod x 2 == 0 = [x] ++ pares xs
             | otherwise = [] ++ pares xs

--3.8
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n [] = []
multiplosDeN n (x:xs) | mod x n == 0 = [x] ++ multiplosDeN n xs
                     | otherwise = [] ++ multiplosDeN n xs


--3.9
ordenar :: [Int] -> [Int]
ordenar [x] = [x]
ordenar (x:y:xs) | x > y = insertarOrdenado y (ordenar (x:xs))
                 | otherwise = insertarOrdenado x (ordenar (y:xs))

insertarOrdenado :: Int -> [Int] -> [Int]
insertarOrdenado n [] = [n]
insertarOrdenado n (x:xs) | n <= x = (n:x:xs)
                          | otherwise = x : (insertarOrdenado n xs)
