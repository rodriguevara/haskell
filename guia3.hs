todoMenor :: (Float, Float) -> (Float, Float) -> Bool 
todoMenor (x,y) (a,b) | x < a && y < b = True
                      | otherwise = False

posPrimerPar :: (Int,Int,Int) -> Int
posPrimerPar (x,y,z) | x `mod` 2 == 0 = 0
                     | y `mod` 2 == 0 = 1
                     | z `mod` 2 == 0 = 2
                     | otherwise = 4


f :: Int -> Int
f x | x <= 7 = x * x
    | otherwise = 2*x -1

g :: Int -> Int
g y | y `mod` 2 == 0 = y `div` 2
    | otherwise = 3*y +1

todosMenores :: (Int,Int,Int) -> Bool
todosMenores (x,y,z) | ((f(x) > g(x))&&(f(y) > g(y))&&(f(z) > g(z))) = True
                     | otherwise = False

distanciaManhattan:: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (x1,x2,x3) (y1,y2,y3) = (abs(x1-y1) + abs(x2-y2) + abs(x3-y3))


sumaUltimosDigitos :: Int -> Int
sumaUltimosDigitos x = (x`mod`10) + ((x`div`10) `mod` 10)

comparar :: Int -> Int -> Int
comparar a b | sumaUltimosDigitos(a) < sumaUltimosDigitos(b) = 1
             | sumaUltimosDigitos(a) > sumaUltimosDigitos(b) = -1
             | otherwise = 0