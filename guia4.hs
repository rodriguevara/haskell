fib :: Int -> Int
fib x | x == 0 = 0
      | x == 1 = 1
      | otherwise = fib(x - 1) + fib(x - 2)

parteEntera :: Float -> Int
parteEntera x | x >=0 && x < 1 = 0 
              | x <= 0 && x > -1 = 0
              | x >= 1 = 1 + parteEntera(x-1)
              | otherwise = (-1) + parteEntera(x+1)


esDivisible :: Int -> Int -> Bool
esDivisible x y | x < y = False
                | x == y = True
                | otherwise = esDivisible (x-y) y

sumaImpares :: Int -> Int
sumaImpares n | n == 1 = 1
              | otherwise = sumaImpares(n-1) + 2*n-1

medioFac :: Int -> Int
medioFac x | x == 0 = 1
           | x == 1 = x
           | x > 0 = x * medioFac(x - 2)


sumaDigitos :: Int -> Int
sumaDigitos n | n < 10 = n --significa q tiene un solo digito
              | otherwise = n `mod` 10 + sumaDigitos(n `div` 10)


todosDigitosIguales :: Int -> Bool
todosDigitosIguales n | n < 10 = True
                      | otherwise = digitosIguales (mod n 10) (div n 10)

-- todosDigitosIguales :: Int -> Bool
todosDigitosIguales n | tieneunsolodig n = True
                      | unidades n == decenas n = todosDigitosIguales(sinUnidades n)
                      | otherwise = False

    where
        tieneunsolodig n = n < 10
        unidades n = mod n 10
        sinUnidades n = div n 10
        decenas n = unidades (sinUnidades n)

digitosIguales :: Int -> Int -> Bool
digitosIguales x y | y < 10 && x == y = True
                   | otherwise = (x == mod y 10) && digitosIguales x (div y 10)


--Ej 16

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n desde | mod n desde == 0 = desde
                          | otherwise = menorDivisorDesde n (desde + 1)

-- esPrimo
esPrimo :: Int -> Bool
esPrimo n | menorDivisor n == n = True
          | otherwise = False

-- ej 19 REVISAR
esSumaInicialDePrimos :: Int ->Bool
esSumaInicialDePrimos n = esSumaInicialDePrimosAux n 2

esSumaInicialDePrimosAux :: Int -> Int -> Bool
esSumaInicialDePrimosAux n q | n == sumaPrimosHasta 2 q = True
                             | n < sumaPrimosHasta 2 q = False
                             | otherwise = esSumaInicialDePrimosAux n (q+1)

sumaPrimosHasta :: Int -> Int -> Int
sumaPrimosHasta m n  | esPrimo n && n == m = m
                     | n == m = 0
                     | esPrimo m = m + sumaPrimosHasta (m + 1) n
                     | otherwise = sumaPrimosHasta (m+1) n

-- ej 13
f :: Int -> Int -> Int
f filas columnas = recorrerFilasHasta filas columnas


recorrerFilasHasta :: Int -> Int -> Int
recorrerFilasHasta 1 columnas = sumaFila 1 columnas -- caso base
recorrerFilasHasta fila columnas = sumaFila fila columnas + recorrerFilasHasta (fila -1) columnas

sumaFila :: Int -> Int -> Int
sumaFila fila 1 = fila^1
sumaFila fila columnas = fila^columnas + sumaFila fila (columnas - 1)


-- ej 8
--iesimoDigito :: Int -> Int -> Int
--iesimoDigito n i = (n `div` 10^(cantDigitos n-i )) `mod` 10

--cantDigitos :: Int -> Int
--cantDigitos n | n == 0 = 0
  --            | otherwise = 1 + cantDigitos (div n 10)

-- ej 9


esCapicua :: Int -> Bool
esCapicua n | n < 10 = True
            | otherwise = (primerDigito (n) == ultimoDigito (n)) && (primerDigito (reducirNumero n) == ultimoDigito (reducirNumero n))

reducirNumero :: Int -> Int
reducirNumero n = sacarPrimerDigito (sacarUltimoDigito (n))

ultimoDigito :: Int -> Int
ultimoDigito n = mod n 10

primerDigito :: Int -> Int
primerDigito n | n < 10 = n
               | otherwise = primerDigito (div n 10)

sacarUltimoDigito :: Int -> Int
sacarUltimoDigito n = div n 10

sacarPrimerDigito :: Int -> Int
sacarPrimerDigito n =  mod n (10 ^ ((cantDigitos n) - 1))

cantDigitos :: Int -> Int
cantDigitos n | n < 10 = 1
              | otherwise = 1 + cantDigitos(sacarUltimoDigito n)

--10 a
f1 :: Int -> Int
f1 n | n == 0 = 1
     | otherwise = 2^n + f1(n-1)

--10 b  
f2 :: Int -> Float -> Float
f2 n q | n == 1 = q
       | otherwise = q^n + f2 (n-1) q

--10 c
f3 :: Int -> Float -> Float
f3 n q = f2 (2*n) q

--10 d
f4 :: Int -> Float -> Float
f4 n q = f3 n q - f2 (n-1) q