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


--todosDigitosIguales :: Int -> Bool
--todosDigitosIguales n | n < 10 = True
  --                    | otherwise = digitosIguales (mod n 10) (div n 10)

todosDigitosIguales :: Int -> Bool
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