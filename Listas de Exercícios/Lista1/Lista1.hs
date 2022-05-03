module Lista1 where

{-Lista 1 até quinta feira-}

{-import Data.Char-}
{-importa a biblioteca para ter outras funções como a toUpper-}

ehTriangulo a b c = a < b+c && b < a+c && c < a+b
{-módulo: conjunto de funções, classes e tipos dentro de um haskell script.
Bibliotecas são módulos-}

{-dando main ao rodar o arquivo de verificação vai aparecer o que está correto ou não-}

tipoTriangulo a b c | a == b && b == c = "equilatero"
                    | a == b || a == c || b == c = "isosceles"
                    | otherwise = "escaleno"

triangulo a b c | a >= b+c || b >= a+c || c >= a+b = "nao eh um triangulo"
                | a == b && b == c = "equilatero"
                | a == b || a == c || b == c = "isosceles"
                | otherwise = "escaleno"

somaPares n | n == 0 = 0
            | even n = n + somaPares (n-2)
            | otherwise = somaPares (n-1)
{- a função even pode ser usada no lugar de "rem n 2 == 0", 
nesse caso seria "even n"-}


{-expo m n | n == 0 = 1  
         | otherwise = m * expo m (n-1)-}

somaPot2m m 0 = m
somaPot2m m n = (2^n) * m + somaPot2m m (n-1)

{-primon n m | m == 2 = True
           | rem n m /= 0 = primon n (m-1)
           | otherwise = False-}

primo n | even n && n /= 2 = False
        | [x | x <- [1..n], rem n x == 0] == [1,n] = True 
        | otherwise = False
    

{-serie n m | 4/n < 4/m = 4/m - 4/(m+2) + serie n (m+4)
          | otherwise = 4/n    

seriePI n = serie n 1-}

{-seriePI n = if n then seriePI (n-1) else 4/n + seriePI (n-2)-}

{-seriePI 1 = 4 
seriePI n = if even (truncate n) then seriePI (n-1) 
            else if even (truncate (n-1/2)) then -4/n + seriePI (n-2) 
            else 4/n + seriePI (n-2)
            
            Professor, o exercício 6 tinha alguma maneira de fazer sem utilizar um lista (eu usei essa [x | x <- [1..n], rem n x == 0]) e sem chamar outra função de modo que a função funcione pra todos os primos
            
            -}

seriePI n | n == 1 = 4 
          | even (truncate n) = seriePI (n-1)
          | even (truncate ((n-1)/2)) = 4/n + seriePI (n-2)
          | otherwise = -4/n + seriePI (n-2)
