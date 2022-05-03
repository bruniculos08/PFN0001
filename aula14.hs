import Data.Char
menorMaior xs = (menor xs, maior xs)

menor [] = []
menor [x] = x
menor (x:xs) = if x < r then x else r 
    where r = menor xs

maior [] = []
maior [x] = [x]
maior (x:xs) = if x > r then x else r 
    where r = maior xs

menoresMaiores xs n = (menores xs n, maiores xs n)

menores [] _ = []
menores (x:xs) n = if x < n then x:r else r
    where r = menores xs n

maiores [] _ = []
maiores (x:xs) n = if x >= n then x:r else r 
    where r = maiores xs n

menores' xs n = [a | a <- xs, a < n]
maiores' xs n = [b | b <- xs, b > n]

menoresMaiores' xs n = ([a | a <- xs, a < n], [b | b <- xs, b >= n])


minusc [] = []
minusc (x:xs) = if isLower x == True then x : result else result
    where result = minusc xs

maiusc [] = []
maiusc (x:xs) = if isUpper x then x : result else result
    where result = maiusc xs

minuscMaiusc xs = (minusc xs, maiusc xs)

minuscMaiusc xs = ([a | a <- xs, a isLower], [b | b <- xs, b isUpper])

fizzBuzz [] = []
fizzBuzz (x:xs) = if rem x 3 == 0 && rem x 5 == 0 then "FizzBuzz" : fizzBuzz xs
    else if rem x 3 == 0 then "Fizz" : fizzBuzz xs
    else if rem x 5 == 0 then "Buzz" : fizzBuzz xs
    else (show x) : fizzBuzz xs