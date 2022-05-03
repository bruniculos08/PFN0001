module Lista2 where

pertence _ [] = False
pertence n (x:xs) = if x == n then True 
    else pertence n xs

intercessao _ [] = []
intercessao [] _ = []
intercessao (x:xs) (y:ys) = if pertence x (y:ys) == True then x : intercessao xs ys
    else intercessao xs (y:ys)


inversoLista [] = []
inversoLista (x:xs) = inversoLista xs ++ [x]


nUltimos _ [] = []
nUltimos n (x:xs) = inversoLista (take n (inversoLista (x:xs)))


enesimo n (x:xs) = if n <= 1 then x
    else if n > length(x:xs) then -1
    else enesimo (n-1) xs



repetir 0 _ = []
repetir n m = m : repetir (n-1) m

intercalacao [] [] = []
intercalacao (x:xs) [] = x : intercalacao xs []
intercalacao [] (y:ys) = y : intercalacao [] ys
intercalacao (x:xs) (y:ys) = if x < y then x : intercalacao xs (y:ys)
    else if x > y then y : intercalacao (x:xs) ys
    else x : y : intercalacao xs ys

menor [x] = x
menor (x:xs) = if x <= menor xs then x
    else menor xs

removerElem _ [] = []
removerElem n (x:xs) = if x == n then xs
    else x : removerElem n xs



ordenarLista [] = []
ordenarLista (x:xs) = menor (x:xs) : ordenarLista (removerElem (menor (x:xs)) (x:xs))


insereElem n [] = [n]
insereElem n (x:xs) = if n < x then n : (x:xs)
    else if n > x then x : insereElem n xs
    else (x:xs)



primeirosDuplas [] = []
primeirosDuplas ((x, y):xs) = x : primeirosDuplas xs


somaDuplas [] = []
somaDuplas ((x, y):xs) = x + y : somaDuplas xs




menoresDuplas [] = []
menoresDuplas ((x, y):xs) = if x < y then (x, y) : menoresDuplas xs
    else menoresDuplas xs

separarDuplas v (x:xs) = (menoresv v (x:xs), maioresv v (x:xs))

menoresv v (x:xs) = [a | a <- (x:xs), a <= v]
maioresv v (x:xs) = [b | b <- (x:xs), b > v]

mdc (a,0) = a 
mdc (a, b) = if b > 0 then mdc (b,(rem a b))
    else a


inversoDupla [] = []
inversoDupla ((x, y):xs) = (y, x) : inversoDupla xs




simetrico [] = []
simetrico ((x,y):xs) = if x == y then True : simetrico xs
    else False : simetrico xs

pares a = ([(x, y) | x <- [1..a], y <- [1..a], y /= x])





inverteDNA (x:xs) = inversoLista(inverteDNA' (x:xs))

inverteDNA' [] = []
inverteDNA' (x:xs) = if x == 'A' then 'T' : inverteDNA' xs
    else if x == 'T' then 'A' : inverteDNA' xs
    else if x == 'G' then 'C' : inverteDNA' xs
    else 'G' : inverteDNA' xs



coins = [50, 20, 10, 5]


trocoCafe a b = trocoCafe' (b-a) coins

trocoCafe' _ [] = []
trocoCafe' n (x:xs) = if n == 0 then []
    else if div n x > 0 then (x, div n x) : trocoCafe' (rem n x) xs
    else trocoCafe' n xs

magica [] = []
magica l1 = magica' l1 ++ tail (inversoLista (magica' l1))

magica' [] = []
magica' (x:xs) = repetir (length(x:xs)) x ++ magica' xs