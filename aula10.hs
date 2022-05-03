inverso [] = []
inverso ls = last ls : inverso (init ls)

inverso' [] = []
inverso' (x:xs) = inverso' xs ++ [x]

{- O operador "++" junta duas listas. 
O operador : junta um elemento (cabeça) e uma lista (cauda)-}

nultimos 0 _ = []
nultimos n [] = []
nultimos n xs = inverso (take n (inverso xs))

dobrolista [] = []
dobrolista (x:xs) = 2*x : dobrolista xs

dobrolista' = [2*x | x <- [1..100]]

dobrolistapares = [2*x | x <- [1..100], even x]

tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

tamanho' xs = sum [1 | _ <- xs]

umlista [] = []
umlista (x:xs) = x ++ umlista xs

somalistas [] [] = []
somalistas [] _ = []
somalistas _ [] = []
somalistas (x:xs) (y:ys) = x + y : somalistas xs ys