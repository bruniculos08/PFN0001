{--dobro [] = []
dobro (x:xs) = 2*x:dobro xs--}

dobro [x] = [2*x]
dobro (x:xs) = x:dobro xs

glista l1 l2 = glista' l1 l2
    where
        glista' [] _ = [] 
        glista' (x:xs) [] = glista' xs l2
        glista' (x:xs) (y:ys) = (x, y) : glista' (x:xs) ys 


menores _ [] = []
menores n (x:xs) = if x < n then x : menores n xs 
                   else menores n xs


partition _ [] = ([], [])
partition g (x:xs) = let (ts, fs) = partition g xs in if g x then (x:ts, fs) else (ts, x:fs)


par x  = x rem 2 == 0

pares [] = []
pares (x:xs) = if par x then x:pares xs 
               else pares xs


map' _ [] = []
map' g (x:xs) = g x : map' g xs


map'' g xs = [g x | x <- xs]


foldr' _ e [] = e
foldr' g e (x:xs) = g x (foldr' g e xs)

foldl' _ e [] = e
foldl' g e (x:xs) = foldl' g (g e x) xs

{- filter (varre uma lista e retorna uma lista com os elementos de acordo), talvez usar no trabalho, reduce, map, fold-}

{-pares-}

pares' [] = []
pares' (x:xs) = if even x then x : pares' xs
                else pares' xs

pares'' xs = [x | x <- xs, even x == True ]

pares'''' xs = filter even xs

pares''''' xs = filter (\x -> even x) xs

filter' g (x:xs) = if g x then x : filter' g xs
                   else filter' g xs
filter'' g xs = [x | x <- xs, g x == True]

len' [] = 0
len' (x:xs) = 1 + len' xs

{- prova: questões parecidas da lista de exercício + de explicar o funcionamento de uma função, dia 13, durante -}