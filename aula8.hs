{-listas -> [1, 2, 3, 4, 5, 6], a lista tem 4 funções relacionadas
head = primeiro elemento da lista, tail = todos os elementos menos o da cabeça
init = todos os elementos menos o último, last = último elementos de lista -}


head' :: [a]->a {-lista com elemento tipo a, retorna um elemento do tipo a-}
head' (x:xs) = x   {-cabeça chamada de x e causa chama xs como 1:(2:3:[])-}

tail' (x:xs) = xs 

init' (x:[]) = []
init' (x:xs) = x : init' xs

{-os ":" e o "[]" são construtores de dados, o : cria uma lista com elementos e o [] cria 
uma lista vazia-}

last' (x:[]) = x 
last' (x:xs) = last' xs

maior (x:[]) = x
maior (x:xs) = if x > maior xs then x
               else maior xs

nprimeiros 0 _ = []
nprimeiros _ [] = []
nprimeiros n (x:xs) = x : nprimeiros (n-1) xs

nultimos 0 _ = []
nultimos _ [] = []
nultimos n (x:xs) = if length (x:xs) - n > 0 then nultimos n xs
                    else x : nultimos n xs
{-nultimos n ls@(x:xs) | length ls <= n = ls
                    | otherwise = nultimos n xs-}

nelemento n (x:xs) = if length (x:xs) - n > 0 then nelemento (n) xs
                    else x : []

nelemento' 0 _ = []
nelemento' n (x:xs) = nelemento (length(x:xs)+1-n) (x:xs) ++ nelemento' (n-1) (x:xs)


{-fazer uma função que traz o inverso de uma lista.
o 'operador' "++"permite chamar mais de uma função.-}

inverso [] = []
inverso (x:xs) = nelemento' (length (x:xs)) (x:xs)
