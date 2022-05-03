fat 0 = 1 
fat n = n*fat(n-1)
{- é o fatorial na matemática-}

expoente n 0 = 1 
expoente n m = n * expoente n (m-1)
{- é a exponenciação
por exemplo expoente 2 2 = 2 * expoente 2 1	
				2 * expoente 2 * expoente 2 0 
				2 * 2 * 1					-}

{-mdc a b = if a == b then a else if a>b then mdc (a-b) b else mdc a (b-a)-}
{- máximo divisor comum que também pode ter a função descrita 
de uma maneira mais parecida com a matemática 

mdc a b | a == b  = a 
		| a > b  = mdc (a-b) b 
		| otherwise = mdc a (b-a) 
		
essa maneira é a através de guardas, onde a primeira expressão 
verdadeira é executada 

-}

mdc a b | a == b  = a 
		| a > b  = mdc (a-b) b 
		| otherwise = mdc a (b-a)
 