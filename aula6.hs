menor [x] = x 
menor (x:xs) = if x<menor xs then x else menor xs
{- 
exemplo de caso: 
menor [2,1,3,4,5] = 2<1 (false) 
					menor [1,3,4,5]
					1<menor[3,4,5]
					1 < 3 < menor [4,5]
					1 < 3 < 4 < menor[5]
					1 < 3 < 4 < 5 
					1
-}

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)	