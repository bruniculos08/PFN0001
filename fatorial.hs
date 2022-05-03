fat1 0 = 1 
fat1 n = n * fat1 (n-1)

fat2 n = if n == 0 then 1 else n * fat2 (n-1)

fat3 n | n == 0 = 1 
       | otherwise = n * fat3 (n-1)

fat4 :: (Eq p, Num p) => p -> p
fat4 n | n == 0 = 1 
       | True    = n * fat4 (n-1)

{- true e otherwise terão mesmo valor, e esse tipo de 
função é de casamento de padrão -}

somai 0 = 1 
somai n = 2 ** n + somai (n-1)


multi n 0 = 0
multi n m = n + multi n (m-1) 

multi' a b = if b == 0 then 0 else a + multi' a (b-1)

multi'' a b | b == 0 = 0 
            | otherwise = a + multi'' a (b-1)

fib 0 = 0 
fib 1 = 1 
fib n = fib(n-2) + fib (n-1)

fib' n | n == 0 = 0
       | n == 1 = 1 
       | otherwise = fib'(n-2) + fib'(n-1)



