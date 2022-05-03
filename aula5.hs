tamanho :: [a] -> Int 
tamanho [] = 0 
tamanho (x:xs) = 1 + tamanho xs 

somatorio ::(Num a)=>[a]->a
somatorio [] = 0 
somatorio (x:xs) = x + somatorio xs 

nprimeiros :: Int -> [a] -> [a] 
nprimeiros 0 _ = []
nprimeiros _ [] = [] 
nprimeiros n (x:xs) = x : nprimeiros (n-1) xs
