take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take (n-1) xs 

maiores10 (x:xs) = if x > 10 then x : maiores10 xs 
                   else maiores10 xs
