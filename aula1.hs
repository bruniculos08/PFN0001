f x = x + 1

maior a b = if a>b then a else b 


maior3 a b c = if a>b then (if a>c then a else c) 
			   else (if b>c then b else c) 
			   {- os parenteses são opcionais-}
			   
bigger3 a b c = maior (maior a b) c 


negacao x = if x == True then False else True
{- pode ser apenas if x == True then False else True
ou pelo modo de "Casamento de Padrão": 
negacao False = True
negacao True = False -}

