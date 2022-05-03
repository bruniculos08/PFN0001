{- uma lista qualquer pode ser dada por x:xs-}

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = [] 
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y): zip' xs ys

{- a função zip' recebe duas listas e retorna uma lista de duplas (na 1ª linha está 
a assinatura); os casos base na 2º e 3º linha (se receber um dado vazio como a ou b), assim quando acabam os elementos
de uma lista a função para, pois nessa linha não há recusividade; 
na 4º linha o processo que ocorre, onde duas listas com suas caudas são iseridas e é retornado
o elemento cabeça de cada lista em um tupla dupla e após isso a função é recursiva chamando a si 
própria com suas caudas com input-}   

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])

{-unzip' ((x,y): xys) = (x:fst (unzip' xys), y:snd(unzip' xys))-}

{- a função unzip' pode também ser escrita como: -} 

unzip' ((x,y):xys) = (x:xs, y:ys)
	where
		(xs, ys) = unzip' xys
		

{- a função  unzip' pega uma lista de duplas tuplas e retorna duas listas (o processo contrário da zip'); 
na 2º linha está o caso base, se receber uma lista vazia retorna duas listas vazias, assim a função irá parar quando
a lista acabar pois não há recursividade nessa linha; na 3º linha a função 
pega a lista de duplas tuplas e sua cauda e transforma e retorna a lista do x com cauda do primeiro elemento obtido 
com a recursão da função, o mesmo acontece com o y mas esse pega o segundo elemento da recursão da função pois esse
estará na mesma posição de y-}

{- a função unzip' escrita da segunda maneira é mais simples de se vizualizar, ela pega uma lista de tuplas duplas
formas por x,y em cada tupla, e retorna uma lista com os x e outra com os y mas para terminar a lista foi necessário 
definir o que será xs e ys através do where, -}


