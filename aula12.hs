altura_familia = [1.72, 1.67, 1.73, 1.75]

acessar_altura_fam :: (Ord t, Num t) => [p] -> t -> p
acessar_altura_fam [] _ = error("Não existem registros")
acessar_altura_fam fam@(x:xs) pos_fam | pos_fam > 0 = acessar_altura_fam xs (pos_fam -1)
    | otherwise = x

{- o "pos_fam é a posição do item na lista ou seja, é um número dado que é subtraido (-1) em cada recursão até que chegue a 0, -}

casa_tipo_comodos = ["sala", "cozinha", "quarto", "banheiro", "corredor"]

casa_inf_area_comodos = [ [3,5], [2,4], [2.5,2], [2,2.4], [1.2,2]]

acessar_altura_comodo [] _ = error("Cômodo inexistente!")
acessar_altura_comodo casa@(x:xs) pos_comodo = if pos_comodo > 1 then acessar_altura_comodo xs (pos_comodo - 1)
    else head x

acessar_alturas_casa [] = []
acessar_alturas_casa casa@(x:xs) = head x : acessar_alturas_casa xs

alterar_altura_comodo [] _ _ = []
alterar_altura_comodo casa@(x:xs) pos_comodo a | pos_comodo > 1 = x : (alterar_altura_comodo xs (pos_comodo - 1) a)
    | otherwise = [a, last x] : xs

{- o "a" é a informação que será colocada e o "pos_comodo" é a posição da informação que será trocada por essa nova informação
e "last x" é o último elemento da lista que está sendo alterada dentro da tupla (pois esse elemento não é alterado)-}

calcular_casa_m2_comodos [] = []
calcular_casa_m2_comodos casa@(x:xs) = (head x)*(last x) : calcular_casa_m2_comodos xs

mostrar_nome_dimensoes_comodos [] [] = []
mostrar_nome_dimensoes_comodos [] _ = []
mostrar_nome_dimensoes_comodos _ [] = []
mostrar_nome_dimensoes_comodos tipo@(x:xs) al@(y:ys) = mensagem
    where mensagem = head x ++ "tem" ++ show(head y) ++ "de altura e" ++ show(last y) ++ "de largura"