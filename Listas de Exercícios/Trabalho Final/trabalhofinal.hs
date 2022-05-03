tarefa = [("Tarefa A",1),("Tarefa X",2),("Tarefa G",7),("Tarefa C",2),("Tarefa K",4)]


geraPosicaoDosItens [] _ = []
geraPosicaoDosItens (x:xs) p = (fst x, snd x, p) : geraPosicaoDosItens xs (p+1) 

ordenaListaPelaPrioridade [] = []
ordenaListaPelaPrioridade (x:xs) = menor (x:xs) : ordenaListaPelaPrioridade (removerElem (menor (x:xs)) (x:xs))

atualizaPrioridade [] _ = []
atualizaPrioridade (x:xs) p = if snd' x > p then (fst' x, p+1, thr x) : atualizaPrioridade xs (p+1) 
                               else if snd' x == p then (fst' x, p, thr x) :  atualizaPrioridade xs p
                               else (fst' x, snd' x, thr x) : atualizaPrioridade xs p

ordenaListaPelaOrdem [] = []
ordenaListaPelaOrdem (x:xs) = menor' (x:xs) : ordenaListaPelaOrdem (removerElem (menor' (x:xs)) (x:xs))

imprimeListaAtualizada xs = ordenaListaPelaOrdem (atualizaPrioridade (ordenaListaPelaPrioridade (geraPosicaoDosItens xs 1)) 1)


{- funções essenciais -}
fst' (a, b, c) = a
snd' (a, b, c) = b
thr (a, b, c) = c

menor [x] = x
menor (x:xs) = if snd' x <= snd' (menor xs) then x 
               else menor xs

menor' [x] = x
menor' (x:xs) = if thr x <= thr (menor' xs) then x 
               else menor' xs

removerElem _ [] = []
removerElem n (x:xs) = if x == n then xs
    else x : removerElem n xs

ordenarLista [] = []
ordenarLista (x:xs) = menor (x:xs) : ordenarLista (removerElem (menor (x:xs)) (x:xs))

{- fim de funções essenciais -}