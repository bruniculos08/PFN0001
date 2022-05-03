dimensao_comodo = (3, 5)
comodo_casa = ("sala", 3, 5)
comodos_casa = [("sala", 3, 5), ("cozinha", 2, 4), ("quarto", 2.5, 2), ("banheiro", 2, 2.4), ("corredor", 1.2, 2)]

{-- Lista tem tamanho flexível mas tipo rígido.
Tuplas tem tamanho rígido e tipo flexível. --}

t1 = fst dimensao_comodo
t2 = snd dimensao_comodo

fst' (x, y) = x
snd' (x, y) = y

fst3 (x, y, z) = x
snd3 (x, y, z) = y
thr3 (x, y, z) = z

{--acessar_altura_comodo [] _ = error("Cômodo não existente!")
acessar_altura_comodo casa@(x:xs) pos_comodo | pos_comodo > 0 = acessar_altura_comodo xs (pos_comodo - 1)
  | otherwise = head x--}

{--acessar_altura_comodo [] _ = error("Cômodo não existente!")
acessar_altura_comodo (x:xs) comodo | fst3 x == comodo = snd3 x
  | otherwise = acessar_altura_comodo xs comodo--}

acessar_altura_comodo [] _ = error ("Cômodo não existente!")
acessar_altura_comodo (x:xs) comodo = if fst3 x == comodo then snd3 x
    else acessar_altura_comodo xs comodo

alterar_altura_comodo [] _ _ = error ("Cômodo não existente!")
alterar_altura_comodo (x:xs) comodo nova_altura = if (fst3 x) == comodo then (fst3 x, nova_altura, thr3 x)
    else alterar_altura_comodo xs comodo nova_altura

{-- Exercício 1
Considere uma coleção para armazenar os hóspedes (nome e ano) de cada um dos quartos de
um hotel, como você definiria essa estrutura?--}

hospedes = [("Quarto 1", [("Ana", 2008),("João", 2009)])]