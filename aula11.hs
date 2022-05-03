uns = 1 : uns
pares = pares' 0
pares' n = n : pares' (n+2)

{--

Exemplos de listas:

[x/y| x <-[10,20,30], y <-[1,2,3]]

take 10 [take n uns| n <-[0..]]

Declarar uma lista que apresente todas as possibilidades de strings formadas pelas
letras de "a" a "z" com tamanho de 3 caracteres.
--}