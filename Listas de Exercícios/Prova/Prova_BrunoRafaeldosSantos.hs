--read converte de caracter pra int e show de int para caracter
import Data.Char
multiplos3ou5 = multiplos3ou5' [x | x <- [1..1000], rem x 3 == 0 || rem x 5 == 0] 0
multiplos3ou5' [] n = n
multiplos3ou5' (x:xs) n = if rem x 3 == 0 || rem x 5 == 0 then multiplos3ou5' xs n+x
    else multiplos3ou5' xs n

velasBoloAniversario' [x] n = if x == n then 1 
                                 else 0
velasBoloAniversario' (x:xs) n = if x == n then 1 + velasBoloAniversario' xs n
                                 else velasBoloAniversario' xs n

velasBoloAniversario (x:xs) = velasBoloAniversario' (x:xs) (maior(x:xs)) 


maior [x] = x
maior (x:xs) = if x >= maior xs then x
                else maior xs

converteInt x = read (x) :: Int
ehchar x = if (isDigit x) then "sim"
           else "nao"

enesimo n (x:xs) = if n <= 1 then x
    else if n > length(x:xs) then -1
    else enesimo (n-1) xs

length' [] = 0
length' (x:xs) = 1 + length' xs


decimo (_,_,_,_,_,_,_,_,_,a) = a
nono (_,_,_,_,_,_,_,_,a,_) = a

tipohora [] = []
tipohora (x:xs) = if x == 'P' then "PM" 
                  else if x == 'A' then "AM"
                  else tipohora xs

converteint n = read(n) :: Int

converterHorario [] = []
converterHorario (x:y:xs) = if tipohora (x:y:xs) == "PM" then converterHorarioPM (x:[y]) ++ horasmin xs else converterHorarioAM (x:[y]) ++ horasmin xs

converterHorarioPM (x:y:xs) = converterHorarioPM' (converteint(x:[y]))
                                 

converterHorarioPM' n = if n == 12 then show(n)
                        else if n<12 && n>=1 then show(n+12)
                        else show(0) ++ show(n-12)

converterHorarioAM (x:y:xs) = converterHorarioAM' (converteint(x:[y]))

converterHorarioAM' n = if n == 0 then show(n+12)
                        else if n<12 && n>1 then show(n)
                        else show(0) ++ show(n-12)

test (x:y:xs) = (converteint(x:[y]))

horasmin [] = []
horasmin (x:xs) = if x /= 'P' && x /= 'A' && x /= 'M' then x : horasmin xs
                  else horasmin xs