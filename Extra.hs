import System.IO
main = putStrLn "Hello Word!"
main' = getLine >>= (\nome -> putStrLn ("Alo" ++ nome))

main'' = putStr "Qual o seu nome?" >> getLine >>= (\nome -> putStrLn ("Alo" ++ nome))


-- putStr: imprime na tela; putStrLn: imprime na tela e pula uma linha; 
-- ">>" move para a próxima fução; ">>=" move e leva um parâmetro para a próxima função;
-- tipo da função main:
-- para gerar executável digitar "ghc nomedoarquivo.hs"


imprime nome = putStrLn ("Alo" ++ nome)


main1 = putStr "Qual o seu nome?" >> getLine >>= imprime
main2 = do putStr "Qual o seu nome?"
           hFlush stdout
           nome <- getLine
           imprime nome



-- outros exeplos | obs.: para se executar uma das funções em com compilação é preciso escrever esta sozinha em um arquivo

fat 0 = 1
fat n = n * fat(n-1)

mainv = do putStr "Digite o valor"
           hFlush stdout
           valor <- getLine
           putStrLn ("O resultado é" ++ show (fat (read valor)))


    
teste = putStr "Digite o valor" >> hFlush stdout >> getLine >>= (\valor -> putStrLn ("O resultado é" ++ show (fat (read valor))))

teste' = do putStr "Primeiro nome: "
            n1 <- getLine 
            putStrLn ("Segundo nome: ")
            n2 <- getLine
            if n1 < n2 then putStrLn n1 else putStrLn n2

-- o do serve para fazer um sintaxe que parece com um linguagem imperativa, mais simples

teste'' = putStr "Primeiro nome: " >> getLine >>= (\n1 -> putStr "Segundo nome: " >> getLine >>= (\n2 -> if n1 < n2 then putStrLn n1 else putStrLn n2))

-- perceba que o getline envie n1 e este então receberá a impressão que será seguida ">>" pelo resto da função 