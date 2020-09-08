-- Exercicio 1
leNomeEnd :: IO (String,String)
leNomeEnd = do
   putStrLn "Digite seu nome:"
   nome <- getLine
   putStrLn "Digite seu Endereço:"
   end <- getLine
   return (nome,end)

-- Exercicio 2

leNNE :: Int-> IO [(String,String)]
leNNE 0 = return []
leNNE n = do
    r <- leNomeEnd
    lr <- leNNE (n-1)
    return (r:lr)

-- Exercicio 3

leNumero ::  IO Int
leNumero = do
         putStr "Digite um número: "
         n <- getLine
         return (read n)

executaN :: Int -> IO a -> IO [a]
executaN 0 io = return []
executaN n io = do
             r <- io
             lr <- executaN (n-1) io
             return (r:lr)

-- Exercicio 4
leNNE2 :: Int-> IO [(String,String)]
leNNE2 n = executaN n leNomeEnd


--- Exercicio 5
printLista :: [Int] -> IO ()
printLista [] = return ()
printLista (x:xs) = do
                   putStrLn (show x)
                   printLista xs

-- Exercicio 6

printListaComoVetor :: [Int] -> IO ()
printListaComoVetor l = plv 0 l

plv :: Int -> [Int] -> IO ()
plv n [] = return ()
plv n (x:xs) = do
        putStrLn ("lista[" ++ show n ++ "] = " ++ show x)
        plv (n+1) xs




-- Exercico 

maiorDeN :: Int -> IO ()
maiorDeN n = do
         putStrLn ("Digite " ++ show n ++ " números maiores do que zero!")
         lista <- executaN n leNumero
         putStrLn ("O maior número digitado é: " ++ show (foldr (\x y -> max x y) 0 lista))


