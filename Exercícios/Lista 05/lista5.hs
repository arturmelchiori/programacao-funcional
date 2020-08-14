-- TEC IX - Programação Funcional - André Du Bois
-- Exercícios de Programação Haskell V - Listas II
-- Artur Melchiori Cerri

-- Exercício 1 --------------------------------------------------------
membro :: Int -> [Int] -> Bool
membro n [] = False
membro n (x:xs)
    | (x == n) = True
    | otherwise = membro n xs

-- Exercício 2 --------------------------------------------------------
membroNum :: Int -> [Int] -> Int
membroNum n [] = 0
membroNum n (x:xs)
    | (x == n) = 1 + membroNum n xs
    | otherwise = membroNum n xs

-- Exercício 3 --------------------------------------------------------
membro2 :: Int -> [Int] -> Bool
membro2 n (x:xs)
    | (membroNum n (x:xs) == 0) = False
    | otherwise = True

-- Exercício 4 --------------------------------------------------------
-- Função para remover elemento da lista a ser utilizada na resolução
remove :: Int -> [Int] -> [Int]
remove n [] = []
remove n (x:xs)
    | (n == x) = remove n xs
    | otherwise = x : remove n xs

unico :: [Int] -> [Int]
unico [] = []
unico (x:xs)
    | (membroNum x (x:xs) == 1) = x : unico xs
    | (membroNum x (x:xs) > 1) = unico (remove x (x:xs))

-- Exercício 5 --------------------------------------------------------
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort (menores x xs) ++ [x] ++ quickSort (maiores x xs)

menores :: Int -> [Int] -> [Int]
menores n [] = []
menores n (x:xs)
    | x < n = x : menores n xs 
    | otherwise = menores n xs

maiores :: Int -> [Int] -> [Int]
maiores n [] = []
maiores n (x:xs)
    | x >= n = x : maiores n xs 
    | otherwise = maiores n xs