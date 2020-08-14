-- TEC IX - Programação Funcional - André Du Bois
-- Exercícios de Programação Haskell IV - Listas
-- Artur Melchiori Cerri

-- Exercício 1 --------------------------------------------------------
multDoisLista :: [Int] -> [Int]
multDoisLista [] = []
multDoisLista (x:xs) = 2*x : multDoisLista xs

-- Exercício 2 --------------------------------------------------------
tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

-- Exercício 3 --------------------------------------------------------
produtoLista :: [Int] -> Int
produtoLista [] = 1
produtoLista (x:xs) = x * produtoLista xs

-- Exercício 4 --------------------------------------------------------
andLista :: [Bool] -> Bool
andLista [] = True
andLista (x:xs) = x && andLista xs

-- Exercício 5 --------------------------------------------------------
concatLista :: [[Int]] ->[Int]
concatLista [] = []
concatLista (x:xs) = x ++ concatLista xs

-- Exercício 6 --------------------------------------------------------
inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (x:xs) = inverteLista xs ++ [x]