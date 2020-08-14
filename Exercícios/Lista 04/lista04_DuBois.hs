--  1
multDoisLista :: [Int] -> [Int]
multDoisLista [] = []
multDoisLista (x:xs) = 2*x : multDoisLista xs

-- 2
tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

-- 3
produtoLista :: [Int] -> Int
-- poderia ter o caso base: produtoLista [] = 1, ao invés desse:
produtoLista [x] = x
produtoLista (x:xs) = x * produtoLista xs

-- 4
andLista :: [Bool] -> Bool
-- poderia ter o caso base: andLista [] = True, ao invés desse:
andLista [x] = x
andLista (x:xs) = x && andLista xs

--  5
concatLista :: [[Int]] -> [Int]
concatLista [] = []
concatLista (x:xs) = x ++ concatLista xs

-- 6
inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (x:xs) = inverteLista xs ++ [x]