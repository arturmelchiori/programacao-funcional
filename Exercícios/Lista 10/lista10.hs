{- TEC IX - Programação Funcional - André Du Bois
   Exercícios de Programação Haskell 10 - map, foldr, filter, takeWhile e dropWhile
   Artur Melchiori Cerri
-}

-- Exercício 1 -------------------------------------------------------- 
concatena :: [[t]] -> [t]
concatena xs = foldr (++) [] xs

-- Exercício 2 -------------------------------------------------------- 
andLista :: [Bool] -> Bool
andLista xs = foldr (&&) True xs

-- Exercício 3 -------------------------------------------------------- 
somaQuadPos :: [Int] -> Int
somaQuadPos xs = foldr (+) 0 (map (^2) (filter (\x -> x >= 0) xs))

-- Exercício 4 -------------------------------------------------------- 
somaListas :: Num t => [[t]] -> t
somaListas xs = foldr (+) 0 (map (foldr (+) 0) xs)

-- Exercício 5 -------------------------------------------------------- 
tamanhoListas :: [[t]] -> Int
tamanhoListas xs = foldr (+) 0 (map (\xs -> foldr (+) 0 (map (\x -> 1) xs)) xs)  

-- Exercício 6 -------------------------------------------------------- 
inverte :: [t] -> [t]
inverte xs = foldr (\x y -> y ++ [x]) [] xs

-- Exercício 7 --------------------------------------------------------
separaPalavras :: [Char] -> [[Char]]
separaPalavras [] = []
separaPalavras (' ':xs) = separaPalavras (dropWhile (== (' ')) (' ':xs))
separaPalavras xs = takeWhile (/= ' ') xs : separaPalavras (dropWhile (/= ' ') xs)