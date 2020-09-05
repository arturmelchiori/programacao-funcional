-- Exercicio 1
concatena :: [[a]] -> [a]
concatena  = foldr (++) [] 

-- Exercico 2
andLista :: [Bool] -> Bool
andLista  = foldr (&&) True 

-- Exercicio 3
somaQuadPos :: [Int] -> Int
somaQuadPos = foldr (+) 0 . map (^2) . filter (>0) 
-- ou somaQuadPos l = foldr (+) 0 (map (^2) (filter (>0) l))

-- Exercicio 4
somaListas :: [[Int]] -> Int
somaListas  = foldr (+) 0 . map (foldr (+) 0) 
-- somaListas l = foldr (+) 0 (map (foldr (+) 0) l)
----somaListas l = foldr (+) 0 (foldr (++) [] l)

-- Exercicio 5
tamanhoListas :: [[a]] -> Int
tamanhoListas = foldr (+) 0 . map length 
-- ou tamanhoListas l = foldr (+) 0 (map length l)

-- Exercicio 6
inverte :: [a] -> [a]
inverte = foldr (\x y -> y ++[x]) []
-- ou
-- inverte  = foldr inv [] 
--     where
--        inv :: a -> [a] -> [a]
--        inv x y = y ++ [x]

-- Exercicio 7
separaPalavras :: String -> [String]
separaPalavras [] = []
separaPalavras (' ':xs) = separaPalavras xs
separaPalavras l = takeWhile (/=' ') l : separaPalavras(dropWhile (/=' ') l)

