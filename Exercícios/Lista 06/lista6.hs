-- TEC IX - Programação Funcional - André Du Bois
-- Exercícios de Programação Haskell 6 - Listas III
-- Artur Melchiori Cerri

-- Exercício 1 --------------------------------------------------------
pegaPosicao :: Int -> [Int] -> Int
pegaPosicao n (x:xs)
    | (n == 1) = x
    | otherwise = pegaPosicao (n-1) xs

-- Exercício 2 --------------------------------------------------------
pega :: Int -> [t] -> [t]
pega n [] = []
pega n (x:xs) 
    | (n == 0) = []
    | (n == 1) = [x]
    | otherwise = x : pega (n-1) xs

-- Exercício 3 --------------------------------------------------------
retira :: Int -> [t] -> [t]
retira n [] = []
retira n (x:xs)
    | (n == 0) = []
    | (n == 1) = xs
    | otherwise = retira (n-1) xs

-- Exercício 4 --------------------------------------------------------
mediaLista :: [Int] -> Float
mediaLista [] = 0
mediaLista (x:xs) = fromIntegral(soma (x:xs)) / fromIntegral(tamanho (x:xs))

soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma xs

tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs


-- Exercício 5 --------------------------------------------------------
pegaMaiores :: Int -> [Int] -> [Int]
pegaMaiores n [] = []
pegaMaiores n (x:xs)
    | x > n = x : pegaMaiores n xs
    | otherwise = pegaMaiores n xs

-- Exercício 6 --------------------------------------------------------
contaMaiores :: Int -> [Int] -> Int
contaMaiores n [] = 0
contaMaiores n (x:xs)
    | x > n = 1 + contaMaiores n xs
    | otherwise = contaMaiores n xs

-- Exercício 7 --------------------------------------------------------
intercala :: [t] -> [t] -> [t]
intercala n [] = n
intercala [] m = m
intercala (x:xs) (y:ys) = x : y : intercala xs ys

-- Exercício 8 --------------------------------------------------------
dupli :: [Int] -> [Int]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- Exercício 9 --------------------------------------------------------
repli :: Int -> [Char] -> [Char]
repli _ [] = []
repli n (x:xs) = (repli2 n x) ++ (repli n xs) 
        where
            repli2 :: Int -> Char -> [Char]
            repli2 1 x = [x]
            repli2 n x = x : repli2 (n-1) x

-- Exercício 10 --------------------------------------------------------
dropEvery :: Int -> [Char] -> [Char]
dropEvery n l = drop n n l
        where
            drop :: Int -> Int -> [Char] -> [Char]
            drop _ _ [] = []
            drop a 1 (x:xs) = drop a a xs
            drop a b (x:xs) = x : drop a (b-1) xs

-- Exercício 11 --------------------------------------------------------
split :: Int -> String -> (String,String) 
split 0 x = ("",x)
split n x = ((pega n x), (retira n x))