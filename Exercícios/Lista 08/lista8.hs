-- TEC IX - Programação Funcional - André Du Bois
-- Exercícios de Programação Haskell 8 - Funções de Alta Ordem
-- Artur Melchiori Cerri

-- Exercício 1 --------------------------------------------------------
aplicaDuasVezes :: (Int -> Int) -> Int -> Int
aplicaDuasVezes f x = f (f x)

-- Funções teste de exemplo do exercício 1
incrementa :: Int -> Int
incrementa x = x + 1
-- 12

dobra :: Int -> Int
dobra x = x*x
-- 10000

-- Exercício 2 --------------------------------------------------------
-- Função vendas (mesma utilizada em listas anteriores)
vendas :: Int -> Int
vendas 0 = 6
vendas 1 = 10
vendas 2 = 12
vendas 3 = 7
vendas 4 = 9
vendas _ = 10

vendaTotal :: (Int -> Int) -> Int -> Int
vendaTotal f 0 = vendas 0
vendaTotal f x = f x + vendaTotal f (x-1)

-- Exercício 3 --------------------------------------------------------
foldInt :: (Int -> Int -> Int) -> [Int] -> Int
foldInt f [] = 0
foldInt f [x] = x
foldInt f (a:b:xs) = f (f a b) (foldInt f xs)

soma :: Int -> Int -> Int
soma x y = x + y

mult :: Int -> Int -> Int
mult x y = x * y

-- Exercício 4 --------------------------------------------------------
filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString f [] = []
filterString f (x:xs)
    | (f x == False) = filterString f xs
    | otherwise = x : filterString f xs

naoEspaco :: Char -> Bool
naoEspaco x = x /= ' '

-- Exercício 5 --------------------------------------------------------
mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f (x:xs) = f x : mapInt f xs

somaQuadrado :: [Int] -> Int
somaQuadrado [x] = quadrado x
somaQuadrado l = foldInt soma (mapInt quadrado l)

quadrado :: Int -> Int
quadrado x = x * x

-- Exercício 6 --------------------------------------------------------
iter :: Int -> (Int -> Int) -> Int -> Int
iter 0 f x = error "Entrada inválida. Impossível realizar zero iterações."
iter 1 f x = f x
iter n f x = f (iter (n-1) f x)
