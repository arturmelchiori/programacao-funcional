-- TEC IX - Programação Funcional - André Du Bois
-- Exercícios de Programação Haskell III - Tuplas
-- Artur Melchiori Cerri

-- Exercício 1 --------------------------------------------------------
somaTuplas :: ((Int,Int),(Int,Int)) -> Int
somaTuplas ((a,b),(c,d)) = a + b + c + d

-- Exercício 2 --------------------------------------------------------
shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((a, b),c) = (a,(b,c))

-- Exercício 3 --------------------------------------------------------
mini :: Int -> Int -> Int -> Int
mini x y z
    | (x<=y && x<=z) = x
    | (y<=x && y<=z) = y
    | otherwise = z

maxi :: Int -> Int -> Int -> Int
maxi x y z
    | (x>=y && x>=z) = x
    | (y>=x && y>=z) = y
    | otherwise = z

minEmax :: Int -> Int -> Int -> (Int, Int)
minEmax x y z = (maxi x y z, mini x y z)

-- Exercício 4 --------------------------------------------------------
vendas :: Int -> Int
vendas 0 = 6
vendas 1 = 10
vendas 2 = 0
vendas 3 = 7
vendas 4 = 9
vendas 5 = 11
vendas _ = 0

zeroVenda :: Int -> (Int, Bool)
zeroVenda n
    | (n == -1) = (-1, False)
    | (vendas n == 0) = (n, True)
    | otherwise = zeroVenda (n-1)

-- Exercício 5 --------------------------------------------------------
type Livro = (String, String, Int)

titulo :: Livro -> String
titulo (t,a,i) = t

autor :: Livro -> String
autor (t,a,i) = a

isbn :: Livro -> Int
isbn (t,a,i) = i

witcher :: Livro
witcher = ("The Last Wish", "Andrzej Sapkowski", 9785170429172)