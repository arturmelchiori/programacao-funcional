-- 1

somaTuplas :: ((Int,Int),(Int,Int)) -> Int
somaTuplas ((a,b),(c,d)) = a + b + c + d

-- 2

shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((a,b),c) = (a,(b,c))

-- 3

maxi :: Int -> Int -> Int
maxi a b
    | a >= b    = a
    | otherwise = b

mini :: Int -> Int -> Int
mini a b
    | a <= b    = a
    | otherwise = b


minEmax :: Int -> Int -> Int -> (Int, Int)
minEmax a b c = (mini (mini a b) c, maxi(maxi a b) c)

-- 4


vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 4
vendas 2 = 0
vendas 3 = 66
vendas _ = 33



zeroVenda :: Int -> (Int, Bool)
zeroVenda 0
    | vendas 0 == 0 = (0, True)
    | otherwise     = (-1, False)
zeroVenda n
    | vendas n == 0 = (n, True)
    | otherwise     = zeroVenda (n-1)


-- 5
type Livro = (String,String,Int)

titulo :: Livro -> String
titulo (a,b,c) = a

autor :: Livro -> String
autor (a,b,c) = b

isbn :: Livro -> Int
isbn (a,b,c) = c