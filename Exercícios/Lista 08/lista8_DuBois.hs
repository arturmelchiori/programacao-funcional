-- Exercicio 1
aplicaDuasVezes :: (Int -> Int) -> Int -> Int
aplicaDuasVezes f x = f (f x)

-- ou aplicaDuasVezes f = f . f

-- Exercicio 2
vendaTotal :: (Int -> Int) -> Int -> Int
vendaTotal f 0 = f 0
vendaTotal f n = f n + vendaTotal f (n-1)

-- Exercicio 3
foldInt :: (Int -> Int -> Int) -> [Int] -> Int
foldInt f [x] = x
foldInt f (x:xs) = f x (foldInt f xs)

-- Exercicio 4
filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString f [] = []
filterString f (x:xs)
    | f x          = x : filterString f xs
    | otherwise    = filterString f xs

-- Exercicio 5
somaQuadrado :: [Int] -> Int
somaQuadrado l = foldInt soma (mapInt quadrado l)
    where
      soma :: Int -> Int -> Int
      soma x y = x+y
      quadrado :: Int -> Int
      quadrado x = x*x

-- ou simplesmente:
-- somaQuadrado :: [Int] -> Int
-- somaQuadrado = foldInt (+) . mapInt (^2) 

mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f (x:xs) = f x : mapInt f xs

-- Exercicio 6

iter :: Int -> (Int -> Int) -> Int -> Int
iter 1 f x = f x
iter n f x = f (iter (n-1) f x)
