-- Exercicio 1
somaQuadrupla :: [(Int, Int, Int, Int)] -> Int
somaQuadrupla [] = 0
somaQuadrupla ((a,b,c,d):xs) = a + b + c + d + somaQuadrupla xs

-- Exercicio 2
somaTuplas :: [((Int, Int), (Int, Int))] -> Int
somaTuplas [] = 0
somaTuplas (((a,b),(c,d)):xs) = a + b + c + d + somaTuplas xs

-- Exercicio 3
zipp :: [Int] -> [Int] -> [(Int, Int)]
zipp [] l = []
zipp l [] = []
zipp (x:xs) (y:ys) = (x,y) : zipp xs ys

-- exercício 4
zipTres :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
zipTres [] _  _ = []
zipTres _ [] _  = []
zipTres _ _  [] = []
zipTres (x:xs) (y:ys) (z:zs) = (x,y,z) : zipTres xs ys zs

-- exercício 5
unZipp :: [(Int, Int)] -> ([Int], [Int])
unZipp l = (unzipEsq l, unzipDir l)
  where
    unzipEsq :: [(Int, Int)] -> [Int]
    unzipEsq [] = []
    unzipEsq ((a,b):xs) = a : unzipEsq xs
    unzipDir :: [(Int, Int)] -> [Int]
    unzipDir [] = []
    unzipDir ((a,b):xs) = b : unzipDir xs
