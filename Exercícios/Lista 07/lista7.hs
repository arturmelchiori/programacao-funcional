-- TEC IX - Programação Funcional - André Du Bois
-- Exercícios de Programação Haskell 7 - Listas e Tuplas
-- Artur Melchiori Cerri

-- Exercício 1 --------------------------------------------------------
somaQuadrupla :: [(Int,Int,Int,Int)] -> Int
somaQuadrupla [] = 0
somaQuadrupla (x:xs) = a + b + c + d + somaQuadrupla xs
            where
                (a,b,c,d) = x

-- Exercício 2 --------------------------------------------------------
somaTuplas :: [((Int,Int),(Int,Int))] -> Int
somaTuplas [] = 0
somaTuplas (x:xs) = a + b + c + d + somaTuplas xs
            where
                ((a,b),(c,d)) = x

-- Exercício 3 --------------------------------------------------------
zipp :: [Int] -> [Int] -> [(Int,Int)]
zipp [] [] = []
zipp x [] = []
zipp [] y = []
zipp (x:xs) (y:ys) = (x,y) : zipp xs ys

-- Exercício 4 --------------------------------------------------------
zipp3 :: [Int] -> [Int] -> [Int] -> [(Int,Int,Int)]
zipp3 [] [] [] = []
zipp3 x [] [] = []
zipp3 x y [] = []
zipp3 x [] z = []
zipp3 [] y z = []
zipp3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zipp3 xs ys zs

-- Exercício 5 --------------------------------------------------------
unZipp :: [(Int,Int)] -> ([Int], [Int])
unZipp [] = error "Lista vazia."
unZipp x  = (unzipEsq x, unzipDir x)
            where
                unzipEsq :: [(Int,Int)] -> [Int]
                unzipEsq [] = []
                unzipEsq ((a,b):xs) = a : unzipEsq xs
        
                unzipDir :: [(Int,Int)] -> [Int]
                unzipDir [] = []
                unzipDir ((a,b):xs) = b : unzipDir xs