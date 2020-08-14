-- 1

membro :: Int -> [Int] -> Bool
membro n [] = False
membro n (x:xs)
    | n == x = True
    | otherwise = membro n xs

--2
membroNum :: Int -> [Int] -> Int
membroNum n [] = 0
membroNum n (x:xs)
    | n == x = 1 + membroNum n xs
    | otherwise = membroNum n xs

--3
membro2 :: Int -> [Int] -> Bool
membro2 n l = membroNum n l > 0

--4

unico :: [Int] -> [Int]
unico l = unico2 l l
  where
   unico2 :: [Int] -> [Int] -> [Int]
   unico2 [] l = []
   unico2 (x:xs) l
      | membroNum x l == 1 = x: unico2 xs l
      | otherwise          = unico2 xs l

-- 5
quickSort :: [Int] -> [Int]     
quickSort [] = []
quickSort (x:xs) =  quickSort (menores x xs) ++ [x] ++ quickSort (maiores x xs)

menores :: Int -> [Int] -> [Int]
menores n [] = []
menores n (x:xs) 
    | x <= n = x: menores n xs
    | otherwise = menores n xs    
                                                                                                    
maiores :: Int -> [Int] -> [Int]
maiores n [] = []
maiores n (x:xs) 
    | x > n = x : maiores n xs
    | otherwise = maiores n xs