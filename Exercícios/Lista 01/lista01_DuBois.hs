-- 1
osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais x y z w = (x == y) && ( y == z) && (z == w)


--2
quantosIguais :: Int -> Int -> Int -> Int
quantosIguais x y z 
  | (x == y) && (y==z)                  = 3
  | (x == y) || (y == z) || (x == z)    = 2
  | otherwise                           = 0

-- 3
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes x y z  = x/=y && y/=z && x/=z

-- 4 Na definiçao dada, faltava comparar "x" com "z". O objetivo aqui
-- era que vocês verificassem se a solução do exercício anterior estava correta 

-- 5
todosIguais :: Int -> Int -> Int -> Bool
todosIguais x y z = x == y && y == z

quantosSaoIguais2 :: Int -> Int -> Int -> Int
quantosSaoIguais2 x y z 
  | todosIguais x y z        = 3 
  | todosDiferentes x y z    = 0  
  | otherwise                = 2 

--8 
elevadoDois :: Int -> Int 
elevadoDois x = x * x 

--9
elevadoQuatro :: Int -> Int 
elevadoQuatro x = elevadoDois x  *  elevadoDois x 

-- elevadoQuatro x = elevadoDois (elevadoDois x) 

--10
vendas :: Int -> Int 
vendas 0 = 0
vendas 1 = 3
vendas 2 = 2
vendas 3 = 4
vendas 4 = 2

vendaTotal :: Int -> Int
vendaTotal 0  = vendas 0
vendaTotal n  = vendas n + vendaTotal (n-1)  

