{- TEC IX - Programação Funcional - André Du Bois
   Exercícios de Programação Haskell 9 - Polimorfismo de Tipos
   Artur Melchiori Cerri
-}

-- Exercício 1 -------------------------------------------------------- 
-- Tipo mais geral:
head2 :: [a] -> a
head2 (a:x) = a

-- Tipo mais geral:
tail2 :: [a] -> [a] 
tail2 (a:x) = x

-- Tipo mais geral:
fst2 :: (a, b) -> a 
fst2 (t,u) = t

-- Tipo mais geral:
shift :: ((a,b),c) -> (a,(b,c)) 
shift ((a,b),c) = (a,(b,c))

-- Exercício 2 --------------------------------------------------------
-- Tipo mais geral:
concatena :: [[a]] -> [a]
concatena [] = []
concatena (x:xs) = x ++ concatena xs

-- Exercício 3 --------------------------------------------------------
-- Tipo mais geral:
inverte :: [a] -> [a]
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]

-- Exercício 4 --------------------------------------------------------
-- Tipo mais geral:
zipp3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zipp3 [] x y = []
zipp3 x [] y = []
zipp3 x y [] = []
zipp3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zipp3 xs ys zs

-- Exercício 5 --------------------------------------------------------
{- Tipo mais geral: O tipo mais geral continua sendo como base (a -> b) -> [a] -> [b]
mas a função aplicada em (x+1) teria que ser um tipo numérico, 
como o interpretador anuncia, por isso o uso da constraint (Num a)
-}

mapMaisUm ::  (Num a) => (a -> b) -> [a] -> [b]
mapMaisUm f [] = []
mapMaisUm f (x:xs) = f (x+1) : mapMaisUm f xs

-- Exercício 6 --------------------------------------------------------
{- Usando como exemplo o operador (!!) (http://zvon.org/other/haskell/Outputprelude/EE_o.html)
que recebe uma lista e um Int e retorna o item da lista na posição Int, e é do tipo:
[t] -> Int -> t

Pela definição de foldr de tipo geral (a -> b -> b) -> b -> [a] -> b
temos que:

[t] -> Int -> t
(a -> b -> b)

a é do tipo [t], e b é do tipo t, resultado em algo do tipo geral:

([a] -> a -> a) -> a -> [[a]] -> a
-}