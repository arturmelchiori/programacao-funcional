-- TEC IX - Programação Funcional - André Du Bois
-- Exercícios de Programação Haskell I
-- Artur Melchiori Cerri

-- Exercício 1
osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais x y z w = (x == y) && (y == z) && (z == w)

-- Exercício 2
quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais x y z
    | (x == y) && (y == z) = 3
    | (x == y) || (x == z) || (y == z) = 2
    | otherwise = 0

-- Exercicio 3
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes x y z = (x /= y) && (x /= z) && (y /= z)

-- Exercício 4
-- O teste está incompleto, ignorando a comparação de n/=p.

-- Exercício 5
-- Definição da função todosIguais (ainda não implementada na lista)
todosIguais :: Int -> Int -> Int -> Bool
todosIguais x y z = (x == y) && (y == z)

-- Tive que fazer uso de nome diferente devido a existência prévia de uma função chamada quantosSaoIguais
quantosSaoIguais2 :: Int -> Int -> Int -> Int
quantosSaoIguais2 x y z
    | todosIguais x y z = 3
    | todosDiferentes x y z = 0
    | otherwise = 2

-- Exercício 6
elevadoDois :: Int -> Int 
elevadoDois x = x ^ 2

-- Exercício 7
elevadoQuatro :: Int -> Int
elevadoQuatro x = elevadoDois x * elevadoDois x

-- Exercício 8
-- Definição da função vendas ainda não definida na lista de exercícios.
vendas :: Int -> Int
vendas 0 = 6
vendas 1 = 10
vendas 2 = 12
vendas 3 = 7
vendas 4 = 9
vendas _ = 10

vendaTotal :: Int -> Int
vendaTotal 0 = vendas 0
vendaTotal n = vendas n + vendaTotal (n-1)