-- TEC IX - Programação Funcional - André Du Bois
-- Exercícios de Programação Haskell II
-- Artur Melchiori Cerri

-- Exercício 1 --------------------------------------------------------
maxi :: Int -> Int -> Int
maxi x y
    | x >= y = x
    | otherwise = y

-- Exercício 2 --------------------------------------------------------
-- Definição da função vendas
vendas :: Int -> Int
vendas 0 = 6
vendas 1 = 10
vendas 2 = 12
vendas 3 = 7
vendas 4 = 9
vendas 5 = 11
vendas _ = 1

maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0
maiorVenda n = maxi (vendas n) (maiorVenda (n-1))

-- Exercicio 3 --------------------------------------------------------
maxVenda :: Int -> Int
maxVenda n
    | (n == 0) = 0
    | (maiorVenda n == vendas n) = n
    | otherwise = maxVenda(n-1)

-- Exercício 4 --------------------------------------------------------
zeroVendas :: Int -> Int
zeroVendas n
    | (n == -1) = -1
    | (vendas n == 0) = n
    | otherwise = zeroVendas (n-1)

-- Exercício 5 --------------------------------------------------------
achaSemana :: Int -> Int -> Int
achaSemana s n
    | (n == -1) = -1
    | (vendas n == s) = n
    | otherwise = achaSemana s (n-1)

-- Exercício 6 --------------------------------------------------------
-- Simplesmente trocando s (valor a ser procurado de vendas) por 0.

-- Exercício 7 --------------------------------------------------------
maiorVendaAlt :: Int -> Int -> Int
maiorVendaAlt m n
    | (m == n) = vendas m
    | otherwise = maxi (vendas n) (maiorVendaAlt m (n-1))

maxVendaAlt :: Int -> Int -> Int
maxVendaAlt m n
    | (m == n) = m
    | (maiorVendaAlt m n == vendas n) = n
    | otherwise = maxVendaAlt m (n-1)

zeroVendasAlt :: Int -> Int -> Int
zeroVendasAlt m n
    | (n < m) = -1
    | (vendas n == 0) = n
    | otherwise = zeroVendasAlt m (n-1)

-- Exercício 8 --------------------------------------------------------
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n-1)

-- Exercício 9 --------------------------------------------------------
produto :: Int -> Int -> Int
produto m n
 | (m == n) = m*1
 | otherwise = m * (produto (m+1) n)

-- Exercício 10 -------------------------------------------------------
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)