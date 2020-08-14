vendas :: Int -> Int
vendas 0 = 22
vendas 1 = 33
vendas 2 = 0
vendas 3 = 44
vendas _ = 12


-- 1

maxi :: Int -> Int -> Int
maxi x y
  | x>=y        = x
  | otherwise   = y

-- 2

maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0
maiorVenda n = maxi (vendas n) (maiorVenda (n-1))

-- 3

maxVenda :: Int -> Int
maxVenda 0 = 0
maxVenda n
   | vendas n == maiorVenda n = n
   | otherwise                = maxVenda (n-1)

-- 4

zeroVendas :: Int -> Int
zeroVendas 0
  | vendas 0 == 0  = 0
  | otherwise      = -1
zeroVendas n
  | vendas n == 0  = n
  | otherwise      = zeroVendas (n-1)

-- 5

achaSemana :: Int -> Int -> Int
achaSemana s 0
    | vendas 0 == s   = 0
    | otherwise       = -1
achaSemana s n
    | vendas n == s   = n
    | otherwise       = achaSemana s (n-1)

-- 6 

zeroVendas2 :: Int -> Int
zeroVendas2 s = achaSemana 0 s

-- 7


maiorVenda2 :: Int -> Int -> Int
maiorVenda2 m n 
 | m == n    = vendas m
maiorVenda2 m n = maxi (vendas n) (maiorVenda2 m (n-1))


maxVenda2 :: Int -> Int -> Int
maxVenda2 m n
  | m == n   = m
maxVenda2 m n
   | vendas n == maiorVenda2 m n = n
   | otherwise                   = maxVenda2 m (n-1)


zeroVendas3 :: Int -> Int -> Int
zeroVendas3 m n
  | (m==n) && vendas n == 0  = n
  | m == n                   = -1
  | vendas n == 0  = n
  | otherwise      = zeroVendas3 m (n-1)


achaSemana2 :: Int -> Int -> Int -> Int
achaSemana2 s m n
    | (m== n) && (vendas n == s)   = n
    | m == n                       = -1
    | vendas n == s   = s
    | otherwise       = achaSemana2 s m (n-1)


zeroVendas4 :: Int -> Int -> Int
zeroVendas4 m n = achaSemana2 0 m n

-- 8

fatorial :: Int -> Int 
fatorial 0 = 1
fatorial n = n * fatorial (n-1)

-- 9

produto :: Int -> Int -> Int
produto m n
   | m == n = m
   | otherwise = n * produto m (n-1)

-- 10
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
