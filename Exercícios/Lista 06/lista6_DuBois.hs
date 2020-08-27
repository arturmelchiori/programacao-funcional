-- Exercicio 1

pegaPosicao :: Int -> [Int] -> Int
pegaPosicao n [] = error "Posicao não existe na lista vazia"
pegaPosicao 1 l = head l
pegaPosicao n (x:xs) = pegaPosicao (n-1) xs

-- Exercicio 2


pega :: Int -> [Int] -> [Int]
pega a []             = error "Numero maior que a lista"
pega 1 (x:xs)         = [x]
pega n (x:xs)         =  x : pega (n-1) xs

-- Exercicio 3

retira :: Int -> [Int] -> [Int]
retira a []             = error "Numero maior que a lista"
retira 1 (x:xs)         = xs
retira n (x:xs)         = retira (n-1) xs

-- Exercicio 4
-- Alternativamente, pode-se usar listas de inteiros e dividir usando "div"
mediaLista :: [Float] -> Float
mediaLista [] = error "Lista vazia"
mediaLista l = (somaLista l)/ (fromIntegral (length l))
 where somaLista :: [Float]-> Float
       somaLista [x] = x
       somaLista (x:xs) = x + somaLista xs

-- Exercicio 5

pegaMaiores :: Int -> [Int] -> [Int]
pegaMaiores n [] = []
pegaMaiores n (x:xs)
    | x > n = x:pegaMaiores n xs
    | otherwise = pegaMaiores n xs

--6

contaMaiores :: Int -> [Int] -> Int
contaMaiores n [] = 0
contaMaiores n (x:xs)
    | x > n = 1 + contaMaiores n xs
    | otherwise = contaMaiores n xs


--Questao 7
intercala :: [Int] -> [Int] -> [Int]
intercala [] l = l
intercala l [] = l 
intercala (x:xs) (z:zs) = x:z:intercala xs zs

--Exercício 8

dupli :: [Int] -> [Int]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- Exercicio 9

repli :: Int -> String -> String
repli n [] = []
repli n (x:xs) = mEl n x ++ repli n xs
   where mEl :: Int -> Char -> String
         mEl 1 c = [c]
         mEl n c = c : mEl (n-1) c

-- Exercicio 10

dropEvery :: Int -> String -> String
dropEvery n l = de2 n n l
      where de2 :: Int -> Int -> String -> String
            de2 n1 n2 [] = []
            de2 1  n  (x:xs) = de2 n n xs
            de2 n1 n2 (x:xs) = x: de2 (n1-1) n2 xs


-- Exercicio 11

split :: Int -> String -> (String,String)
split n s = (pega2 n s, retira2 n s)

pega2 :: Int -> String -> String
pega2 a []             = error "Numero maior que a lista"
pega2 1 (x:xs)         = [x]
pega2 n (x:xs)         =  x : pega2 (n-1) xs

retira2 :: Int -> String -> String
retira2 a []             = error "Numero maior que a lista"
retira2 1 (x:xs)         = xs
retira2 n (x:xs)         = retira2 (n-1) xs


