1.

-- nas duas próximas, o tipo dos elementos da lista de entrada, deve ser igual ao tipo de saída (a)

head :: [a] -> a

tail :: [a] -> a

-- Nada na definição das seguintes funções obriga os elementos a terem o mesmo tipo. Por isso
-- usamos varáveis diferentes:

fst :: (a,b) -> a

shift :: ((a,b),c) -> (a,(b,c))

2.

concatena :: [[a]] -> [a]

3. 

inverte :: [a] -> [a]

4. 

zipp3 :: [a] -> [b] -> [c] -> [(a,b,c)]

5.

-- Aqui muita gente colocou
-- mapMaisUm :: Num a => (a -> b) -> [a] -> [b]
-- essa resposta eu não aceitei (apesar de correta) sem a devida explicação

mapMaisUm :: (Int->a) -> [Int] -> [a]

6. 

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v
foldr f v (x:xs) = f x (foldr f v xs)

O que o foldr faz, é computar um valor baseado na sucessiva aplicação da função "f" nos elementos de uma lista e
em um acumulador. O "f" recebe sempre dois valores, o primeiro é um elemento da lista e, o segundo, o resultado
acumulado da aplicação anterior. Como a gente não sabe o que a função f faz, os valores que vem da lista (tipo a), podem
ter tipo diferente do valor computado e acumulado (tipo b). O foldr (r vem de "right"/diretia em inglês), faz essa
computação de forma associativa para à direita, ou seja, os elementos da lista vão sendo inseridos na computação
do acumulador do final para o inicio. (Você consegue pensar como ficaria a implementação de um foldl???)


Pode parecer complicado, mas a grande maioria dos problemas usando recursão podem ser implementados usando o foldr.
Um exemplo, é o caso recursivo da função abreJogada do Minesweeper:

abreJogada :: Int -> Int -> MBoard -> GBoard -> GBoard
abreJogada l c mb gb
  | isMine l c mb       = gb
  | isDigit (gPos l c gb)   = gb
  | cMinas l c mb /= 0  = uPos l c (intToDigit $ cMinas l c mb) gb
  | otherwise           = foldr (\(i,j) ngb -> abreJogada i j mb ngb) (uPos l c '0' gb) (validMoves (length gb) l c)

Aqui eu uso o foldr para ir abrindo as jogadas e também acumulando as modificações que são feitas no tabuleiro.
Toda a vez que abro uma jogada, estou passando o valor acumulado do tabuleiro, obtido abrindo os movimentos válidos anteriores.

A gente pode tentar encontrar também o tipo do foldr pensando de maneira parecida ao algoritmo de inferência de tipos
do Haskell.
Vamos analisar essa equação:

f x (foldr f v xs)

A gente consegue observa três coisas:
1. O primeiro argumento da função f vem da lista, dessa forma, o primeiro argumento do "f" tem que possuir
o mesmo tipo dos valores que estão na lista. Chutando um tipo "a" qualquer, obtemos:

foldr :: (a -> ? -> ?) -> ? -> [a] -> ?

2. Notem que o tipo do segundo argumento de "f", tem que ser o mesmo tipo do resultado do foldr, pois a chamada recursiva é passada
como segundo argumento para "f". Vamos chutar então um outro tipo para isso:

foldr :: (a -> b -> ?) -> ? -> [a] -> b

3. Podemos observar também, que o resultado do foldr é uma chamada para "f", dessa forma, o tipo de saída do "f"
deve ser o mesmo tipo de saída do "foldr":

foldr :: (a -> b -> b) -> ? -> [a] -> b


Olhando agora para o caso base:

foldr f v [] = v

podemos ver que o valor "v" possui o mesmo tipo do resultado do foldr (no caso base retornamos "v" como resposta).
Dessa forma obtemos:

foldr :: (a -> b -> b) -> b -> [a] -> b

Considerando esse exemplo:

Prelude> foldr (\x y -> y ++ [x]) [] [1,2,3,4]
[4,3,2,1]

Nesse exemplo, temos tipos diferentes para "a" e "b". "a" possuí tipo "Int" e "b" possui tipo "[Int]".





