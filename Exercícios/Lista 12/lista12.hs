{- TEC IX - Programação Funcional - André Du Bois
   Exercícios de Programação Haskell 12 - Tipos Algébricos Recursivos
   Artur Melchiori Cerri
-}

-- Definição de árvore passada na lista∷
data Arvore a = Folha a | Nodo a (Arvore a) (Arvore a)
    deriving (Eq, Show)

-- Exercício 1 --------------------------------------------------------
multDois :: Arvore Int -> Arvore Int
multDois (Folha f) = Folha (2 * f)
multDois (Nodo n a1 a2) = Nodo (2 * n) (multDois a1) (multDois a2)

-- Exercício 2 --------------------------------------------------------
contaElementos :: Arvore a -> Int
contaElementos (Folha f) = 1
contaElementos (Nodo n a1 a2) = 1 + contaElementos a1 + contaElementos a2

-- Exercício 3 --------------------------------------------------------
altura :: Arvore a -> Int
altura (Folha f) = 1
altura (Nodo n a1 a2) = 1 + max (altura a1) (altura a2)

-- Exercício 4 --------------------------------------------------------
maiorElemento :: Arvore Int -> Int
maiorElemento (Folha f) = f
maiorElemento (Nodo n a1 a2) = max n (max (maiorElemento a1) (maiorElemento a2))

-- Exercício 5 --------------------------------------------------------
procuraInt :: Int -> Arvore Int -> Bool
procuraInt x (Folha f) = x == f
procuraInt x (Nodo n a1 a2) = x == n || procuraInt x a1 || procuraInt x a2

-- Exercício 6 --------------------------------------------------------
quantasVezes :: Int -> Arvore Int -> Int
quantasVezes x (Folha f)
    | x == f = 1
    | otherwise = 0
quantasVezes x (Nodo n a1 a2)
    | x == n = 1 + (\x y -> x + y) (quantasVezes x a1) (quantasVezes x a2)
    | otherwise = 0 + (\x y -> x + y) (quantasVezes x a1) (quantasVezes x a2)

-- Exercício 7 --------------------------------------------------------
refleteArvore :: Arvore a -> Arvore a
refleteArvore (Folha f) = Folha f
refleteArvore (Nodo n a1 a2) = Nodo n (refleteArvore a2) (refleteArvore a1)

-- Exercício 8 --------------------------------------------------------
arvoreToLista :: Arvore a -> [a]
arvoreToLista (Folha f) = [f]
arvoreToLista (Nodo n a1 a2) = n : (arvoreToLista a1) ++ (arvoreToLista a2)

-- Exercício 9 --------------------------------------------------------
mapTree :: (a -> b) -> Arvore a -> Arvore b
mapTree fn (Folha f) = Folha (fn f)
mapTree fn (Nodo n a1 a2) = Nodo (fn n) (mapTree fn a1) (mapTree fn a2)
