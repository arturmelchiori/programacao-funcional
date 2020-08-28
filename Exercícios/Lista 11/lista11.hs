{- TEC IX - Programação Funcional - André Du Bois
   Exercícios de Programação Haskell 11 - Tipos Algébricos
   Artur Melchiori Cerri
-}

-- Exercício 1 --------------------------------------------------------
data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo
    deriving (Eq, Show)

-- Exercício 2 --------------------------------------------------------
finalDeSemana :: Dia -> Bool
finalDeSemana Sabado = True
finalDeSemana Domingo = True
finalDeSemana _ = False

-- Exercício 3 --------------------------------------------------------
data TalvezFloat = Valor Float | Erro String
    deriving (Eq, Show)

-- Exercício 4 --------------------------------------------------------
divisao :: Float -> Float -> TalvezFloat
divisao n1 0 = Erro "Nao e possivel realizar divisao por zero."
divisao n1 n2 = Valor (n1/n2)

-- Exercício 5 --------------------------------------------------------
data Nat = Zero | Suc Nat
    deriving (Eq, Show)

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Suc n) = 1 + natToInt n

-- Exercício 6 --------------------------------------------------------
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Suc (intToNat (n-1))
