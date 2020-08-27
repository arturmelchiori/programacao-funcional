{- TEC IX - Programação Funcional - André Du Bois
   Trabalho Final - Jogo (Terminal) - Dungeons and Dragons Simplificado
   Artur Melchiori Cerri
-}

import Data.Char
import System.IO
import System.Random

data Personagem = Personagem 
    { pNome   :: String
    , pRaca   :: String
    , pClasse :: String
    , pHPMax  :: Int
    , pHP     :: Int
    , pCA     :: Int
    , pStr    :: Int -- Força
    , pDex    :: Int -- Destreza
    , pCons   :: Int -- Constituição
    , pInt    :: Int -- Inteligência
    , pWis    :: Int -- Sabedoria
    , pChar   :: Int -- Carisma
    , pInit   :: Int -- Iniciativa
    , pSpells :: [Char] -- Magias
    , pItems  :: [Char] -- Itens equipados
    } deriving (Show)

data NPC = NPC 
    { npcNome  :: String
    , npcHPMax :: Int -- Máximo de pontos de vida
    , npcHP    :: Int -- Pontos de vida atual 
    , npcCA    :: Int -- Classe de armadura
    , npcDmg1  :: Int -- Dado de dano do ataque primário do NPC
    , npcDmg2  :: Int -- Dado de dano do ataque secundário do NPC
    } deriving (Show)

-- Lista de NPCs criados previamente para o jogo
goblin :: NPC
goblin = NPC "Goblin" 11 11 13 4 0

hobgoblin :: NPC
hobgoblin = NPC "Hobgoblin" 16 16 17 6 0

orc :: NPC
orc = NPC "Orc" 20 20 17 6 0

{- modifier recebe um valor de habilidade e retorna o modificador referente ao valor passado.
Modificadores são valores a serem adicionados às rolagens de dados para aumentar/diminuir as chances de
sucesso, baseado na habilidade reference à ação a ser realizada. 

Exemplo:
O jogador deseja fazer um teste de subterfúgio para passar pelos inimigos despercebido. Caso ele possua
15 de Destreza (habilidade utilizada para subterfúgio), será adicionado +2 no resultado de sua rolagem, 
tendo em vista que o modificador para o valor 15 de habilidade é +2.
-}
modifier :: Int -> Int
modifier hab = 
    case hab of 
        1  -> -5 
        2  -> -4
        3  -> -4
        4  -> -3
        5  -> -3
        6  -> -2
        7  -> -2
        8  -> -1
        9  -> -1
        10 -> 0
        11 -> 0
        12 -> 1
        13 -> 1
        14 -> 2
        15 -> 2
        16 -> 3
        17 -> 3
        18 -> 4
        19 -> 4
        20 -> 5
        21 -> 5
        22 -> 6
        23 -> 6
        24 -> 7
        25 -> 7
        26 -> 8
        27 -> 8
        28 -> 9
        29 -> 9
        30 -> 10


{- rollDie recebe o número e lados do dado a ser rolado e uma dificuldade da ação a ser realizada, e então
compara se a rolagem resultou em um sucesso, caso a rolagem seja maior que a dificuldade, ou falha, caso
a rolagem seja menor que a dificuldade.
-}
rollDie :: Int -> Int -> IO (Bool, Int)
rollDie nSides difficulty = do
    roll <- randomRIO(1,nSides :: Int)
    case (roll > difficulty) of
        True  -> return (True, roll)
        False -> return (False, roll)


printRollResults :: (Int,Int) -> IO ()
printRollResults (roll, diff) = do
    (bool, n) <- rollDie roll diff
    case (bool, n) of
        (True, 20) -> putStrLn ("Acerto crítico, rolando um 20!")
        (True, _)  -> putStrLn ("Sucesso, rolando um " ++ show n ++ ".")
        (False, 1) -> putStrLn ("Falha crítica, rolando um 1!")
        (False, _) -> putStrLn ("Falha, rolando um " ++ show n ++ ".")

{- Humano: +1 para todos os valores de habilidade
   Elfo: +2 para Dex, +1 para Int
   Anão: +2 para Força, +1 para Cons

   Guerreiro: Str 15, Dex 13, Cons 14, Int 8, Wis 12, Char 10
   Ladino: Str 8, Dex 15, Cons 13, Int 10, Wis 12, Char 14
   Mago: Str 8, Dex 10, Cons 12, Int 15, Wis 14, Char 13
-}
createCharacter :: IO Personagem
createCharacter = do
    putStr "Qual o nome do seu personagem? "
    nome <- getLine
    putStr "Qual a raça do seu personagem (humano, elfo ou anão)? "
    raca <- getLine
    putStr "Qual a classe do seu personagem (guerreiro, ladino ou mago)? "
    classe <- getLine
    case (map toLower raca, map toLower classe) of
        ("humano", "guerreiro") -> return (Personagem nome raca classe 12 12 15 16 14 15 9 13 11 2 [] [])
        ("elfo", "guerreiro")   -> return (Personagem nome raca classe 12 12 15 15 15 14 9 12 10 2  [] [])
        ("anão", "guerreiro")   -> return (Personagem nome raca classe 12 12 15 17 13 15 8 12 10 1 [] [])
        ("humano", "ladino")    -> return (Personagem nome raca classe 10 10 15 9 16 14 11 13 15 3 [] [])
        ("elfo", "ladino")      -> return (Personagem nome raca classe 10 10 15 8 17 13 11 12 14 2 [] [])
        ("anão", "ladino")      -> return (Personagem nome raca classe 10 10 15 10 15 14 10 12 14 2 [] [])
        ("humano", "mago")      -> return (Personagem nome raca classe 8 8 15 9 11 13 16 15 14 0 [] [])
        ("elfo", "mago")        -> return (Personagem nome raca classe 8 8 15 8 12 12 16 14 13 1 [] [])
        ("anão", "mago")        -> return (Personagem nome raca classe 8 8 15 10 10 13 15 14 13 0 [] [])
        (_, _)                  -> return (Personagem nome raca classe 0 0 0 0 0 0 0 0 0 0 [] [])




main :: IO ()
main = do
    personagem <- createCharacter
    putStrLn (show personagem)        