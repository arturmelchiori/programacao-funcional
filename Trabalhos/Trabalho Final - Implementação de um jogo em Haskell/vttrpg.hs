{- TEC IX - Programação Funcional - André Du Bois
   Trabalho Final - Jogo (Terminal) - Virtual Tabletop RPG
   Artur Melchiori Cerri
-}

import Data.Char
import System.IO
import System.Random

-----------------------------------------------------------------------
--------------- Estuturas e conteúdo pré definidos --------------------
-----------------------------------------------------------------------

data Personagem = Personagem
    { pNome   :: String
    , pRaca   :: String
    , pClasse :: String
    , pHPMax  :: Int
    , pHP     :: Int
    , pCA     :: Int
    , pStr    :: Int -- Força
    , pDex    :: Int -- Destreza
    , pCon    :: Int -- Constituição
    , pInt    :: Int -- Inteligência
    , pInit   :: Int -- Iniciativa
    , pItems  :: [String] -- Lista de itens equipados
    } deriving (Eq,Show)

data Raca = Raca
    { rNome  :: String
    , rStr   :: Int
    , rDex   :: Int
    , rCon  :: Int
    , rInt   :: Int
    } deriving (Eq,Show)

data Classe = Classe
    { cNome  :: String
    , cHPMax :: Int
    , cHP    :: Int
    , cCA    :: Int
    , cStr   :: Int
    , cDex   :: Int
    , cCon   :: Int
    , cInt   :: Int
    , cDmg   :: Int
    } deriving (Eq,Show)

data NPC = NPC
    { npcNome  :: String
    , npcHPMax :: Int -- Máximo de pontos de vida
    , npcHP    :: Int -- Pontos de vida atual
    , npcCA    :: Int -- Classe de armadura
    , npcDmg  :: Int -- Dado de dano do ataque do NPC
    } deriving (Eq,Show)

data Item = Item
    { iNome :: String
    , iStr  :: Int
    , iDex  :: Int
    , iCon  :: Int
    , iInt  :: Int
    } deriving (Eq,Show)

-- Raças e Classes a serem utilizadas no jogo

humano :: Raca
humano = Raca "Humano" 1 1 1 1

elfo :: Raca
elfo = Raca "Elfo" 0 2 0 2

anao :: Raca
anao = Raca "Anao" 2 0 2 0

guerreiro :: Classe
guerreiro = Classe "Guerreiro" 12 12 16 15 11 14 8 6

ladino :: Classe
ladino = Classe "Ladino" 10 10 14 11 15 12 10 4

mago :: Classe
mago = Classe "Mago" 8 8 14 8 12 12 15 8

-- Lista de NPCs criados previamente para o jogo
goblin :: NPC
goblin = NPC "Goblin" 11 11 13 4

hobgoblin :: NPC
hobgoblin = NPC "Hobgoblin" 16 16 17 6

orc :: NPC
orc = NPC "Orc" 20 20 17 6

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

-----------------------------------------------------------------------
------------------------------- Funções -------------------------------
-----------------------------------------------------------------------

{- rollDie recebe o número e lados do dado a ser rolado e uma dificuldade da ação
a ser realizada, e então compara se a rolagem resultou em um sucesso, caso a rolagem
seja maior que a dificuldade, ou falha, caso a rolagem seja menor que a dificuldade.
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
        (True, 20) -> putStrLn ("Acerto critico, rolando um 20!")
        (True, _)  -> putStrLn ("Sucesso, rolando um " ++ show n ++ ".")
        (False, 1) -> putStrLn ("Falha critica, rolando um 1!")
        (False, _) -> putStrLn ("Falha, rolando um " ++ show n ++ ".")

{- rollDie recebe o número e lados do dado a ser rolado e uma dificuldade da ação
a ser realizada, e então compara se a rolagem resultou em um sucesso, caso a rolagem
seja maior que a dificuldade, ou falha, caso a rolagem seja menor que a dificuldade.
-}
createCharacter :: IO Personagem
createCharacter = do
    putStr "Preencha os dados do seu personagem abaixo \n"
    putStr "Nome: "
    nome <- getLine
    putStr "Raça (humano, elfo ou anao): "
    raca <- getLine
    putStr "Classe (guerreiro, ladino ou mago): "
    classe <- getLine
    case (map toLower raca, map toLower classe) of
        ("humano", "guerreiro") -> return (Personagem nome (rNome humano) (cNome guerreiro) (cHPMax guerreiro) (cHP guerreiro) (cCA guerreiro)
                                                           ((cStr guerreiro) + (rStr humano)) ((cDex guerreiro) + (rDex humano)) ((cCon guerreiro) + (rCon humano))
                                                           ((cInt guerreiro) + (rInt humano)) (cDmg guerreiro) [])
        ("elfo", "guerreiro")   -> return (Personagem nome (rNome elfo) (cNome guerreiro) (cHPMax guerreiro) (cHP guerreiro) (cCA guerreiro)
                                                           ((cStr guerreiro) + (rStr elfo)) ((cDex guerreiro) + (rDex elfo)) ((cCon guerreiro) + (rCon elfo))
                                                           ((cInt guerreiro) + (rInt elfo)) (cDmg guerreiro) [])
        ("anao", "guerreiro")   -> return (Personagem nome (rNome anao) (cNome guerreiro) (cHPMax guerreiro) (cHP guerreiro) (cCA guerreiro)
                                                           ((cStr guerreiro) + (rStr anao)) ((cDex guerreiro) + (rDex anao)) ((cCon guerreiro) + (rCon anao))
                                                           ((cInt guerreiro) + (rInt anao)) (cDmg guerreiro) [])
        ("humano", "ladino")    -> return (Personagem nome (rNome humano) (cNome ladino) (cHPMax ladino) (cHP ladino) (cCA ladino)
                                                           ((cStr ladino) + (rStr humano)) ((cDex ladino) + (rDex humano)) ((cCon ladino) + (rCon humano))
                                                           ((cInt ladino) + (rInt humano)) (cDmg ladino) [])
        ("elfo", "ladino")      -> return (Personagem nome (rNome elfo) (cNome ladino) (cHPMax ladino) (cHP ladino) (cCA ladino)
                                                           ((cStr ladino) + (rStr elfo)) ((cDex ladino) + (rDex elfo)) ((cCon ladino) + (rCon elfo))
                                                           ((cInt ladino) + (rInt elfo)) (cDmg ladino) [])
        ("anao", "ladino")      -> return (Personagem nome (rNome anao) (cNome ladino) (cHPMax ladino) (cHP ladino) (cCA ladino)
                                                           ((cStr ladino) + (rStr anao)) ((cDex ladino) + (rDex anao)) ((cCon ladino) + (rCon anao))
                                                           ((cInt ladino) + (rInt anao)) (cDmg ladino) [])
        ("humano", "mago")      -> return (Personagem nome (rNome humano) (cNome mago) (cHPMax mago) (cHP mago) (cCA mago)
                                                           ((cStr mago) + (rStr humano)) ((cDex mago) + (rDex humano)) ((cCon mago) + (rCon humano))
                                                           ((cInt mago) + (rInt humano)) (cDmg mago) [])
        ("elfo", "mago")        -> return (Personagem nome (rNome elfo) (cNome mago) (cHPMax mago) (cHP mago) (cCA mago)
                                                           ((cStr mago) + (rStr elfo)) ((cDex mago) + (rDex elfo)) ((cCon mago) + (rCon elfo))
                                                           ((cInt mago) + (rInt elfo)) (cDmg mago) [])
        ("anao", "mago")        -> return (Personagem nome (rNome anao) (cNome mago) (cHPMax mago) (cHP mago) (cCA mago)
                                                           ((cStr mago) + (rStr anao)) ((cDex mago) + (rDex anao)) ((cCon mago) + (rCon anao))
                                                           ((cInt mago) + (rInt anao)) (cDmg mago) [])
        (_, _)                  -> error "Entrada invalida."

-----------------------------------------------------------------------
--------------------------------- Jogo --------------------------------
-----------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "  __     ___      _               _                 \n\
              \  \\ \\   / (_)_ __| |_ _   _  __ _| |             \n\
              \   \\ \\ / /| | '__| __| | | |/ _` | |             \n\
              \    \\ V / | | |  | |_| |_| | (_| | |              \n\
              \     \\_/__|_|_| _ \\__|\\__,_|\\__,_|_|           \n\
              \       |_   _|_ _| |__ | | ___| |_ ___  _ __       \n\
              \         | |/ _` | '_ \\| |/ _ \\ __/ _ \\| '_ \\  \n\
              \         | | (_| | |_) | |  __/ || (_) | |_) |     \n\
              \         |_|\\__,_|_.__/|_|\\___|\\__\\___/| .__/  \n\
              \                                       |_|         \n\
              \"

    personagem <- createCharacter
    putStrLn (show personagem)
