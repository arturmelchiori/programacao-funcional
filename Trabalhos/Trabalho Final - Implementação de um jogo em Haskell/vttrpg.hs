{- TEC IX - Programação Funcional - André Du Bois
   Trabalho Final - Jogo (Terminal) - Virtual Tabletop RPG
   Artur Melchiori Cerri
-}

import Data.Char
import System.IO
import System.Random
import System.Console.ANSI


data Personagem = Personagem
    { pNome   :: String
    , pRaca   :: String
    , pClasse :: String
    , pHPMax  :: Int -- Máximo de Hit Points do personagem
    , pHP     :: Int -- Hit Points atual do personagem
    , pCA     :: Int -- Classe de Armadura
    , pStr    :: Int -- Força
    , pDex    :: Int -- Destreza
    , pCon    :: Int -- Constituição
    , pInt    :: Int -- Inteligência
    , pDmg    :: Int -- Dado de dano
    , pItems  :: [String] -- Lista de itens equipados
    } deriving (Eq,Show)

data Raca = Raca
    { rNome  :: String
    , rStr   :: Int
    , rDex   :: Int
    , rCon   :: Int
    , rInt   :: Int
    } deriving (Eq,Show)

data Classe = Classe
    { cNome  :: String
    , cHPMax :: Int
    , cHP    :: Int
    , cCA    :: Int -- Classe de Armadura
    , cStr   :: Int -- Força
    , cDex   :: Int -- Destreza
    , cCon   :: Int -- Constituição
    , cInt   :: Int -- Inteligência
    , cDmg   :: Int -- Dado de dano
    } deriving (Eq,Show)

data NPC = NPC
    { npcNome  :: String
    , npcHPMax :: Int -- Máximo de pontos de vida
    , npcHP    :: Int -- Pontos de vida atual
    , npcCA    :: Int -- Classe de armadura
    , npcDmg   :: Int -- Dado de dano do ataque do NPC
    } deriving (Eq,Show)

data Item = Item
    { iNome :: String
    , iCA   :: Int
    , iStr  :: Int
    , iDex  :: Int
    , iCon  :: Int
    , iInt  :: Int
    , iDmg  :: Int
    } deriving (Eq,Show)

-- Raças e Classes pré deifinidos

humano :: Raca
humano = Raca "Humano" 1 1 1 1

elfo :: Raca
elfo = Raca "Elfo" 0 2 0 2

anao :: Raca
anao = Raca "Anão" 2 0 2 0

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

-- Itens pré-definidos

escudo :: Item
escudo = Item "Escudo" 1 0 0 0 0 0

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

{- rollDie recebe o número e lados do dado a ser rolado e gera um número
aleatório como resultado da rolagem
-}
rollDie :: Int -> IO Int
rollDie nSides = do
    roll <- randomRIO(1,nSides :: Int)
    return roll

{- rollPlusModifier recebe um personagem, realiza uma rolagem de dados
e aplica o modificador de habilidade referente à classe do Personagem
(Força para Guerreiro, Destreza para Ladino e Inteligência para Mago)
-}
rollPlusModifier :: Personagem -> IO Int
rollPlusModifier char = do
    roll <- rollDie 20
    case pClasse char of
        "Guerreiro" -> return (roll + modifier (pStr char))
        "Ladino"    -> return (roll + modifier (pDex char))
        "Mago"      -> return (roll + modifier (pInt char))
        _           -> return 0

{- rollDieResults e printRollResults não seriam usadas propriamente durante a
história. Eu fiz uso delas mais para testes e decidi manter, já que o trabalho
não foi finalizado.
-}
rollDieResults :: Int -> Int -> IO (Bool, Int)
rollDieResults nSides difficulty = do
     roll <- randomRIO(1,nSides :: Int)
     case (roll > difficulty) of
         True  -> return (True, roll)
         False -> return (False, roll)


printRollResults :: (Int,Int) -> IO ()
printRollResults (roll, diff) = do
    (bool, n) <- rollDieResults roll diff
    case (bool, n) of
        (True, 20) -> putStrLn ("Acerto critico, rolando um 20!")
        (True, _)  -> putStrLn ("Sucesso, rolando um " ++ show n ++ ".")
        (False, 1) -> putStrLn ("Falha critica, rolando um 1!")
        (False, _) -> putStrLn ("Falha, rolando um " ++ show n ++ ".")

createCharacter :: IO Personagem
createCharacter = do
    putStr "Para informações sobre raças, classes, funcionamento e exemplos  \n\
           \de ações/escolhas, por favor consulte o manual do jogador (PDF de\n\
           \descrição do trabalho).                                    \n\n\

           \---------------------------------------------- \n\
           \| Preencha os dados do seu personagem abaixo | \n\
           \---------------------------------------------- \n\n"
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


-- characterCard recebe um personagem e imprime um card com suas informações
characterCard :: Personagem -> [Char]
characterCard char = "\n|--------------------------------------------|\n\
                      \| Nome: " ++ pNome char ++ " \n\
                      \| Raça: " ++ pRaca char ++ " \n\
                      \| Classe: " ++ pClasse char ++ " \n\
                      \|                            \n\
                      \| Hit Points: " ++ show (pHP char) ++ "  \n\
                      \| Classe de Armadura: " ++ show (pCA char) ++ " \n\
                      \| Itens: " ++ show (pItems char) ++ " \n\
                      \|                                     \n\
                      \| Força: " ++ show (pStr char) ++ "  \n\
                      \| Destreza: " ++ show (pDex char) ++ "  \n\
                      \| Constituição: " ++ show (pCon char) ++ "  \n\
                      \| Inteligência: " ++ show (pInt char) ++ "  \n\
                      \| Dano: 1d" ++ show (pDmg char) ++ "  \n\
                      \|--------------------------------------------|\n"

{- equiItem recebe um Item a ser "equipado" no personagem, somando os atributos
do item aos do personagem, assim como adicionando o nome do Item à lista de
itens equipados.
-}
equipItem :: Item -> Personagem -> IO Personagem
equipItem item char = return (Personagem (pNome char) (pRaca char) (pClasse char) (pHPMax char) (pHP char) ((pCA char) + (iCA item))
                                         ((pStr char) + (iStr item)) ((pDex char) + (iDex item)) ((pCon char) + (iCon item))
                                         ((pInt char) + (iInt item)) ((pDmg char) + (iDmg item)) ((iNome item) : (pItems char)))

{- dmgPlayer e dmgNPC foram criadas para atribuir dano aos Hit Points do
personagem e inimigos, respectivamente.
Tentei fazer a implementação chamando diretamente o ia ser modificado, mas não
consegui botar em prática.
No terminal é possível, após a criação de um personagem, realizar por exmeplo
pHP personagem - 5
para diminuir o HP em 5, mas não era possível (ou não consegui) fazer o mesmo
dentro de outras funções.
-}
dmgPlayer :: Int -> Personagem -> IO Personagem
dmgPlayer dmg char = return (Personagem (pNome char) (pRaca char) (pClasse char) (pHPMax char) ((pHP char) - dmg) (pCA char)
                                   (pStr char) (pDex char) (pCon char) (pInt char) (pDmg char) (pItems char))

dmgNPC :: Int -> NPC -> IO NPC
dmgNPC dmg npc = return (NPC (npcNome npc) (npcHPMax npc) ((npcHP npc) - dmg) (npcCA npc) (npcDmg npc))

-----------------------------------------------------------------------
---------------------- Mapas/Escolhas durante o jogo ------------------
-----------------------------------------------------------------------

intro :: Personagem -> IO Personagem
intro char = do
    putStrLn "Você acorda em uma caverna escura, amarrado por cordas à uma viga\n\
             \de madeira. Aos poucos, suas memórias retornam; o comboio no qual\n\
             \você trabalhava como escolta foi atacado por um grupo de orcs. \n\
             \Subjulgados, vocês foram capturados.\n\
             \\n\
             \Aos poucos, tentando se livrar das amarras, as cordas se afrouxam,\n\
             \permitindo que você se solte. Um orc permanece de guarda na sala.\n\
             \\n\
             \Como deseja prosseguir?\n\
             \1 - Atacar o Orc (Iniciar combate)\n\
             \2 - Tentar escapar sem que seja notado (Subterfúgio)\n\
             \Escolha (1 ou 2): "
    escolha <- getLine
    if escolha == "1"
        then do
            putStr "TRABALHO NÃO FINALIZADO.\n"
        else if escolha == "2"
            then do
                putStr "TRABALHO NÃO FINALIZADO.\n"
            else do
                putStr "ENTRADA INVÁLIDA.\n"

    return char

-----------------------------------------------------------------------
--------------------------------- Main --------------------------------
-----------------------------------------------------------------------

main :: IO ()
main = do
    clearScreen
    putStrLn "Olá, aventureiro. Bem vindo à demo de"
    putStrLn "      __     ___      _               _                 \n\
              \      \\ \\   / (_)_ __| |_ _   _  __ _| |             \n\
              \       \\ \\ / /| | '__| __| | | |/ _` | |             \n\
              \        \\ V / | | |  | |_| |_| | (_| | |              \n\
              \         \\_/__|_|_| _ \\__|\\__,_|\\__,_|_|           \n\
              \           |_   _|_ _| |__ | | ___| |_ ___  _ __       \n\
              \             | |/ _` | '_ \\| |/ _ \\ __/ _ \\| '_ \\  \n\
              \             | | (_| | |_) | |  __/ || (_) | |_) |     \n\
              \             |_|\\__,_|_.__/|_|\\___|\\__\\___/| .__/  \n\
              \                                           |_|     \n\
              \"

    personagem <- createCharacter
    putStrLn "\n**Personagem criado com sucesso**\n"
    personagem <- dmgPlayer 10 personagem
    putStrLn (characterCard personagem)

    putStr "Pressione qualquer tecla para iniciar a história..."
    option <- getChar
    clearScreen

    intro personagem

    putStr "\n\n**FIM**\n\n"
