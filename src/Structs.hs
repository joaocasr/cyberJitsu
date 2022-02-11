module Structs where



type Instrucoes = [Instrucao]
type PosicaoGrelha = (Int,Int)
type Dimensao = (Int,Int)
type Matriz a = [[a]]

data Instrucao
    = Move Direcao
  deriving (Read,Show,Eq)

data Direcao
    = U 
    | D 
    | L 
    | R 
  deriving (Read,Show,Eq,Enum,Bounded)

type Posicao = (Int,Int)

data Parede = Wall -- "+"
            | Semaforo -- "A"
            | Cancela -- "-"
            | Caminho -- "C"
            | Passeio -- "S"
       --     | Caminhooo -- "L"
  deriving (Read,Show,Eq,Enum,Bounded)

type Labirinto  = [[Peca]]

data Peca
    = Bloco Parede  
    | Chao -- " "
    | Porta -- "p"
 --   | Agua -- "O"
    | Railway -- "#"
    | Umbrella -- "G"
 --   | Banheira
  deriving (Read,Show,Eq)



data Jogador = Jogador
    { posicaoJogador :: PosicaoGrelha   
    , direcaoJogador :: Direcao         
    , vidasJogador   :: Int
    , lasersJogador  :: Int
    , choquesJogador :: Int
    , armaJogador    :: Guns
         
    }
  deriving (Read,Show,Eq)


data Estado = Estado
    { mapaEstado      :: Labirinto       
    , jogadoresEstado :: [Jogador]   
    }
  deriving (Read,Show,Eq)

data Jogada
    = Movimenta Direcao 
    | Comprar
    | None
  deriving (Read,Show,Eq)

data Guns = Camera | Guitar
    deriving (Read,Show,Eq,Enum,Bounded)
