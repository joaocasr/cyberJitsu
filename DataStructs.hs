module DataStructs where



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
            | Agua -- "A"
            | Cancela -- "-"
  deriving (Read,Show,Eq,Enum,Bounded)

type Labirinto  = [[Peca]]

data Peca
    = Bloco Parede  
    | Chao -- " "
    | Porta -- "p"
    | Semaforo -- "O"
    | Railway -- "#"
    | Umbrella -- "G"
  deriving (Read,Show,Eq)



data Jogador = Jogador
    { posicaoJogador :: PosicaoGrelha   
    , direcaoJogador :: Direcao         
    , vidasJogador   :: Int         
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

