module Maps where

import Structs
import Functions


sampleMaze=     ["+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
                 "+                                           +                +              +                                                                           +",
                 "+                                           +                +              +                                                                           +",
                 "+                                           +                +              +                                                                           +",
                 "+                                           +                +              +                                                                           +",
                 "+                                           p                p              p                                                                           +",
                 "+                                           p                p              p                                                                           +",
                 "+                                           p                p              p                                                                           +",
                 "+                                           p                p              p                                                                           +",
                 "+                                           p                p              p                                                                           +",
                 "+                                           p                p              p                                                                           +",
                 "+                                           +                +              +                                                                           +",
                 "+                                           +                +              +                                                                           +",
                 "+                                           +                +              +                                                                           +",
                 "+                                           +                +              +                                                                           +",
                 "+                            ++++++++++++++++                ++++++++++++++++                                                                           +",
                 "+                            +                                              +                                                                           +",
                 "+                            +                                              +                                                                           +",
                 "+                            +                                              +                                                                           +",
                 "+                            +                                              +                                                                           +",
                 "+                            ++++++++++++++++                ++++++++++++++++                                                                           +",
                 "+                            +              +                +              +                                                                           +",
                 "+                            +              +                +              +                                            O                              +",
                 "+                            +              +                +              +      --------------------------------------O                              +",
                 "+                            +              +                p              +                                            O                              +",
                 "+                            +              p                p              +                                            O                              +",
                 "+                            +              p                p              +###########################################################################+",
                 "+                            +              p                p              +   p       p       p       p       p       p       p       p               +",
                 "+                            +              p                p              +   p       p       p       p       p       p       p       p               +",
                 "+                            +              p                p              +   p       p       p       p       p       p       p       p               +",
                 "+                            +              p                +              +###########################################################################+",
                 "+                            +              +                +              +                                                                           +",
                 "+                            +              +                +              +                                                                           +",
                 "+                            +              +                +              +                                                                           +",
                 "+                            +              +                +              +                                                                           +",
                 "+                            ++++++++++++++++                ++++++++++++++++                                                                           +",
                 "+                            +                                              ++++++++++++++++++++++++++++++++++++++++++++++++++pppppp+++++++++++++++++++++",
                 "+                            +                                              +           +              +                +               +               +",
                 "+                            +                                              +           +              +                +               +               +",
                 "+                            +                                              +           +              +                +               +               +",
                 "+                            +                                              +           p              +                +               +               +",
                 "+                            ++++++++++++++++++++++++++++++++++++++++++++++++           p              +                +               +               +",
                 "+                            +                                              +           p              +                +               p               +",
                 "+                            p                                              p           p              p                +               p               +",
                 "+                            p                                              p           p              p                +               p               +",
                 "+                            p                                              p           p              +                +               p               +",
                 "+                            p                                              p           +              +                +               +               +",
                 "+                            p                                              p           +              +                +               +               +",
                 "+                            p                                              p           +              +                +               +               +",
                 "+                            +                                              +           +              +                +               +               +",
                 "+                            +++++++pppppp+++++++++++++++++++++++++++++++++++++++++++++++              +                +               +               +",
                 "+                            +                                                          +              +                +++++++++++++++++               +",
                 "+                            +                                                          +              +                                                +",
                 "+                            +                                                          +              +                                                +",
                 "+                            +                     +                                    +              +                                                +",
                 "+                            +                                                          +              +                                                +",
                 "+                            +                                    +                     +              +                +++++pppppp++++++               +",
                 "+                            +                                                          +              +                +               +               +",
                 "+                            +                                                          +              p                +               +               +",
                 "+                            +                                                          +              p                +               +               +",
                 "+                            +                                                          +              p                +               +               +",
                 "+     C                      +                                                          +              p                +               +               +",
                 "+                            +                                                          p              p                +               +               +",
                 "+                            +                                                          p              p                +               +               +",
                 "+                            +                                                          p              +                +               +               +",
                 "+                            +                                                          p              +                +               +               +",
                 "+                            +                                                          p              +                +               +               +",
                 "+                            +                                                          p              +                +               +               +",
                 "+                            +                                                          +              +                +               +               +",
                 "+     S                      +    G        G        G        G        G        G        +              +                +               +               +",
                 "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"]


estadoInicial :: Estado
estadoInicial = (Estado ( convStrLab sampleMaze)
                                                [(Jogador (15,15) D 100 12 200 Camera),(Jogador (15,130) L 100 12 200 Guitar),(Jogador (25,156) U 3 1 99 Guitar)])

convStrLab :: Matriz Char -> Labirinto
convStrLab m = map (linha) m 
    where
    linha :: String -> [Peca]
    linha "" = []
    linha (x:xs) | x == '+' = Bloco Wall : (linha xs)
                 | x == 'p' = Porta : (linha xs)
                 | x == ' ' = Chao : (linha xs)
                 | x == '#' = Railway : (linha xs)
                 | x == 'G' = Umbrella : (linha xs)
                 | x == '-' = Bloco Cancela : (linha xs)
                 | x == 'O' = Bloco Semaforo : (linha xs)
                 | x == 'C' = Bloco Caminho : (linha xs)
                 | x == 'S' = Bloco Passeio : (linha xs)
               --  | x == 'L' = Bloco Caminhooo : (linha xs)
               --  | x == 'B' = Banheira : (linha xs)
       

showMaze :: IO()
showMaze =  putStrLn $ unlines sampleMaze








{-  
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = let gen = mkStdGen seed
                        in take n $ randomRs (0,99) gen
nrAleatorio :: Int -> Int
nrAleatorio seed = head $ geraAleatorios 1 seed


subLista :: Int -> [a] -> [[a]]
subLista _ [] = []
subLista n l = take n l: subLista n (drop n l)

converteCorredor :: [Int] -> Corredor
converteCorredor c = map (\p -> convertePeca p) c

converteLabirinto :: [[Int]] -> Labirinto
converteLabirinto l = map (\c -> converteCorredor c) l


corredorParedes::Corredor->Corredor
corredorParedes [] = []
corredorParedes (h:t)= Parede: corredorParedes t

printCorridor :: Corridor -> String
printCorridor [] = "\n"
printCorridor (h:t) = show h ++ printCorridor t


printMaze :: Maze -> String
printMaze [] = ""
printMaze (h:t) = printCorridor h ++ printMaze t

mudaPrimeira::Labirinto-> Labirinto
mudaPrimeira [[]] = [[]]
mudaPrimeira ((x:xs):t) = corredorParedes (x:xs) : t


mudaUltima :: Labirinto-> Labirinto
mudaUltima [[]] = [[]]
mudaUltima [(x:xs)] = [corredorParedes (x:xs)]
mudaUltima ((x:xs):t)  = (x:xs) : mudaUltima t


mudaTetoChao :: Labirinto -> Labirinto
mudaTetoChao [[]]= [[]]
mudaTetoChao l = mudaPrimeira(mudaUltima l)


extremidadeEsq:: Labirinto->Labirinto
extremidadeEsq [[]] = [[]]
extremidadeEsq [(x:xs)] = [Parede:xs]
extremidadeEsq ((x:xs):t) = (Parede:xs) : extremidadeEsq t

extremidadeDrt :: Labirinto->Labirinto
extremidadeDrt [[]] = [[]]
extremidadeDrt [(x:xs)] = [init (x:xs) ++ [Parede]]
extremidadeDrt ((x:xs):t) = (init (x:xs) ++ [Parede]) : extremidadeDrt t


extremidadeLados :: Labirinto -> Labirinto
extremidadeLados [[]] = [[]]
extremidadeLados l = extremidadeEsq(extremidadeDrt l)


quatroExtremidades::Labirinto -> Labirinto
quatroExtremidades [[]]= [[]]
quatroExtremidades l = extremidadeLados(mudaTetoChao l)

geraMapa :: Int -> Labirinto
geraMapa s  =
            let random_nrs = geraAleatorios (200*45) s
            in quatroExtremidades (converteLabirinto $ subLista 200 random_nrs)
-}
