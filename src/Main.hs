module Main where

import System.IO (stdin, hSetEcho, hSetBuffering, BufferMode(NoBuffering), hReady)
import Control.Monad (when)
import System.Random
import Functions
import Structs
import Maps
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game


type PosicaoGloss = (Float,Float)

altura :: Float
altura = 450

comprimento :: Float
comprimento = (-950)

type Textures = [(Peca, (Picture, (Float, Float)))]
type Armas    = [(Guns, Picture)]
type EstadoGloss = (Estado, Textures,Armas)



estadoGlossInicial :: Textures -> Armas ->EstadoGloss
estadoGlossInicial textures armas = (estadoInicial, textures,armas)

reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss _ s = s

reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss _ s = s

fr :: Int
fr = 50

dm :: Display
dm = FullScreen

l :: Float
l = 12.5

desenhaPeca :: Float -> Float -> Peca -> Textures-> Picture
desenhaPeca x y peca textures = Translate realX realY texture
    where tuple = (fromJust . lookup peca) textures
          texture = fst tuple
          realX = ((+x) . fst . snd) tuple
          realY = ((+y) . snd . snd) tuple


desenhaLinha :: Float -> Float -> [Peca] -> Textures -> [Picture]
desenhaLinha x y (h:t) textures = peca : resto
             where peca  = desenhaPeca x y h textures
                   resto = desenhaLinha (x+l) y t textures
desenhaLinha _ _ _ _     = []

desenhaMapa :: Float -> Float -> Labirinto -> Textures -> [Picture]
desenhaMapa x y (h:t) textures = linha ++ resto
            where linha = desenhaLinha x y h textures
                  resto = desenhaMapa x (y-l) t textures
desenhaMapa _ _ _ _             = []

drawJogadores :: [Jogador] -> Armas -> [Picture]
drawJogadores ((Jogador _ _ 0 _ _ _):xs) armas     = drawJogadores xs armas
drawJogadores ((Jogador (i,j) d _ _ _ a):xs) armas = (drawJogador i j d a armas): (drawJogadores xs armas)
drawJogadores _ _                                  = []

drawJogador :: Int -> Int -> Direcao -> Guns -> Armas -> Picture
drawJogador i j d arma armas = (Translate (realPlayerX j) (realPlayerY i)
                               . Rotate degrees) imagem 
  where imagem    = (fromJust . lookup arma) armas
        degrees   = convDirToDegree d

realPlayerX :: Int -> Float
realPlayerX = (+ comprimento)
              . (*l)
              . realToFrac
              . succ

realPlayerY :: Int -> Float
realPlayerY = (+ altura)
              . (* (-l))
              . realToFrac 



desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss (estado,textures,armas) = Pictures draw
                   where draw        =  drawMapa
                                     ++ drawPlayers 
                         drawMapa    =  desenhaMapa comprimento altura mapa textures
                         mapa        =  getMapa estado 
                         drawPlayers =  drawJogadores players armas
                         players     = ( take 1 
                                       . getPlayers) estado


chao :: Picture
chao = Color black (Polygon [(0,0),(l,0),(l,l),(0,l),(0,0)])



main :: IO()
main = do
    wall <- loadBMP "img/parede.bmp"
    door <- loadBMP "img/porta.bmp"
    cferreos <- loadBMP "img/cferreos.bmp"
    guarda <- loadBMP "img/guarda.bmp"
    semaforo <- loadBMP "img/semafro.bmp"
    cancela <- loadBMP "img/cancela.bmp"
    Just caminho <- loadJuicy "img/original.bmp"
    Just passeio <- loadJuicy "img/original2.bmp"
    guitarra <- loadBMP "img/guitarra.bmp"
    camera <- loadBMP "img/camara.bmp"
    play dm
         black
         fr
         (estadoGlossInicial
             [
              (Chao, (chao,(0,0))),
              (Porta,((Scale 0.25 0.25 door), (6.25 , 6.25))),
              (Bloco Wall, ((Scale 0.26 0.26 wall), (6.25,6.25))),
              (Bloco Cancela, ((Scale 0.25 0.25 cancela), (6.25,6.25))),
              (Bloco Semaforo, ((Scale 0.25 0.25 semaforo), (6.25,6.25))),
              (Railway, ((Scale 0.3 0.3 cferreos), (6.25,6.25))),
              (Umbrella, ((Scale 0.9 0.8 guarda), (6.25,6.25))),
              (Bloco Caminho, ((Scale 1 1 caminho), (1.25, 1.25))),
              (Bloco Passeio, ((Scale 1 1 passeio), (1.25, 1.25)))
             ]
             [ (Guitar, guitarra),
               (Camera, camera) 
             ]
         )
         desenhaEstadoGloss
         reageEventoGloss
         reageTempoGloss
