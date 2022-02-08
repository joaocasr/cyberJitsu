module Main where


import System.Random
import Functions
import DataStructs
import Maps
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game


type PosicaoGloss = (Float,Float)

altura :: Float
altura = 525

comprimento :: Float
comprimento = (-940)

type Textures = [(Peca, (Picture, (Float, Float)))]
type EstadoGloss = (Estado, Textures)


estadoGlossInicial :: Textures -> EstadoGloss
estadoGlossInicial textures = (estadoInicial, textures)

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

desenhaPeca :: Float -> Float -> Peca -> Textures
desenhaPeca x y peca textures = Translate realX realY texture
    where tuple = (fromJust . lookup peca) textures
          texture = fst tuple
          realX = ((+x) . fst . snd) tuple
          realY = ((+y) . snd . snd) tuple


desenhaLinha :: Float -> Float -> [Peca] -> Textures -> Picture
desenhaLinha x y (h:t) textures = peca : resto
             where peca  = desenhaPeca x y h textures
                   resto = desenhaLinha (x+l) y t textures


desenhaMapa :: Float -> Float -> Labirinto -> Textures -> [Picture]
desenhaMapa x y (h:t) textures = linha ++ resto
            where linha = desenhaLinha x y h textures
                  resto = desenhaMapa x (y-l) t textures
desenhaMapa _ _ _ _             = []



desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss (estado,textures) = Pictures drawMapa
                   where drawMapa    =  desenhaMapa comprimento altura mapa textures
                         mapa        =  getMapa estado 

chao :: Picture
chao = Color black (Polygon [(0,0),(l,0),(l,l),(0,l),(0,0)])


main :: IO()
main = do
    --cameramen <- loadBMP "img/image1.bmp"
    play dm
         black
         fr
         (estadoGlossInicial
             [
              (Chao, (chao,(0,0)))
             ]
         )
         desenhaEstadoGloss
         reageEventoGloss
         reageTempoGloss


