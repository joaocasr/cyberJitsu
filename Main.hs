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
    wall <- loadBMP "img/wall.bmp"
    door <- loadBMP "img/porta.bmp"
    cferreos <- loadBMP "img/railway.bmp"
    guarda <- loadBMP "img/umbrella.bmp"
    semaforo <- loadBMP "img/semaforo.bmp"
    water <- loadBMP "img/water.bmp"
    cancela <- loadBMP "img/cancela.bmp"
    play dm
         black
         fr
         (estadoGlossInicial
             [
              (Chao, (chao,(0,0))),
              (Porta,((Scale 0.25 0.25 door), (6.25 , 6.25))),
              (Bloco Wall, ((Scale 0.25 0.25 wall), (6.25,6.25))),
              (Bloco Cancela, ((Scale 0.25 0.25 cancela), (6.25,6.25))),
              (Bloco Semaforo, ((Scale 0.25 0.25 semaforo), (6.25,6.25))),
              (Railway, ((Scale 0.25 0.25 cferreos), (6.25,6.25))),
              (Umbrella, ((Scale 0.25 0.25 guarda), (6.25,6.25))),
              (Agua, ((Scale 0.25 0.25 water), (6.25,6.25)))
             ]
         )
         desenhaEstadoGloss
         reageEventoGloss
         reageTempoGloss


