module Functions where

import Structs

getList :: Int -> [a] -> a
getList n l = (!!) l n

getMapa (Estado m j) = m

getPlayers (Estado m j) =j

convDirToDegree :: Direcao -> Float
convDirToDegree D = 0
convDirToDegree L = 90
convDirToDegree U = 180
convDirToDegree R = 270
