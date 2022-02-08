module Functions where

import DataStructs

getList :: Int -> [a] -> a
getList n l = (!!) l n

getMapa (Estado m j) = m

getJogadores (Estado m j) =j
