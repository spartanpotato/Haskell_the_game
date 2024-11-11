module World (World(..), initialState) where

import SharedTypes
import Tanks
import System.Random

-- Dimensiones de la ventana
width, height :: Int
width = 640
height = 480

-- Definicion de tipo de dato que define el estado del juego
data World = Game
  { player1 :: Tank,
    player2 :: Tank,
    pillar :: (Float, Float, Float, Float), -- (offset x, offset y, width, height)
    currentPlayer :: Int, -- Player1=1 Player2=2
    finnished :: Int, -- 0 = no terminado, 1 = terminado
    gen :: StdGen
  } deriving Show


-- Se define el estado inicial del juego
initialState :: World
initialState = Game {
    player1 = player1Tank,
    player2 = player2Tank,
    pillar = (0, -(fromIntegral height/2) + 70, 50, 100),
    currentPlayer = 2,
    finnished = 0,
    gen = mkStdGen 0
} 


