module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

--Definicion de la ventana
width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

--Definicion color de fondo
background :: Color
background = black

--Definicion fps
fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

--Definicion de tipo de dato que define el estado del juego, show solo esta para depurar
data PongGame = Game
  { ballLoc :: (Float, Float)  
  , ballVel :: (Float, Float) 
  , player1 :: Float           
                               
  , player2 :: Float           
  } deriving Show

--Deficion estado inicial del juego
initialState :: PongGame
initialState = Game
  { ballLoc = (-10, 30)
  , ballVel = (90, -30)
  , player1 = 40
  , player2 = -80
  }

--Definicion de funcion para convertir estado del juego en una imagen
render :: PongGame -> Picture  
render game = pictures [ball, walls,
            mkPaddle rose 120 $ player1 game,
            mkPaddle orange (-120) $ player2 game]
  where
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)

--Funcion que en base de segundos y el estado anterior del juego actualiza la posicion de la bola
moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

--Funcion que 
update ::  Float -> PongGame -> PongGame
update seconds = wallBounce . moveBall seconds

--Creacion de sinonimos
type Radius = Float 
type Position = (Float, Float)

--Funcion que verifica si posicion de la bola tiene colision con la de una pared
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral width / 2 
    bottomCollision = y + radius >=  fromIntegral width / 2

--Funcion que usa wallCollision para ver si la bola golpea una pared e invierte la velocidad correspondiente
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities.
    (vx, vy) = ballVel game

    vy' = if wallCollision (ballLoc game) radius
          then
             -- Update the velocity.
             -vy
           else
            -- Do nothing. Return the old velocity.
            vy


-- Respond to key events.
handleKeys :: Event -> PongGame -> PongGame

-- For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (Char 's') _ _ _) game = game { ballLoc = (0, 0) }

-- Do nothing for all other events.
handleKeys _ game = game