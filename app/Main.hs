module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

--Definicion de la ventana
width, height, offset :: Int
width = 640
height = 480
offset = 100

window :: Display
window = InWindow "Canonwars" (width, height) (offset, offset)

--Definicion color de fondo
background :: Color
background = white

--Definicion fps
fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

--Definicion de tipo de dato que define el estado del juego, show solo esta para depurar
data World = Game
  { playerVel :: Float,  
    player1Loc :: (Float,Float),
    player2Loc :: (Float,Float),
    pillar :: (Float, Float, Float, Float),  
    moveLeft :: Bool,  
    moveRight :: Bool,
    currentPlayer :: Int, -- Player1=1 Player2=2
    direction :: Int -- izquierda=1 derecha=2         
  } deriving Show

--Deficion estado inicial del juego
initialState :: World
initialState = Game
  { playerVel = 2,
    player1Loc = (-(fromIntegral width / 4), -(fromIntegral height / 2) + 25),
    player2Loc = (fromIntegral width/4, -(fromIntegral height / 2) + 25),
    pillar = (0, -(fromIntegral height/2) + 70, 50, 100),
    moveLeft = False,
    moveRight = False,
    currentPlayer = 1,
    direction = 0
  }

--Definicion de funcion para convertir estado del juego en una imagen
render :: World -> Picture  
render game = pictures [player1, player2, mainFloor, mainPillar]
  where
    player1 = uncurry translate (player1Loc game) $ color player1Color $ rectangleSolid 10 10
    player1Color = dark red
    player2 = uncurry translate (player2Loc game) $ color player2Color $ rectangleSolid 10 10
    player2Color = dark blue


    makeFloor :: Float -> Float -> Float -> Float -> Picture
    makeFloor offsetX offsetY floorWidth floorHeight = 
        translate offsetX offsetY $ color wallColor $ rectangleSolid floorWidth floorHeight

    wallColor = greyN 0.5
    mainFloor = makeFloor 0 (- (fromIntegral height/2) + 10) (fromIntegral width) 20

    makePillar :: (Float, Float, Float, Float) -> Picture
    makePillar (offsetX, offsetY, floorWidth, floorHeight) =
        translate offsetX offsetY $ color pillarColor $ rectangleSolid floorWidth floorHeight

    pillarColor = greyN 0.5
    mainPillar = makePillar (pillar game)


--Funcion que en base de segundos y si se esta presionando una tecla mueve al jugador
movePlayer :: Float -> World -> World
movePlayer _ game
    | moveLeft game = if currentPlayer game == 1
        then game { player1Loc = (x - playerVel game, y) }
        else game { player2Loc = (x - playerVel game, y) }
    | moveRight game = if currentPlayer game == 1
        then game { player1Loc = (x + playerVel game, y) }
        else game { player2Loc = (x + playerVel game, y) }
    | otherwise = game 
  where
    (x, y) = player1Loc game

--Funcion que 
update ::  Float -> World -> World
update seconds = wallBounce . movePlayer seconds


type Radius = Float 
type Position = (Float, Float)

wallCollision :: Position -> Radius -> Bool 
wallCollision (x, _) radius = leftCollision || rightCollision || leftPillarCollision || rightPillarCollision
  where
    leftCollision  = (x - radius <= - (fromIntegral width / 2))
    rightCollision = (x + radius >= fromIntegral width / 2)
    leftPillarCollision = (x - radius > -25 && x - radius < 25)
    rightPillarCollision = (x + radius > -25 && x + radius < 25)


--Funcion que usa wallCollision para ver si el personaje esta en un limite y revierte su movimiento
wallBounce :: World -> World
wallBounce game
    | currentPlayer game == 1 = game { player1Loc = (x', y) }
    | currentPlayer game == 2 = game { player2Loc = (x', y) }
    where
        radius = 3 -- radio debe ser el radio original del personaje menos la velocidad
        (x, y) = if currentPlayer game == 1 
                 then player1Loc game 
                 else player2Loc game
        x' = if wallCollision (if currentPlayer game == 1 
                                then player1Loc game 
                                else player2Loc game) radius
             then if direction game == 1  -- colision izquierda
                  then x + playerVel game
                  else x - playerVel game  -- colision derecha
             else x


-- Respond to key events.
handleKeys :: Event -> World -> World

-- Player se mueve a la izquierda con a
handleKeys (EventKey (Char 'a') Down _ _) game = 
    game { moveLeft = True, moveRight = False, direction = 1}

-- Player deja de moverse al dejar de presionar a
handleKeys (EventKey (Char 'a') Up _ _) game = 
    game { moveLeft = False}

-- Player se mueve a la derecha con d
handleKeys (EventKey (Char 'd') Down _ _) game = 
    game { moveRight = True, moveLeft = False, direction = 2}

-- Player deja de moverse al dejar de presionar d
handleKeys (EventKey (Char 'd') Up _ _) game = 
    game { moveRight = False}

-- Do nothing for all other events.
handleKeys _ game = game