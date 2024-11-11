module Movimiento (currentTank,
                    opositeTank,
                    setCurrentTank,
                    setOppositeTank,
                    updatePosition,
                    movePlayer,
                    collitionPillar,
                    wallCollision,
                    wallBounce,
                    moveCannon) where

import Tanks
import World
import Bala

width, height :: Int
width = 640
height = 480

type Radius = Float 
type Position = (Float, Float)

-- Funcion que devuelve el tanque del jugador actual
currentTank :: World -> Tank
currentTank game
  | currentPlayer game == 1 = player1 game
  | otherwise               = player2 game

-- Funcion que devuelve el tanque del jugador contrario al actual
opositeTank :: World -> Tank
opositeTank game
  | currentPlayer game == 2 = player1 game
  | otherwise               = player2 game

-- Funcion que actualiza el tanque actual
setCurrentTank :: Tank -> World -> World
setCurrentTank tank game
  | currentPlayer game == 1 = game { player1 = tank }
  | otherwise               = game { player2 = tank }

-- Funcion que actualiza el tanque oponente
setOppositeTank :: Tank -> World -> World
setOppositeTank tank game
  | currentPlayer game == 1 = game { player2 = tank }
  | otherwise               = game { player1 = tank }

-- Funcion que calcula la nueva posicion del tanque en funcion de la velocidad
-- 
-- Parametros:
--   dx - Cantidad a desplazarse en el eje x
--  tank - El tanque a desplazarse
-- salida:
-- El tanque con posición actualizada
updatePosition :: Float -> Tank -> Tank
updatePosition dx tank = moveTank (+ dx) tank -- tank { position = (x + dx, y) }
  -- where
  --   (x, y) = position tank

-- Funcion que actualiza la posicion del tanque en funcion de la velocidad y el estado actual de moveLeft y moveRight
movePlayer :: Float -> World -> World
movePlayer _ game
    | moveLeft tank = setCurrentTank (updatePosition (-tankVel tank) tank) game
    | moveRight tank = setCurrentTank (updatePosition (tankVel tank) tank) game
    | otherwise = game
    where
        tank = currentTank game


collitionPillar :: Bullet -> (Float, Float, Float, Float) -> Bool
collitionPillar bullet (x,y,w,h) =
    let (bX,bY) = bPosition bullet
        halfW = w/2 + bulletRadius
        halfH = h/2 + bulletRadius
    in (bX >= x-halfW && bX<=x+halfW && bY >= y-halfH && bY <=y+halfH)


wallCollision :: Position -> Radius -> Bool 
wallCollision (x, _) radius = leftCollision || rightCollision || leftPillarCollision || rightPillarCollision
  where
    leftCollision  = (x - radius <= - (fromIntegral width / 2))
    rightCollision = (x + radius >= fromIntegral width / 2)
    leftPillarCollision = (x - radius > -25 && x - radius < 25)
    rightPillarCollision = (x + radius > -25 && x + radius < 25)


--Funcion que usa wallCollision para ver si el personaje esta en un limite y revierte su movimiento
wallBounce :: World -> World
wallBounce game = 
    let tank = if currentPlayer game == 1 then player1 game else player2 game
        (x, y) = position tank
        (w, h) = bodySize tank
        x' = if wallCollision (position tank) (w / 2)
             then if direction tank == 1  -- colision izquierda
                  then x + tankVel tank
                  else x - tankVel tank  -- colision derecha
             else x
    in if currentPlayer game == 1
        then game {player1 = tank {position = (x', y)}}
        else game {player2 = tank {position = (x', y)}}

-- Funcion que se encarga de mover el cañon de un tanque hacia arriba o hacia abajo, dependiendo del estado de moveUp y moveDown
moveCannon :: Float -> World -> World
moveCannon _ game
  | moveUp tank = setCurrentTank (tank {angle = newAngle}) game
  | moveDown tank = setCurrentTank (tank {angle = newAngle}) game
  | otherwise = game
  where 
    tank = currentTank game
    newAngle 
      | (currentPlayer game) == 1 && moveUp tank = max minAngle ((angle tank) - angleDiff)
      | (currentPlayer game) == 1 && moveDown tank = min maxAngle ((angle tank) + angleDiff)
      | (currentPlayer game) /= 1 && moveUp tank = min (-minAngle) ((angle tank) + angleDiff)
      | (currentPlayer game) /= 1 && moveDown tank = max (-maxAngle) ((angle tank) - angleDiff)
      | otherwise = angle tank