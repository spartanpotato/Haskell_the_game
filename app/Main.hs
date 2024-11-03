module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Combustible
import Colores
import Bala
import System.Random

--Definicion de la ventana
width, height, offset :: Int
width = 640
height = 480
offset = 100

window :: Display
window = InWindow "Canonwars" (width, height) (offset, offset)

initVelocity :: Float
initVelocity = 100

--Definicion color de fondo
background :: Color
background = blue' --makeColor 0.8549 0.9961 1.0 1.0

--Definicion fps
fps :: Int
fps = 60

main :: IO ()
main = do
    gen' <- newStdGen
    let initialGame = initialState{gen = gen'}
    play window background fps initialGame render handleKeys update

data Tank = Tank
  { position :: (Float, Float)  -- Posición del tanque (x, y)
  , health :: Int
  , angle    :: Float           -- Ángulo de dirección del tanque
  , isShooting :: Bool          -- Estado del disparo
  , currentBullet :: Bullet     -- Bala con los datos de posicion
  , bodySize :: (Float, Float)  -- Tamaño del cuerpo (ancho, alto)
  , cannonSize :: (Float, Float) -- Tamaño del cañón (ancho, largo)
  , colorBody :: Color          -- Color del cuerpo del tanque
  , colorCannon :: Color        -- Color del cañón del tanque
  , tankVel :: Float            -- Velocidad del tanque
  , moveLeft :: Bool           -- Movimiento hacia la izquierda
  , moveRight :: Bool           -- Movimiento hacia la derecha
  , direction :: Int            -- Dirección (0 = izq, 1 = der)
  , fuelBar :: (Float, Float, Float, Float)
  , amountFuel :: Float
  , currentFuelBar :: (Float, Float, Float, Float)
  , offsetBar :: Float
  , barWidth :: Float
  , shotUsage :: Float
  , moveUsage :: Float
  , cannonUsage :: Float
  , percentage :: String
  } deriving (Show)
--Definicion de tipo de dato que define el estado del juego, show solo esta para depurar
data World = Game
  { player1 :: Tank,
    player2 :: Tank,
    pillar :: (Float, Float, Float, Float),
    currentPlayer :: Int, -- Player1=1 Player2=2
    gen :: StdGen
  } deriving Show


-- Se define el estado inicial del juego
initialState :: World
initialState = Game {
    player1 = Tank {
        position = (-(fromIntegral width / 4), -(fromIntegral height / 2) + 25),
        health = 30,
        angle = 45,
        isShooting = False,
        currentBullet = defaultBullet,
        bodySize = (60, 20),
        cannonSize = (6, 25),
        colorBody = dark $ dark green,
        colorCannon = greyN 0.3,
        tankVel = 2,
        moveLeft = False,
        moveRight = False,
        direction = 0,
        fuelBar = (-200, 200, 200, 50),
        amountFuel = 100,
        currentFuelBar = (-200, 200, 180, 31),
        barWidth = 180,
        shotUsage = 20, 
        moveUsage = 10,
        cannonUsage = 5,
        offsetBar = -200,
        percentage = "100%"
    },
    player2 = Tank {
        position = (fromIntegral width/4, -(fromIntegral height / 2) + 25),
        health = 30,
        angle = 45,
        isShooting = False,
        currentBullet = defaultBullet,
        bodySize = (60, 20),
        cannonSize = (6, 25),
        colorBody = dark $ dark green,
        colorCannon = greyN 0.3,
        tankVel = 2,
        moveLeft = False,
        moveRight = False,
        direction = 0,
        fuelBar = (-200, 200, 200, 50),
        amountFuel = 100,
        currentFuelBar = (-200, 200, 180, 31),
        barWidth = 180,
        shotUsage = 20, 
        moveUsage = 10,
        cannonUsage = 5,
        offsetBar = -200,
        percentage = "100%"
    },
    pillar = (0, -(fromIntegral height/2) + 70, 50, 100),
    currentPlayer = 1,
    gen = mkStdGen 0
}

-- Funciones para el movimiento del tanque
currentTank :: World -> Tank
currentTank game
  | currentPlayer game == 1 = player1 game
  | otherwise               = player2 game

opositeTank :: World -> Tank
opositeTank game
  | currentPlayer game == 2 = player1 game
  | otherwise               = player2 game

setCurrentTank :: Tank -> World -> World
setCurrentTank tank game
  | currentPlayer game == 1 = game { player1 = tank }
  | otherwise               = game { player2 = tank }

updatePosition :: Float -> Tank -> Tank
updatePosition dx tank = tank { position = (x + dx, y) }
  where
    (x, y) = position tank



--Definicion de funcion para convertir estado del juego en una imagen
render :: World -> Picture  
render game = pictures [renderTank (player1 game), renderTank (player2 game), mainFloor, mainPillar, mainFuelBar, mainTotalFuel, mainPercent,makeBullet]
  where
    -- Funcion para renderizar un tanque
    renderTank :: Tank -> Picture
    renderTank tank = pictures [uncurry translate (position tank) $ color (colorBody tank) $ rectangleSolid (fst $ bodySize tank) (snd $ bodySize tank),
                                uncurry translate (position tank) $ color (colorCannon tank) $ rectangleSolid (fst $ cannonSize tank) (snd $ cannonSize tank)]


    makeFloor :: Float -> Float -> Float -> Float -> Picture
    makeFloor offsetX offsetY floorWidth floorHeight = 
        translate offsetX offsetY $ color wallColor $ rectangleSolid floorWidth floorHeight
    
    wallColor = darkBlue -- greyN 0.5
    mainFloor = makeFloor 0 (- (fromIntegral height/2) + 10) (fromIntegral width) 20

    makePillar :: (Float, Float, Float, Float) -> Picture
    makePillar (offsetX, offsetY, floorWidth, floorHeight) =
        translate offsetX offsetY $ color pillarColor $ rectangleSolid floorWidth floorHeight

    pillarColor = darkBlue -- greyN 0.5
    mainPillar = makePillar (pillar game)

    mainFuelBar = makeFuelBar (fuelBar (currentTank game))
    mainTotalFuel = totalFuel (currentFuelBar (currentTank game))
    mainPercent = showFuel (-290, 190, 0.2 , 0.2, amountFuel (currentTank game))
    makeBullet = drawBullet (currentBullet (currentTank game)) (isShooting (currentTank game))


--Funcion que en base de segundos y si se esta presionando una tecla mueve al jugador
movePlayer :: Float -> World -> World
movePlayer _ game
    | moveLeft tank = setCurrentTank (updatePosition (-tankVel tank) tank) game
    | moveRight tank = setCurrentTank (updatePosition (tankVel tank) tank) game
    | otherwise = game
    where
        tank = currentTank game

moveBullet :: Bool -> Float -> Bullet -> (Bullet,Bool)
moveBullet shoot seconds bullet
    | shoot = (updateBullet seconds bullet)
    | otherwise = (bullet,False)

updateBullet :: Float -> Bullet -> (Bullet,Bool)
updateBullet seconds bullet =
    let (x, y) = bPosition bullet
        (vx, vy) = bVelocity bullet
        -- Actualizamos la velocidad vertical con la gravedad
        newVy = vy + gravity * seconds
        -- Calculamos la nueva posición en x e y
        newX = x + vx * seconds
        newY = y + newVy * seconds
        -- Limitar la posición horizontal entre los bordes de la ventana
        clampedX = min (fromIntegral width / 2) (max (-(fromIntegral width / 2)) newX)
        -- Verificar colisiones y actualizar posición y velocidad
        newPosition = if newY <= -(fromIntegral height / 2) + bulletRadius || 
                         newX <= -(fromIntegral width / 2) + bulletRadius + 40|| 
                         newX >= (fromIntegral width / 2) - bulletRadius - 40
                      then (clampedX, max newY (-(fromIntegral height / 2) + bulletRadius +40 )) -- Posición en caso de colisión
                      else (clampedX, newY)
        -- Si colisiona, detener la bala; de lo contrario, aplicar la nueva velocidad
        newVelocity = if newY <= -(fromIntegral height / 2) + bulletRadius || 
                         newX <= -(fromIntegral width / 2) + bulletRadius -40 || 
                         newX >= (fromIntegral width / 2) - bulletRadius - 40
                      then (0, 0)
                      else (vx, newVy)
        shot = newVelocity /= (0,0)
    in (bullet { bPosition = newPosition, bVelocity = newVelocity },shot)


--Funcion que 
-- Epica funcion que
update :: Float -> World -> World
update seconds game =
    let lastGame = wallBounce . movePlayer seconds $ game
        tank = currentTank lastGame
        
        -- Obtener la nueva posición y estado de la bala disparada
        (updatedBullet, shot) = moveBullet (isShooting tank) seconds (currentBullet tank)

        -- Actualizar el tanque con la nueva bala y el estado de disparo
        updatedTank = tank { currentBullet = updatedBullet, isShooting = shot }

        -- Establecer el tanque actualizado en el juego
    in setCurrentTank updatedTank lastGame




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
wallBounce game = 
    let tank = if currentPlayer game == 1 then player1 game else player2 game
        radius = 3 -- radio debe ser el radio original del personaje menos la velocidad
        (x, y) = position tank
        x' = if wallCollision (position tank) radius
             then if direction tank == 1  -- colision izquierda
                  then x + tankVel tank
                  else x - tankVel tank  -- colision derecha
             else x
    in if currentPlayer game == 1
        then game {player1 = tank {position = (x', y)}}
        else game {player2 = tank {position = (x', y)}}


-- Respond to key events.
handleKeys :: Event -> World -> World

-- Up : indica que tecla fue presionada, Down : indica que una tecla ha sido soltada

-- Player se mueve a la izquierda con a
handleKeys (EventKey (Char 'a') Down _ _) game = 
    let tank = currentTank game
        fuel = amountFuel tank
        usage = moveUsage tank
        offset = offsetBar tank
        barW = barWidth tank
        newFuel = fuel - usage
     in if (newFuel >= 0)
      then updateGame tank {moveLeft = True, moveRight = False, direction = 1, amountFuel = newFuel, 
                            currentFuelBar = (offset - (usage-1), 200, (newFuel / 100) * barW, 31),
                            offsetBar = offset - (usage-1)}
      else game
    where
        updateGame t = if currentPlayer game == 1 then game {player1 = t} else game {player2 = t}

-- Player deja de moverse al dejar de presionar a
handleKeys (EventKey (Char 'a') Up _ _) game = updateGame (currentTank game) {moveLeft = False}
    where
        updateGame t = if currentPlayer game == 1 then game {player1 = t} else game {player2 = t}

-- Player se mueve a la derecha con d
handleKeys (EventKey (Char 'd') Down _ _) game = 
    let tank = currentTank game
        fuel = amountFuel tank
        usage = moveUsage tank
        offset = offsetBar tank
        barW = barWidth tank
        newFuel = fuel - usage
    in if (newFuel >= 0)
      then updateGame tank {moveRight = True, moveLeft = False, direction = 2, amountFuel = newFuel, 
                            currentFuelBar = (offset - (usage-1), 200, (newFuel / 100) * barW, 31),
                            offsetBar = offset - (usage-1)} 
      else game
    where
        updateGame t = if currentPlayer game == 1 then game {player1 = t} else game {player2 = t}

-- Player deja de moverse al dejar de presionar d
handleKeys (EventKey (Char 'd') Up _ _) game = updateGame (currentTank game) {moveRight = False}
    where
        updateGame t = if currentPlayer game == 1 then game {player1 = t} else game {player2 = t}


-- Combustible se consume al disparar
handleKeys (EventKey (Char 'e') Up _ _) game = 
    let tank = currentTank game
        fuel = amountFuel tank
        usage = shotUsage tank
        offset = offsetBar tank
        barW = barWidth tank
        (cannonW,cannonL) = cannonSize tank
        currentAngle = angle tank
        newFuel = fuel - usage
        (newBullet, finalGen) = createBullet (position tank) (cannonL/2) currentAngle initVelocity (gen game)
    in if (newFuel >= 0 && not(isShooting tank))
      then (updateGame (tank {amountFuel = newFuel, currentFuelBar = (offset - (usage - 2), 200, (newFuel / 100) * barW, 31),
                            offsetBar = offset - (usage - 2), isShooting = True,
                            currentBullet = newBullet})){gen = finalGen}
      else game 
    where
        updateGame t = if currentPlayer game == 1 then game {player1 = t} else game {player2 = t} 

-- Gasto al usar el cañon
handleKeys (EventKey (Char 'w') Up _ _) game = 
    let tank = currentTank game
        fuel = amountFuel tank
        usage = cannonUsage tank
        offset = offsetBar tank
        barW = barWidth tank
        newFuel = fuel - usage
    in if (newFuel >= 0)
      then updateGame tank {amountFuel = newFuel, currentFuelBar = (offset - (usage - 0.5), 200, (newFuel / 100) * barW, 31),
                            offsetBar = offset - (usage - 0.5)} 
      else game 
    where
        updateGame t = if currentPlayer game == 1 then game {player1 = t} else game {player2 = t}
-- Gasto al usar el cañon
handleKeys (EventKey (Char 's') Up _ _) game = 
    let tank = currentTank game
        fuel = amountFuel tank
        usage = cannonUsage tank
        offset = offsetBar tank
        barW = barWidth tank
        newFuel = fuel - usage
    in if (newFuel >= 0)
      then updateGame tank {amountFuel = newFuel, currentFuelBar = (offset - (usage - 0.5), 200, (newFuel / 100) * barW, 31),
                            offsetBar = offset - (usage - 0.5)} 
      else game 
    where
        updateGame t = if currentPlayer game == 1 then game {player1 = t} else game {player2 = t}


-- Do nothing for all other events.
handleKeys _ game = game
