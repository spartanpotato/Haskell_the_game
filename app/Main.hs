module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Combustible
import Colores
import SharedTypes
import Bala
import Tanks  
import Movimiento
import World
import Vida
import System.Random

--Definicion de la ventana
width, height, offset :: Int
width = 640
height = 480
offset = 100

window :: Display
window = InWindow "Canonwars" (width, height) (offset, offset)

initVelocity :: Float
initVelocity = 200

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


--Definicion de funcion para convertir estado del juego en una imagen
render :: World -> Picture  
render game = pictures [renderTank (player1 game), renderTank (player2 game), 
                        mainFloor, mainPillar, 
                        mainFuelBar, mainTotalFuel, 
                        player1HealthBar, player2HealthBar, 
                        player1CurrentHealthBar, player2CurrentHealthBar,
                        player1HealthNum, player2HealthNum,
                        mainPercent,makeBullet]
  where
    -- Funcion para renderizar un tanque
    renderTank :: Tank -> Picture
    renderTank tank =
        let radius = 10  -- Radio del círculo en el que gira el cañón
            rectWidth, rectHeight :: Float
            rectWidth = fst $ cannonSize tank
            rectHeight = snd $ cannonSize tank  -- Longitud del cañón (aumentado)

            -- Calcular la posición del cañón
            cannonBaseX = radius * sin (angle tank)
            cannonBaseY = radius * cos (angle tank) + (snd $ bodySize tank) / 2

            cannon = color (colorCannon tank) $ rectangleSolid rectWidth rectHeight

            translatedCannon = translate (fst $ position tank) (snd $ position tank) $
                               translate cannonBaseX cannonBaseY $
                               rotate ((angle tank) * 180 / pi) $ cannon

        in pictures [translatedCannon, uncurry translate (position tank) $ color (colorBody tank) $ rectangleSolid (fst $ bodySize tank) (snd $ bodySize tank)]

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
    mainPercent = showFuel (-85, 190, 0.2 , 0.2, amountFuel (currentTank game)) -- x = bar offset - (bar width / 2 ) - 10 
    makeBullet = drawBullet (currentBullet (currentTank game)) (isShooting (currentTank game))

    player1HealthBar = makeHealthBar (healthBar (player1 game))
    player2HealthBar = makeHealthBar (healthBar (player2 game))
    player1CurrentHealthBar = totalHealth (currentHealthBar (player1 game))
    player2CurrentHealthBar = totalHealth (currentHealthBar (player2 game))
    player1HealthNum = showHealth (-283, 0, 0.2 , 0.2, fromIntegral (health (player1 game)))
    player2HealthNum = showHealth (230, 0, 0.2 , 0.2, fromIntegral (health (player2 game)))


checkTurn :: World -> World
checkTurn game =
    let tank = currentTank game
        currentPlayerFuel = amountFuel tank
        existsBullet = isShooting tank
        isMoving = (moveLeft tank || moveRight tank || moveDown tank || moveUp tank)
    in if (currentPlayerFuel <= 0 && existsBullet == False && isMoving == False)
        then changeTurn tank {amountFuel = defaultAmountFuel, currentFuelBar = defaulCurrentFuelBar,
                            offsetBar = defaultOffSetBar}
        else game
    where
        changeTurn t = if currentPlayer game == 1 
            then game {player1 = t, currentPlayer = 2} 
            else game {player2 = t, currentPlayer = 1} 


--Funcion que 
-- Epica funcion que
update :: Float -> World -> World
update seconds game =
    let lastGame = wallBounce . movePlayer seconds . moveCannon seconds $ game
        tank = currentTank lastGame
        tank2 = opositeTank lastGame
        hpTank2 = health tank2
        (offsetXHealth, offsetYHealth, widthHealth, heightHealth) = currentHealthBar tank2
        currentPillar = pillar lastGame
        -- Obtener la nueva posición y estado de la bala disparada
        (updatedBullet, shot) = moveBullet (isShooting tank) seconds (currentBullet tank)
        isCollitionPillar = collitionPillar updatedBullet currentPillar
        dmgBullet = bDamage updatedBullet
        isCollition = collitionBullet updatedBullet tank2
        -- Actualizar el tanque con la nueva bala y el estado de disparo
        updatedTank2 = 
            let newHeight = heightHealth - fromIntegral (dmgBullet * 5)
                newHealth = hpTank2 - dmgBullet
            in if (isCollition && isShooting tank) 
                then tank2 {health = if newHealth >= 0 then newHealth else 0,
                            currentHealthBar = (offsetXHealth, offsetYHealth - ( fromIntegral dmgBullet * 5) / 2,
                                                widthHealth, if newHeight >= 0 then newHeight else 0)}  
                else tank2
        updatedTank = tank { currentBullet = updatedBullet, isShooting = (shot && not(isCollitionPillar) && not (isCollition)) }
        -- Establecer el tanque actualizado en el juego
    in checkTurn . setCurrentTank updatedTank $ setOppositeTank updatedTank2 lastGame


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
                            currentFuelBar = (offset - (usage), 200, (newFuel / 100) * barW, 31),
                            offsetBar = offset - (usage)}
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
                            currentFuelBar = (offset - (usage), 200, (newFuel / 100) * barW, 31),
                            offsetBar = offset - (usage)} 
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
        (newBullet, finalGen) = createBullet (position tank) (cannonL/2) currentAngle initVelocity (currentPlayer game) (gen game)
    in if (newFuel >= 0 && not(isShooting tank))
      then (updateGame (tank {amountFuel = newFuel, currentFuelBar = (offset - (usage), 200, (newFuel / 100) * barW, 31),
                            offsetBar = offset - (usage), isShooting = True,
                            currentBullet = newBullet})){gen = finalGen}
      else game 
    where
        updateGame t = if currentPlayer game == 1 then game {player1 = t} else game {player2 = t} 

-- Gasto al usar el cañon
handleKeys (EventKey (Char 'w') Down _ _) game = 
    let tank = currentTank game
        fuel = amountFuel tank
        usage = cannonUsage tank
        offset = offsetBar tank
        barW = barWidth tank
        newFuel = fuel - usage
    in if (newFuel >= 0)
      then updateGame tank {amountFuel = newFuel, currentFuelBar = (offset - (usage), 200, (newFuel / 100) * barW, 31),
                            offsetBar = offset - (usage), moveUp = True, moveDown = False} 
      else game
    where
        updateGame t = if currentPlayer game == 1 then game {player1 = t} else game {player2 = t}

handleKeys (EventKey (Char 'w') Up _ _) game = updateGame (currentTank game) {moveUp = False}
    where
        updateGame t = if currentPlayer game == 1 then game {player1 = t} else game {player2 = t}

-- Gasto al usar el cañon
handleKeys (EventKey (Char 's') Down _ _) game = 
    let tank = currentTank game
        fuel = amountFuel tank
        usage = cannonUsage tank
        offset = offsetBar tank
        barW = barWidth tank
        newFuel = fuel - usage
    in if (newFuel >= 0)
      then updateGame tank {amountFuel = newFuel, currentFuelBar = (offset - (usage), 200, (newFuel / 100) * barW, 31),
                            offsetBar = offset - (usage), moveUp = False, moveDown = True} 
      else game 
    where
        updateGame t = if currentPlayer game == 1 then game {player1 = t} else game {player2 = t}

handleKeys (EventKey (Char 's') Up _ _) game = updateGame (currentTank game) {moveDown = False}
    where
        updateGame t = if currentPlayer game == 1 then game {player1 = t} else game {player2 = t}


-- Do nothing for all other events.
handleKeys _ game = game
