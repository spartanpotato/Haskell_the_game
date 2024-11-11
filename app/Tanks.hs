module Tanks (Tank(..), 
            player1Tank, 
            player2Tank, 
            angleDiff, 
            minAngle, 
            maxAngle,
            defaultAmountFuel,
            defaulCurrentFuelBar,
            defaultOffSetBar, moveTank) where

import Graphics.Gloss
import Colores
import Bala
import SharedTypes

-- Tamano de la ventana
width, height :: Int
width = 640
height = 480

-- Angulo minimo y maximo de cada tanque, cantidad de angulo que se mueve el canon.
angleDiff, minAngle, maxAngle :: Float
angleDiff = 5 * (pi / 180)
minAngle = 5 * (pi / 180)
maxAngle = 85 * (pi / 180)

-- Coordenadas de barra de combustible para ambos tanques
defaultAmountFuel, defaultOffSetBar :: Float
defaultFuelBar, defaulCurrentFuelBar :: (Float, Float, Float, Float)
defaultAmountFuel = 100
defaultOffSetBar = 0
defaulCurrentFuelBar = (0, 200, 200, 31)
defaultFuelBar = (0, 200, 200, 31)

-- Coordenadas de barras de vida de ambos tanques
defaultHealth :: Int
defaultHealthBarPlayer1, defaultHealthBarPlayer2 :: (Float, Float, Float, Float)
defaultHealth = 30
defaultHealthBarPlayer1 = (- (fromIntegral width / 10) * 4, (fromIntegral height / 4), 30, 150)
defaultHealthBarPlayer2 = ((fromIntegral width / 10) * 4, (fromIntegral height / 4), 30, 150)

-- Definición de un nuevo tipo "Pair" que encapsula una tupla "(a, b)"
newtype Pair b a = Pair { getPair :: (a,b) }

-- Instancia de Functor para Pair
instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair(f x, y)

-- función que usa fmap para mover el tanque
-- 
-- Parametros:
--   f: Funcion a aplicar al atributo "position" del tanque .
--  Tank: El tanque a modificar su posición.
--
-- salida:
-- Tanque con su posición actualizada. 
moveTank :: (Float -> Float) -> Tank -> Tank
moveTank f tank = tank { position = getPair $ fmap f $ Pair (position tank) }

-- Inicializacion de tanques
player1Tank :: Tank
player1Tank = Tank {
    position = (-(fromIntegral width / 4), -(fromIntegral height / 2) + 25),
    health = defaultHealth,
    healthBar = defaultHealthBarPlayer1,
    currentHealthBar = defaultHealthBarPlayer1,
    healthOffset = 0,
    angle = 45 * (pi/180),
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
    fuelBar = defaultFuelBar,
    amountFuel = defaultAmountFuel,
    currentFuelBar = defaulCurrentFuelBar,
    barWidth = 200,
    shotUsage = 20, 
    moveUsage = 10,
    cannonUsage = 5,
    offsetBar = defaultOffSetBar,
    percentage = "100%",
    moveUp = False,
    moveDown = False
}

player2Tank :: Tank
player2Tank = Tank {
    position = (fromIntegral width/4, -(fromIntegral height / 2) + 25),
    health = defaultHealth,
    healthBar = defaultHealthBarPlayer2,
    currentHealthBar = defaultHealthBarPlayer2,
    healthOffset = 0,
    angle = -45 * (pi / 180),
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
    fuelBar = defaultFuelBar,
    amountFuel = defaultAmountFuel,
    currentFuelBar = defaulCurrentFuelBar,
    barWidth = 200,
    shotUsage = 20, 
    moveUsage = 10,
    cannonUsage = 5,
    offsetBar = defaultOffSetBar,
    percentage = "100%",
    moveUp = False,
    moveDown = False
}