module Tanks (Tank(..), 
            player1Tank, 
            player2Tank, 
            angleDiff, 
            minAngle, 
            maxAngle,
            defaultAmountFuel,
            defaulCurrentFuelBar,
            defaultOffSetBar) where

import Graphics.Gloss
import Colores
import Bala
import SharedTypes

width, height :: Int
width = 640
height = 480

angleDiff, minAngle, maxAngle :: Float
angleDiff = 5 * (pi / 180)
minAngle = 5 * (pi / 180)
maxAngle = 85 * (pi / 180)

defaultAmountFuel, defaultOffSetBar :: Float
defaultFuelBar, defaulCurrentFuelBar :: (Float, Float, Float, Float)
defaultAmountFuel = 100
defaultOffSetBar = 0
defaulCurrentFuelBar = (0, 200, 200, 31)
defaultFuelBar = (0, 200, 200, 31)

defaultHealth :: Int
defaultHealthBarPlayer1, defaultHealthBarPlayer2 :: (Float, Float, Float, Float)
defaultHealth = 30
defaultHealthBarPlayer1 = (- (fromIntegral width / 10) * 4, (fromIntegral height / 4), 30, 150)
defaultHealthBarPlayer2 = ((fromIntegral width / 10) * 4, (fromIntegral height / 4), 30, 150)

player1Tank :: Tank
player1Tank = Tank {
    position = (-(fromIntegral width / 4), -(fromIntegral height / 2) + 25),
    health = defaultHealth,
    healthBar = defaultHealthBarPlayer1,
    currentHealthBar = defaultHealthBarPlayer1,
    healthOffset = 0,
    angle = 90 * (pi / 180),
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