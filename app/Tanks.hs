module Tanks (Tank(..), player1Tank, player2Tank, angleDiff, minAngle, maxAngle) where

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

player1Tank :: Tank
player1Tank = Tank {
    position = (-(fromIntegral width / 4), -(fromIntegral height / 2) + 25),
    health = 30,
    angle = 45 * (pi / 180),
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
    percentage = "100%",
    moveUp = False,
    moveDown = False
}

player2Tank :: Tank
player2Tank = Tank {
    position = (fromIntegral width/4, -(fromIntegral height / 2) + 25),
    health = 30,
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
    fuelBar = (-200, 200, 200, 50),
    amountFuel = 100,
    currentFuelBar = (-200, 200, 180, 31),
    barWidth = 180,
    shotUsage = 20, 
    moveUsage = 10,
    cannonUsage = 5,
    offsetBar = -200,
    percentage = "100%",
    moveUp = False,
    moveDown = False
}