module Tanks (Tank(..), player1Tank, player2Tank) where

import Graphics.Gloss
import Colores
import Bala
import SharedTypes

width, height :: Int
width = 640
height = 480

player1Tank :: Tank
player1Tank = Tank {
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
}

player2Tank :: Tank
player2Tank = Tank {
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
}