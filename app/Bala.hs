module Bala (defaultBullet,Bullet(..),createBullet,bulletRadius,gravity,drawBullet) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

-- Definimos los parámetros de la simulación

bulletRadius, gravity :: Float
gravity = -40
bulletRadius = 7/2

data Bullet = Bullet
    { bPosition :: (Float, Float)
    , bVelocity :: (Float, Float)  -- (velocidad horizontal, velocidad vertical)
    , bDamage :: Int
    } deriving (Show)


defaultBullet :: Bullet
defaultBullet = Bullet
    { bPosition = (0,0)
    , bVelocity = (0,0)
    , bDamage = 0
    }

-- definir la bala
createBullet :: (Float, Float) -> Float -> Float -> Float -> StdGen -> (Bullet,StdGen)
createBullet (xPlayer, yPlayer) length anglePlayer velPlayer gen = 
    let ((xVel, yVel),gen') = calcVelocity velPlayer anglePlayer gen
        (dmg, gen'') = calcDmg gen'
        (x,y) = calcPos (xPlayer, yPlayer) anglePlayer length 
    in (Bullet (x,y) (xVel, yVel) dmg , gen'')

-- posicion de la bala para que salga delante del cañon
calcPos::(Float,Float) -> Float -> Float -> (Float,Float)
calcPos (xPos,yPos) angle radio =
    let x = xPos + radio*cos(angle) + bulletRadius
        y = yPos + radio*sin(angle) + bulletRadius
    in (x,y)

-- calculo del vector de velocidad de la bala
calcVelocity:: Float -> Float -> StdGen -> ((Float,Float),StdGen)
calcVelocity vel angle gen =
    let (plusAngle,gen') = randomR(-5,5) gen :: (Float, StdGen)
        finalAngle = angle + plusAngle
    in ((vel*cos(finalAngle*pi/180),vel*sin(finalAngle*pi/180)), gen')

-- calculo de daño que realizara la bala
calcDmg:: StdGen -> (Int,StdGen)
calcDmg gen =
    let (initialDmg,gen') = randomR (1,3) gen :: (Int,StdGen)
        (critChance,gen'') = randomR(1,20) gen' :: (Int,StdGen)
        finalDamage = initialDmg + if critChance == 1 then 6 else 0
    in (finalDamage,gen'')

-- Fin definiciones para Bullet especifica

-- Función que dibuja la bala
drawBullet :: Bullet -> Bool -> Picture
drawBullet (Bullet (x, y) _ _) value = if value then translate x y $ color red $ circleSolid (bulletRadius*2) else blank