module Bala (defaultBullet,
            Bullet(..),
            createBullet,
            bulletRadius,
            gravity,
            drawBullet,
            moveBullet,
            updateBullet,
            collitionBullet) where

import Debug.Trace (trace)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import SharedTypes

-- Definimos los parámetros de la simulación

bulletRadius, gravity :: Float
gravity = -100
bulletRadius = 7/2


width, height, offset :: Int
width = 640
height = 480
offset = 100


defaultBullet :: Bullet
defaultBullet = Bullet
    { bPosition = (0,0)
    , bVelocity = (0,0)
    , bDamage = 0
    }

-- definir la bala
createBullet :: (Float, Float) -> Float -> Float -> Float -> Int -> StdGen -> (Bullet,StdGen)
createBullet (xPlayer, yPlayer) length anglePlayer velPlayer idPlayer gen = 
    let newAngle = if idPlayer == 1 then pi/2 - anglePlayer else (-anglePlayer + pi/2)
        ((xVel, yVel),gen') = calcVelocity velPlayer newAngle gen
        (dmg, gen'') = calcDmg gen'
        (x,y) = calcPos (xPlayer, yPlayer) newAngle length 
    in (Bullet (x,y) (xVel, yVel) dmg , gen'')

-- posicion de la bala para que salga delante del cañon

calcPos :: (Float, Float) -> Float -> Float -> (Float, Float)
calcPos (xPos, yPos) angle radio =
    let x = xPos + radio * cos(angle) + bulletRadius
        y = yPos + radio * sin(angle) + bulletRadius
        result = (x, y)
    in result


-- calculo del vector de velocidad de la bala
calcVelocity:: Float -> Float -> StdGen -> ((Float,Float),StdGen)
calcVelocity vel angle gen =
    let (plusAngle,gen') = randomR(((-5)*(pi/180)),(5*(pi/180))) gen :: (Float, StdGen)
        finalAngle = angle + plusAngle
    in ((vel*cos(finalAngle),vel*sin(finalAngle)), gen')

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
        clampedX = min ((fromIntegral width / 2)) (max (-(fromIntegral width / 2)) newX)
        -- Verificar colisiones y actualizar posición y velocidad
        newPosition = if newY <= -(fromIntegral height / 2) + bulletRadius + 20 || 
                         newX <= -(fromIntegral width / 2) + bulletRadius|| 
                         newX >= (fromIntegral width / 2) - bulletRadius
                      then (clampedX, max newY ((fromIntegral height / 2) - bulletRadius )) -- Posición en caso de colisión
                      else (clampedX, newY)
        -- Si colisiona, detener la bala; de lo contrario, aplicar la nueva velocidad
        newVelocity = if newY <= -(fromIntegral height / 2) + bulletRadius + 20 || 
                         newX <= -(fromIntegral width / 2) + bulletRadius  || 
                         newX >= (fromIntegral width / 2) - bulletRadius
                      then (0, 0)
                      else (vx, newVy)
        shot = newVelocity /= (0,0)
    in (bullet { bPosition = newPosition, bVelocity = newVelocity },shot)


collitionBullet :: Bullet -> Tank -> Bool
collitionBullet bullet tank =
    let (posBullX, posBullY) = bPosition bullet
        (tankX, tankY) = position tank
        (tankWidth, tankHeight) = bodySize tank
    in (posBullX >=tankX-(tankWidth/2 + bulletRadius) && posBullX<= tankX+(tankWidth/2 + bulletRadius)
    && posBullY >=tankY-(tankHeight/2 + bulletRadius) && posBullY <=tankY+(tankHeight/2 + bulletRadius))