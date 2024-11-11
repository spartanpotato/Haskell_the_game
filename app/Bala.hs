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

-- Bala default para definir inicialmente
defaultBullet :: Bullet
defaultBullet = Bullet
    { bPosition = (0,0)
    , bVelocity = (0,0)
    , bDamage = 0
    }

-- Funcion que crea la bala
-- Toma la posicion del jugador, el largo y el angulo del cañon,
-- la velocidad, el id del jugador actual y el generador, devolviendo el
-- generador actualizado y una bala
createBullet :: (Float, Float) -> Float -> Float -> Float -> Int -> StdGen -> (Bullet,StdGen)
createBullet (xPlayer, yPlayer) length anglePlayer velPlayer idPlayer gen = 
    let newAngle = if idPlayer == 1 then pi/2 - anglePlayer else (-anglePlayer + pi/2)
        ((xVel, yVel),gen') = calcVelocity velPlayer newAngle gen
        (dmg, gen'') = calcDmg gen'
        (x,y) = calcPos (xPlayer, yPlayer) newAngle length 
    in (Bullet (x,y) (xVel, yVel) dmg , gen'')

-- posicion de la bala para que salga delante del cañon

-- Funcion que calcula la posición de donde comienza la bala
-- Toma la posicion, el angulo y la mitad del largo del cañon como argumento
-- devolviendo la posicion de donde saldra
calcPos :: (Float, Float) -> Float -> Float -> (Float, Float)
calcPos (xPos, yPos) angle radio =
    let x = xPos + radio * cos(angle) + bulletRadius
        y = yPos + radio * sin(angle) + bulletRadius
        result = (x, y)
    in result


-- Funcion que calcula la velocidad con la que saldra la bala
-- Toma como argumento la velocidad inicial, el angulo y el generador
-- devuelve la velocidad en forma de vector y el generador
calcVelocity:: Float -> Float -> StdGen -> ((Float,Float),StdGen)
calcVelocity vel angle gen =
    let (plusAngle,gen') = randomR(((-5)*(pi/180)),(5*(pi/180))) gen :: (Float, StdGen)
        finalAngle = angle + plusAngle
    in ((vel*cos(finalAngle),vel*sin(finalAngle)), gen')

-- Funcion que devuelve el daño realizado por la bala
-- toma el generador y devuelve el daño realizado y el generador
calcDmg:: StdGen -> (Int,StdGen)
calcDmg gen =
    let (initialDmg,gen') = randomR (1,3) gen :: (Int,StdGen)
        (critChance,gen'') = randomR(1,20) gen' :: (Int,StdGen)
        finalDamage = initialDmg + if critChance == 1 then 6 else 0
    in (finalDamage,gen'')

-- Fin definiciones para Bullet especifica

-- Función que dibuja la bala
-- toma como argumento la bala y el estado de la bala
-- devuelve la imagen a dibujar
drawBullet :: Bullet -> Bool -> Picture
drawBullet (Bullet (x, y) _ _) value = if value then translate x y $ color red $ circleSolid (bulletRadius*2) else blank

-- Funcion que itera el movimiento simple
-- toma el estado de la bala, los segundos y la bala,
-- devolviendo la bala actualizada en posicion y su estado
moveBullet :: Bool -> Float -> Bullet -> (Bullet,Bool)
moveBullet shoot seconds bullet
    | shoot = (updateBullet seconds bullet)
    | otherwise = (bullet,False)

-- Funcion que actualiza la posicion de la bala
-- toma como argumentos la bala y los segundos
-- que retorna la nueva posicion de la bala y su estado
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

-- Funcion que verifica la colision con un tanque
-- toma como argumentos la bala y el tanque oponente
-- devolviendo verdadero si impacta y falso si no
collitionBullet :: Bullet -> Tank -> Bool
collitionBullet bullet tank =
    let (posBullX, posBullY) = bPosition bullet
        (tankX, tankY) = position tank
        (tankWidth, tankHeight) = bodySize tank
    in (posBullX >=tankX-(tankWidth/2 + bulletRadius) && posBullX<= tankX+(tankWidth/2 + bulletRadius)
    && posBullY >=tankY-(tankHeight/2 + bulletRadius) && posBullY <=tankY+(tankHeight/2 + bulletRadius))