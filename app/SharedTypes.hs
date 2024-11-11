module SharedTypes (Bullet(..), Tank(..)) where

import Graphics.Gloss

data Bullet = Bullet
    { bPosition :: (Float, Float)  -- (Posicion X, Posicion Y)
    , bVelocity :: (Float, Float)  -- (velocidad horizontal, velocidad vertical)
    , bDamage :: Int  -- Daño que produce la bala
    } deriving (Show)


data Tank = Tank
    { position :: (Float, Float)  -- Posición del tanque (x, y)
    , health :: Int -- Vida actual del tanque
    , healthBar :: (Float, Float, Float, Float) -- Contenedor para la barra de vida (offset x, offset y, width, height)
    , currentHealthBar :: (Float, Float, Float, Float) -- Barra de vida (offset x, offset y, width, height)
    , healthOffset :: Float -- Guarda offset vertical actual de la barra de vida
    , angle    :: Float -- Ángulo de dirección del tanque
    , isShooting :: Bool -- Estado del disparo
    , currentBullet :: Bullet -- Bala con los datos de posicion
    , bodySize :: (Float, Float) -- Tamaño del cuerpo (ancho, alto)
    , cannonSize :: (Float, Float) -- Tamaño del cañón (ancho, largo)
    , colorBody :: Color -- Color del cuerpo del tanque
    , colorCannon :: Color -- Color del cañón del tanque
    , tankVel :: Float -- Velocidad del tanque
    , moveLeft :: Bool -- Movimiento hacia la izquierda
    , moveRight :: Bool -- Movimiento hacia la derecha
    , direction :: Int -- Dirección (0 = izq, 1 = der)
    , fuelBar :: (Float, Float, Float, Float) -- Contenedor para la barra de combustible (offset x, offset y, width, height)
    , amountFuel :: Float -- Combustible actual del tanque
    , currentFuelBar :: (Float, Float, Float, Float) -- Barra de vida (offset x, offset y, width, height)
    , offsetBar :: Float -- Guarda offset horizontal actual de la barra de combustible
    , barWidth :: Float -- Guarda ancho actual de la barra de combustible
    , shotUsage :: Float -- Cuanto combustible usa disparar
    , moveUsage :: Float -- Cuanto combustible usa moverse
    , cannonUsage :: Float -- Cuanto combustible usa apuntar el cañon
    , percentage :: String -- Porcentaje de combustible actual como string
    , moveUp :: Bool -- Si el cañon esta rotando hacia arriba
    , moveDown :: Bool -- Si el cañon esta rotando hacia abajo
} deriving (Show)