module SharedTypes (Bullet(..), Tank(..)) where

import Graphics.Gloss

data Bullet = Bullet
    { bPosition :: (Float, Float)  -- (Posicion X, Posicion Y)
    , bVelocity :: (Float, Float)  -- (velocidad horizontal, velocidad vertical)
    , bDamage :: Int  -- Daño que produce la bala
    } deriving (Show)


data Tank = Tank
    { position :: (Float, Float)  -- Posición del tanque (x, y)
    , health :: Int
    , healthBar :: (Float, Float, Float, Float)
    , currentHealthBar :: (Float, Float, Float, Float)
    , healthOffset :: Float
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
    , moveUp :: Bool
    , moveDown :: Bool
} deriving (Show)