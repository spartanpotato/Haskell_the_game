module Combustible(makeFuelBar, totalFuel, getFuel, showFuel) where
 
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Colores

-- Crea (dibuja) el contenedor del combustible
-- 
-- Parametros:
--   (offsetX, offsetY, width, height) - Las coordenadas de desplazamiento y las dimensiones de la barra.
--
-- salida:
-- imagen del contenedor.
makeFuelBar :: (Float, Float, Float, Float) -> Picture
makeFuelBar (offsetX, offsetY, width, height) = pictures [bar, insideBar] where
    bar =  translate offsetX offsetY $ color black $ rectangleSolid (width + 6) (height + 6)
    insideBar =  translate offsetX offsetY $ color white $ rectangleSolid width height

-- Crea la barra que representa el nivel de combustible
-- 
-- Parametros:
--   (offsetX, offsetY, width, height) - Las coordenadas de desplazamiento y las dimensiones de la barra.
--
-- salida:
-- imagen del nivel del combustible
totalFuel :: (Float, Float, Float, Float) -> Picture
totalFuel (offsetX, offsetY, width, height) = 
    translate offsetX offsetY $ color lightPurple $ rectangleSolid width height 


-- Funcion que entrega el ancho del combustible, la cantidad INICIAL, equivalente al tamaño de la barrita
-- 
-- Parametros:
--   (offsetX, offsetY, width, height) - Las coordenadas de desplazamiento y las dimensiones de la barra.
--
-- salida:
-- El ancho inicial de la barra de combustible.
getFuel :: (Float, Float, Float, Float) -> Float
getFuel (offsetX, offsetY, width, height) = width - 20

-- scale : entre 0 1 -> imagen mas chica, mayor a 1 -> imagen mas grande
-- Dibuja la cantidad de combustible restante como texto. 
-- Parámetros:
--   (offsetX, offsetY, sx, sy, amount) - Las coordenadas de desplazamiento, el tamaño del texto y la cantidad de combustible.
--
-- Salida:
--   Una imagen del texto que muestra la cantidad de combustible.
showFuel :: (Float, Float, Float, Float, Float) -> Picture
showFuel (offsetX, offsetY, sx, sy, amount) = translate offsetX offsetY $ Scale sx sy $ Color black $ Text $ show amount
