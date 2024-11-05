module Vida(makeHealthBar, totalHealth, getHealth, showHealth) where
 
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Colores

-- Crea una barra contenedora para la barra que muestra la vida actual
makeHealthBar :: (Float, Float, Float, Float) -> Picture
makeHealthBar (offsetX, offsetY, width, height) = pictures [bar, insideBar] where
    bar =  translate offsetX offsetY $ color black $ rectangleSolid (width + 6) (height + 6)
    insideBar =  translate offsetX offsetY $ color white $ rectangleSolid width height

-- Crea una barra que muestra la vida actual
totalHealth :: (Float, Float, Float, Float) -> Picture
totalHealth (offsetX, offsetY, width, height) = 
    translate offsetX offsetY $ color darkRed $ rectangleSolid width height

-- Funcion que entrega el ancho del combustible, la cantidad INICIAL, equivalente al tamaño de la barrita
getHealth :: (Float, Float, Float, Float) -> Float
getHealth (offsetX, offsetY, width, height) = width - 20

-- scale : entre 0 1 -> imagen mas chica, mayor a 1 -> imagen mas grande
showHealth :: (Float, Float, Float, Float, Float) -> Picture
showHealth (offsetX, offsetY, sx, sy, amount) = translate offsetX offsetY $ Scale sx sy $ Color black $ Text $ show amount