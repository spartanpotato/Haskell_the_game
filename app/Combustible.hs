module Combustible(makeFuelBar, totalFuel, getFuel, showFuel) where
 
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Colores

-- Hace la barrita del combustible 
makeFuelBar :: (Float, Float, Float, Float) -> Picture
makeFuelBar (offsetX, offsetY, width, height) = pictures [bar, insideBar] where
    bar =  translate offsetX offsetY $ color black $ rectangleSolid (width + 6) (height + 6)
    insideBar =  translate offsetX offsetY $ color white $ rectangleSolid width height

-- Hace la barrita de la cantidad de combustible que hay
totalFuel :: (Float, Float, Float, Float) -> Picture
totalFuel (offsetX, offsetY, width, height) = 
    translate offsetX offsetY $ color lightPurple $ rectangleSolid width height 

-- Funcion que entrega el ancho del combustible, la cantidad INICIAL, equivalente al tamaño de la barrita
getFuel :: (Float, Float, Float, Float) -> Float
getFuel (offsetX, offsetY, width, height) = width - 20

-- scale : entre 0 1 -> imagen mas chica, mayor a 1 -> imagen mas grande
showFuel :: (Float, Float, Float, Float, Float) -> Picture
showFuel (offsetX, offsetY, sx, sy, amount) = translate offsetX offsetY $ Scale sx sy $ Color black $ Text $ show amount
