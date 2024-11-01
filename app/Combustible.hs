module Combustible(makeFuelBar, totalFuel, getFuel) where
 
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Colores

-- Hace la barrita del combustible 
makeFuelBar :: (Float, Float, Float, Float) -> Picture
makeFuelBar (offsetX, offsetY, width, height) = pictures [bar, insideBar] where
    bar =  translate offsetX offsetY $ color darkPurple $ rectangleSolid width height
    insideBar =  translate offsetX offsetY $ color white $ rectangleSolid (width - 10) (height - 10)

-- Hace la barrita de la cantidad de combustible que hay
totalFuel :: (Float, Float, Float, Float) -> Picture
totalFuel (offsetX, offsetY, width, height) = 
    translate offsetX offsetY $ color lightPurple $ rectangleSolid width height

-- Funcion que entrega el "100" del combustible, la cantidad INICIAL, equivalente al tamaño de la barrita
getFuel :: (Float, Float, Float, Float) -> Float
getFuel (offsetX, offsetY, width, height) = width - 20


-- fuelUsage :: 