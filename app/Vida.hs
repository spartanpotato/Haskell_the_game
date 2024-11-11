module Vida(makeHealthBar, totalHealth, showHealth) where
 
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Colores

-- Crea una barra contenedora para la barra que muestra la vida actual con offset y dimensiones dadas
makeHealthBar :: (Float, Float, Float, Float) -> Picture -- (offset x, offset y, width, height)
makeHealthBar (offsetX, offsetY, width, height) = pictures [bar, insideBar] where
    bar =  translate offsetX offsetY $ color black $ rectangleSolid (width + 6) (height + 6)
    insideBar =  translate offsetX offsetY $ color white $ rectangleSolid width height

-- Crea una barra que muestra la vida actual con offset y dimensiones dadas
totalHealth :: (Float, Float, Float, Float) -> Picture -- (offset x, offset y, width, height)
totalHealth (offsetX, offsetY, width, height) = 
    translate offsetX offsetY $ color darkRed $ rectangleSolid width height

-- Muesta cantidad de vida actual como numeros con dimensiones y scale dados
showHealth :: (Float, Float, Float, Float, Float) -> Picture -- (offset x, offset y, width, height, scale)
showHealth (offsetX, offsetY, sx, sy, amount) = translate offsetX offsetY $ Scale sx sy $ Color black $ Text $ show amount