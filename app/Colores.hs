module Colores(lightPurple, darkPurple,
                darkBlue, yellow',
                orange', blue') where

import Graphics.Gloss

lightPurple :: Color
lightPurple = makeColor 0.6 0.3 1 1

darkPurple :: Color
darkPurple = makeColor 0.4 0.1 0.8 1 

darkBlue :: Color
darkBlue = makeColor 0.1 0.1 0.2 1

yellow' :: Color
yellow' = makeColor 1 1 0.5 1

orange' :: Color
orange' = makeColor 1 0.5 0.2 1

blue' :: Color
blue' = makeColor 0.2549 0.5961 1.0 1.0
