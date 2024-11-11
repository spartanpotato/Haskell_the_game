# Haskell_the_game
Un juego para dos jugadores desarrollado en Haskell usando principalmente la libreria gloss.  
## Instrucciones
El juego consiste en dos tanques, separados por una pared, que pueden:  
- Moverse hacia los lados con 'a' o 'd'
- Apuntar el cañón hacia arriba o abajo con 'w' o 's'.
- Disparar el cañón con 'e'
Cada acción gasta una cantidad de combustible (10, 5 y 20 respectivamente)  
Al acabarse el combustible de un jugador termina su turno.  
Cada tanque tiene 30 de vida.  
El objetivo del juego es lograr que la vida del oponente llegue a 0, en cuyo caso se puede volver a jugar con 'r'  

## Para compilar:
En una terminal muevase a la carpeta 'Haskell_the_game/app' del proyecto, y ejecute el comando:  
make  
Para limpiar ejecutables ejecute el comando:  
make clean  
desde la misma carpeta.

## Para ejecutar:
Una vez compilado el programa, desde la terminal, en la carpeta 'Haskell_the_game/app', ejecute:  
./main

## Librerias necesarias:
- Gloss
- Random

# Documentos externos

BITACORA: https://docs.google.com/document/d/1bPAGj64Dc7gKxz2paBIS54InxIzt53O7qZTqCM2f1wE/edit?usp=sharing

INSTRUCCIONES: https://docs.google.com/document/d/1IZkwZrJkybahJJ_AHpF0jcm-BudCkEdeSZJjJl7UnDA/edit?usp=sharing

INFORME: https://docs.google.com/document/d/1xpMMi78uMj506fZDoFzaNUXsKmd3Po7HVse_z6prwow/edit?usp=sharing
