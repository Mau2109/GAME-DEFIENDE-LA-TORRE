{-# LANGUAGE OverloadedStrings #-}

module Generador
  ( generarMapaLaberinto ) where

import System.Random (StdGen, randomR, mkStdGen)
import Tipos (Posicion, Casilla(..))
import qualified Data.Map as M
import Data.Map (Map)
import Data.List (nub)

-- Probabilidad de girar (0..99). Sube el número para más curvas.
curvatura :: Int
curvatura = 35

-- Un camino “serpenteante” largo de izquierda a derecha con
-- pequeñas bifurcaciones internas para que no sea una línea recta.
-- Retorna: (mapa, camino, entrada, base)
generarMapaLaberinto :: Int -> Int -> Int -> StdGen -> (Map Posicion Casilla, [Posicion], Posicion, Posicion)
generarMapaLaberinto ancho alto curvatura gen0 =
  let entrada = (1, alto `div` 2)
      base    = (ancho - 2, alto `div` 2)
      camino  = serpentina entrada base curvatura gen0 ancho alto
      mapa0   = M.fromList [ ((x,y), Hierba) | x <- [0..ancho-1], y <- [0..alto-1] ]
      mapa1   = foldl (\m p -> M.insert p Camino m) mapa0 camino
      mapa2   = M.insert entrada Entrada (M.insert base Base mapa1)
  in (mapa2, camino, entrada, base)

-- Construcción de camino “serpenteante” con cambios de dirección controlados
serpentina :: Posicion -> Posicion -> Int -> StdGen -> Int -> Int -> [Posicion]
serpentina (x0,y0) (x1,y1) curv gen ancho alto =
  let (dx,dy) = (signum (x1-x0), signum (y1-y0))
      baseDir = if dx == 0 then (1,0) else (dx,0)
  in takeUnique $ build (x0,y0) baseDir gen 0
  where
    -- evita repetidos consecutivos
    takeUnique [] = []
    takeUnique (a:xs) = a : go a xs
    go _ [] = []
    go prev (b:xs) = (if prev==b then go prev xs else b:go b xs)

    build (x,y) dir g steps
      | (x,y) == (x1,y1) = [(x,y)]
      | steps > ancho*alto*5 = [(x,y)]  -- guarda-vidas
      | otherwise =
          let (r, g1) = randomR (0, 100 :: Int) g
              dir' = if r < curvatura then girar dir g1 else dir
              (nx,ny) = (x + fst dir', y + snd dir')
              nx' = max 1 (min (ancho-2) nx)
              ny' = max 1 (min (alto-2) ny)
              next = (nx', ny')
          in (x,y) : build next dir' g1 (steps+1)

    -- girar ±90° con aleatoriedad
    girar (dx,dy) g =
      let (z, _) = randomR (0,1 :: Int) g
      in if dx == 0 && dy == 0 then (1,0)
         else if z == 0 then (-dy, dx) else (dy, -dx)
