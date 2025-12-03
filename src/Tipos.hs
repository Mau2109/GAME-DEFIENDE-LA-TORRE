{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tipos
  ( -- Generales
    Posicion, Casilla(..)
    -- Oleadas (si quisieras extender)
  , AparicionEnemigo(..), Oleada(..)
    -- Entidades
  , Efecto(..), Enemigo(..)
  , TorreTipo(..), Torre(..)
  , EstadoJuego(..)
    -- Comandos
  , Comando(..)
  ) where

import GHC.Generics (Generic)

-- ======= Grilla / Mapa =======
type Posicion = (Int, Int)

data Casilla
  = Hierba
  | Camino
  | Base
  | Entrada
  deriving (Show, Eq, Generic)

-- ======= Oleadas (opcional extender) =======
data AparicionEnemigo = AparicionEnemigo
  { tiempoAparicion :: Int
  , hpAparicion     :: Int
  , velocidadApar   :: Double
  } deriving (Show, Eq, Generic)

data Oleada = Oleada
  { idOleada    :: Int
  , apariciones :: [AparicionEnemigo]
  } deriving (Show, Eq, Generic)

-- ======= Enemigos / Efectos =======
data Efecto
  = SinEfecto
  | Lento { ticks :: Int, factor :: Double }  -- factor < 1
  deriving (Show, Eq, Generic)

data Enemigo = Enemigo
  { idEnemigo      :: Int
  , hpEnemigo      :: Int
  , hpMax          :: Int
  , indicePosicion :: Int
  , progreso       :: Double
  , velocidadBase  :: Double
  , efecto         :: Efecto
  } deriving (Show, Eq, Generic)

-- ======= Torres =======
data TorreTipo = Arquera | Canon | Mago
  deriving (Show, Eq, Generic)

data Torre = Torre
  { idTorre      :: Int
  , posTorre     :: Posicion
  , tipoTorre    :: TorreTipo
  , nivelTorre   :: Int
  , danioBase    :: Int
  , rangoTorre   :: Int
  , enfriamiento :: Int
  , cdActual     :: Int
  , hpTorre      :: Int       -- NUEVO: vida actual de la torre
  , hpMaxTorre   :: Int       -- NUEVO: vida máxima de la torre
  } deriving (Show, Eq, Generic)

-- ======= Estado del juego =======
data EstadoJuego = EstadoJuego
  { ejTick               :: Int
  , ejCamino             :: [Posicion]
  , ejEntrada            :: Posicion
  , ejBase               :: Posicion
  , ejEnemigos           :: [Enemigo]
  , ejTorres             :: [Torre]
  , ejSiguienteIdEnemigo :: Int
  , ejSiguienteIdTorre   :: Int
  , ejMonedas            :: Int
  , ejMaxTorres          :: Int
  , ejVidaBase           :: Int
  , ejGameOver           :: Bool      -- NUEVO: fin de juego inmediato
  } deriving (Show, Eq, Generic)

-- ======= Comandos JSON =======
data Comando
  = CmdColocarTorre Posicion TorreTipo
  | CmdIniciarOleada
  | CmdNoop
  deriving (Show, Eq, Generic)
