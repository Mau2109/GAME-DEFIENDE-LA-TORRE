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
    -- NUEVO: Proyectiles
  , Proyectil(..)
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
  | Lento { ticks :: Int, factor :: Double }
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
data TorreTipo 
  = Arquera 
  | Canon 
  | Mago
  | Artilleria  -- NUEVO: Torre que lanza bombas con apuntado manual
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
  , hpTorre      :: Int
  , hpMaxTorre   :: Int
  } deriving (Show, Eq, Generic)

-- ======= NUEVO: Proyectiles (bombas en vuelo) =======
data Proyectil = Proyectil
  { idProyectil     :: Int
  , posOrigenProy   :: (Double, Double)  -- posición de lanzamiento (continua)
  , posDestinoProy  :: (Double, Double)  -- posición objetivo (continua)
  , posActualProy   :: (Double, Double)  -- posición actual en el aire
  , progresoVuelo   :: Double            -- 0.0 a 1.0
  , velocidadProy   :: Double            -- velocidad de viaje
  , danioProy       :: Int               -- daño al impactar
  , radioExplosion  :: Double            -- radio de área de efecto
  , idTorreProy     :: Int               -- torre que lo lanzó
  } deriving (Show, Eq, Generic)

-- ======= Estado del juego =======
data EstadoJuego = EstadoJuego
  { ejTick               :: Int
  , ejCamino             :: [Posicion]
  , ejEntrada            :: Posicion
  , ejBase               :: Posicion
  , ejEnemigos           :: [Enemigo]
  , ejTorres             :: [Torre]
  , ejProyectiles        :: [Proyectil]  -- NUEVO: lista de proyectiles en vuelo
  , ejSiguienteIdEnemigo :: Int
  , ejSiguienteIdTorre   :: Int
  , ejSiguienteIdProy    :: Int          -- NUEVO: contador de proyectiles
  , ejMonedas            :: Int
  , ejMaxTorres          :: Int
  , ejVidaBase           :: Int
  , ejGameOver           :: Bool
  } deriving (Show, Eq, Generic)

-- ======= Comandos JSON =======
data Comando
  = CmdColocarTorre Posicion TorreTipo
  | CmdIniciarOleada
  | CmdNoop
  | CmdDispararArtilleria Int (Double, Double)  -- NUEVO: idTorre, posición objetivo (continua)
  deriving (Show, Eq, Generic)