{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where

import Simulacion
import Generador (generarMapaLaberinto)
import Tipos

import Data.Aeson (decode, encode)
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Vector as V
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
#else
import qualified Data.HashMap.Strict as HM
#endif
import System.IO
import System.Random (mkStdGen)

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  hPutStrLn stderr "[motor] === MOTOR ACTUALIZADO CON ARTILLERIA ==="
  
  let ancho = 28
      alto = 18
      curvatura = 35
      gen = mkStdGen 42
      (mapa, camino, entrada, base) = generarMapaLaberinto ancho alto curvatura gen
      estadoInicial = crearEstadoInicial camino entrada base
  
  hPutStrLn stderr $ "[motor] Mapa: " ++ show (length camino) ++ " nodos"
  hPutStrLn stderr "[motor] Listo. Esperando comandos..."

  BLC.putStrLn $ encode (serializarEstadoJSON camino estadoInicial)
  hFlush stdout
  
  bucle camino estadoInicial

bucle :: [Posicion] -> EstadoJuego -> IO ()
bucle camino est = do
  lineaStr <- getLine
  case decode (BLC.pack lineaStr) of
    Nothing -> bucle camino est
    Just obj -> do
      cmd <- parsearComando obj
      let nuevoEst = paso camino est cmd
      BLC.putStrLn $ encode (serializarEstadoJSON camino nuevoEst)
      hFlush stdout
      bucle camino nuevoEst

#if MIN_VERSION_aeson(2,0,0)
lookupKey :: T.Text -> A.Object -> Maybe A.Value
lookupKey k = KM.lookup (K.fromText k)
#else
lookupKey :: T.Text -> A.Object -> Maybe A.Value
lookupKey = HM.lookup
#endif

parsearComando :: A.Value -> IO Comando
parsearComando (A.Object v) = do
  case lookupKey "cmd" v of
    Just (A.String "colocar_torre") -> do
      let Just (A.Array posArr) = lookupKey "pos" v
          [A.Number x, A.Number y] = take 2 (V.toList posArr)
          Just (A.String tipoStr) = lookupKey "tipo" v
          pos = (round x, round y)
          tipoTxt = T.unpack tipoStr
      
      hPutStrLn stderr $ "[DEBUG] Tipo JSON: '" ++ tipoTxt ++ "'"
      
      let tipo | tipoTxt == "arquera"    = Arquera
               | tipoTxt == "canon"      = Canon
               | tipoTxt == "mago"       = Mago
               | tipoTxt == "artilleria" = Artilleria
               | otherwise               = Arquera
      
      hPutStrLn stderr $ "[DEBUG] Tipo parseado: " ++ show tipo
      return $ CmdColocarTorre pos tipo
    
    Just (A.String "iniciar_oleada") -> return CmdIniciarOleada
    
    Just (A.String "disparar_artilleria") -> do
      let Just (A.Number idNum) = lookupKey "id_torre" v
          Just (A.Array objArr) = lookupKey "objetivo" v
          [A.Number ox, A.Number oy] = take 2 (V.toList objArr)
      return $ CmdDispararArtilleria (round idNum) (realToFrac ox, realToFrac oy)
    
    Just (A.String "noop") -> return CmdNoop
    _ -> return CmdNoop

parsearComando _ = return CmdNoop