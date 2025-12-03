{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson.Types (Parser)
import System.IO (isEOF, hPutStrLn, stderr)

import Tipos
import qualified Simulacion as S
import Generador (generarMapaLaberinto)
import System.Random (mkStdGen)

-- JSON -> Comando
instance A.FromJSON Comando where
  parseJSON = A.withObject "Comando" $ \o -> do
    t <- o A..: "cmd" :: Parser String
    case t of
      "noop" -> pure CmdNoop
      "iniciar_oleada" -> pure CmdIniciarOleada
      "colocar_torre" -> do
        pos <- o A..: "pos" :: Parser [Int]
        tip <- o A..:? "tipo" A..!= ("arquera" :: String)
        let tipo = case tip of
                     "arquera" -> Arquera
                     "canon"   -> Canon
                     "mago"    -> Mago
                     _         -> Arquera
        case pos of
          (x:y:_) -> pure (CmdColocarTorre (x,y) tipo)
          _       -> fail "pos debe ser [x,y]"
      _ -> fail ("comando desconocido: " ++ t)

-- Bucle: lee lineas JSON, ejecuta paso, emite estado
bucle :: EstadoJuego -> [Posicion] -> IO ()
bucle estado camino = do
  eof <- isEOF
  if eof then do
      -- si stdin se cierra, seguimos avanzando con noop para no morir
      let est' = S.paso camino estado CmdNoop
      BL.putStrLn (A.encode (S.serializarEstadoJSON camino est'))
      bucle est' camino
    else do
      linea <- getLine
      let cmd = case A.eitherDecode' (BL.pack linea) of
                  Left err -> (hPutStrLn stderr ("[motor] JSON parse error: " ++ err) >> pure CmdNoop)
                  Right c  -> pure c
      c <- cmd
      let est' = S.paso camino estado c
      BL.putStrLn (A.encode (S.serializarEstadoJSON camino est'))
      bucle est' camino

main :: IO ()
main = do
  -- mapa grande, camino laberíntico
  let ancho = 28
      alto  = 18
      curv  = 30   -- 0..100: mayor -> más giros
      (mapa, camino, entrada, base) = generarMapaLaberinto ancho alto curv (mkStdGen 20251203)
      estado0 = S.crearEstadoInicial camino entrada base
  hPutStrLn stderr "[motor] Motor Haskell (tick por comando) listo."
  BL.putStrLn (A.encode (S.serializarEstadoJSON camino estado0))
  bucle estado0 camino
