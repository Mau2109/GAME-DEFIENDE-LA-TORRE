{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Simulacion
  ( paso
  , serializarEstadoJSON
  , crearEstadoInicial
  , Comando(..)
  ) where

import Tipos
import Data.Aeson (Value, (.=), object)
import qualified Data.Aeson as A
import Data.List (foldl', maximumBy, partition)
import Data.Ord (comparing)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

-- ======= parámetros =======
costoArquera, costoCanon, costoMago :: Int
costoArquera = 40
costoCanon   = 60
costoMago    = 80

-- Base de stats por tipo
statsTorre :: TorreTipo -> (Int {-daño-}, Int {-rango-}, Int {-CD-})
statsTorre Arquera = (8, 3, 1)   -- rápido, poco daño
statsTorre Canon   = (18, 3, 2)  -- daño fuerte
statsTorre Mago    = (10, 3, 3)  -- daño medio + slow

-- HP inicial por tipo
hpInicialTorre :: TorreTipo -> Int
hpInicialTorre Arquera = 60
hpInicialTorre Canon   = 90
hpInicialTorre Mago    = 75

-- Daño que hace 1 enemigo a la torre por tick si está tocándola
danioEnemigoATorre :: Int
danioEnemigoATorre = 6

-- monedas al matar
recompensaKill :: Int
recompensaKill = 10

-- efecto Mago
efectoMago :: Efecto
efectoMago = Lento { ticks = 3, factor = 0.6 }

-- ======= estado inicial =======
crearEstadoInicial :: [Posicion] -> Posicion -> Posicion -> EstadoJuego
crearEstadoInicial camino entrada base = EstadoJuego
  { ejTick = 0
  , ejCamino = camino
  , ejEntrada = entrada
  , ejBase = base
  , ejEnemigos = []
  , ejTorres = []
  , ejSiguienteIdEnemigo = 1
  , ejSiguienteIdTorre = 1
  , ejMonedas = 350
  , ejMaxTorres = 12
  , ejVidaBase = 20
  , ejGameOver = False
  }

-- ======= paso =======
paso :: [Posicion] -> EstadoJuego -> Comando -> EstadoJuego
paso camino est0 cmd =
  let est1 = aplicarComando camino est0 cmd
      est2 = actualizarCooldownTorres est1
      est3 = moverEnemigos camino est2
      est4 = enemigosAtacanTorres camino est3    -- NUEVO: enemigos dañan torres si las tocan
      est5 = torresAtacan camino est4
      est6 = aplicarMuertesYRecompensas est5
      est7 = llegarBase camino est6              -- NUEVO: GameOver instantáneo si llegan
      estF = est7 { ejTick = ejTick est7 + 1 }
  in estF

-- ======= comandos =======
aplicarComando :: [Posicion] -> EstadoJuego -> Comando -> EstadoJuego
aplicarComando _ est@(EstadoJuego{..}) (CmdColocarTorre pos tipo) =
  let enCamino = pos `elem` ejCamino
      yaTorre  = any (\t -> posTorre t == pos) ejTorres
      numT     = length ejTorres
      costo = case tipo of Arquera -> costoArquera; Canon -> costoCanon; Mago -> costoMago
  in if enCamino || yaTorre || numT >= ejMaxTorres || ejMonedas < costo || ejGameOver
       then est
       else
         let (dmg, rng, cd) = statsTorre tipo
             hp0 = hpInicialTorre tipo
             nt = Torre { idTorre=ejSiguienteIdTorre, posTorre=pos, tipoTorre=tipo
                        , nivelTorre=1, danioBase=dmg, rangoTorre=rng
                        , enfriamiento=cd, cdActual=0
                        , hpTorre=hp0, hpMaxTorre=hp0 }
         in traceLog ("Colocada torre " ++ show tipo ++ " en " ++ show pos)
            $ est { ejTorres = ejTorres ++ [nt]
                  , ejSiguienteIdTorre = ejSiguienteIdTorre + 1
                  , ejMonedas = ejMonedas - costo }

aplicarComando _ est CmdIniciarOleada
  | ejGameOver est = est
  | otherwise =
      let n   = 60                          -- ← pon 50 si quieres 50 enemigos
          vs  = [0..n-1]
          ids = [ejSiguienteIdEnemigo est .. ejSiguienteIdEnemigo est + n - 1]

          -- Velocidades variadas (cíclico cada 16): 0.40, 0.43, ..., 0.85
          mkV k   = 0.40 + 0.03 * fromIntegral (k `mod` 16)

          -- HP ligeramente creciente
          mkHp k  = 60 + k*3

          -- Progreso inicial para "espaciarlos" en el primer segmento (0.00..0.95)
          -- Así ya nacen distribuidos a lo largo del camino, no todos pegados.
          mkProg k =
            let steps = 20                   -- 20 posiciones: 0.00, 0.05, ..., 0.95
                i     = k `mod` steps
            in  fromIntegral i * (1.0 / fromIntegral steps)  -- 0.0 .. <1.0

          enemigos = [ Enemigo i (mkHp k) (mkHp k) 0 (mkProg k) (mkV k) SinEfecto
                     | (i,k) <- zip ids vs ]

      in traceLog ("Inicia oleada con " ++ show n ++ " enemigos")
         $ est { ejEnemigos = ejEnemigos est ++ enemigos
               , ejSiguienteIdEnemigo = ejSiguienteIdEnemigo est + n }

aplicarComando _ est CmdNoop = est


-- ======= movimiento =======
velocidadActual :: Enemigo -> Double
velocidadActual Enemigo{..} =
  case efecto of
    SinEfecto -> velocidadBase
    Lento{..} -> velocidadBase * factor

moverEnemigos :: [Posicion] -> EstadoJuego -> EstadoJuego
moverEnemigos camino est@EstadoJuego{..} =
  let lenCam = length ejCamino
      step e =
        let prog' = progreso e + velocidadActual e
            (i', p') = avanzarIndice (indicePosicion e) prog' lenCam
            ef' = case efecto e of
                    SinEfecto -> SinEfecto
                    Lento t f -> if t>1 then Lento (t-1) f else SinEfecto
        in e { indicePosicion = i', progreso = p', efecto = ef' }
      ens1 = map step ejEnemigos
      ens2 = filter (\en -> indicePosicion en < lenCam) ens1
  in est { ejEnemigos = ens2 }

avanzarIndice :: Int -> Double -> Int -> (Int, Double)
avanzarIndice idx prog lenCam
  | lenCam <= 0 = (0, 0)
  | prog < 1.0  = (idx, prog)
  | otherwise   =
      let p' = prog - 1.0
          i' = idx + 1
      in if i' >= lenCam then (i', p') else avanzarIndice i' p' lenCam

-- ======= enemigos atacan torres (NUEVO) =======
-- Un enemigo que "toca" una torre (distancia al centro < 0.6 celda) le hace daño por tick.
enemigosAtacanTorres :: [Posicion] -> EstadoJuego -> EstadoJuego
enemigosAtacanTorres camino est@EstadoJuego{..}
  | ejGameOver = est
  | otherwise  =
      let -- posición continua de enemigo
          posEn e = posicionContinua camino e
          -- aplica daño de un enemigo a la torre más cercana que esté tocando
          aplicarDanioDe e torres =
            let (ex,ey) = posEn e
                -- distancia centro a centro en coordenadas de celdas
                dist (tx,ty) = let dx = fromIntegral tx + 0.5 - ex
                                   dy = fromIntegral ty + 0.5 - ey
                               in sqrt (dx*dx + dy*dy)
                -- torres tocadas (r < 0.6 celda ~ están en la misma casilla/adyacente)
                tocadas = filter (\t -> dist (posTorre t) < 0.6) torres
            in case tocadas of
                 []   -> torres
                 t:_  -> -- dañamos la primera tocada (suficiente para el efecto)
                   let t'  = t { hpTorre = hpTorre t - danioEnemigoATorre }
                       ts' = t' : filter (\x -> idTorre x /= idTorre t) torres
                   in ts'
          -- plegado: cada enemigo puede dañar una torre por tick
          torresGolpeadas = foldl (flip aplicarDanioDe) ejTorres ejEnemigos
          -- eliminar torres destruidas
          (muertas, vivas) = partition (\t -> hpTorre t <= 0) torresGolpeadas
          est' = if null muertas
                   then est { ejTorres = vivas }
                   else traceLog ("Torres destruidas: " ++ show (map idTorre muertas))
                        $ est { ejTorres = vivas }
      in est'

-- ======= ataque de torres =======
torresAtacan :: [Posicion] -> EstadoJuego -> EstadoJuego
torresAtacan camino est@EstadoJuego{..}
  | ejGameOver = est
  | otherwise  =
      let aplicar (ens, ts) t@Torre{..}
            | cdActual > 0 = (ens, ts ++ [t { cdActual = cdActual }])
            | otherwise =
                case objetivoMasAvanzado camino t ens of
                  Nothing -> (ens, ts ++ [t])
                  Just eid ->
                    let d = danioBase * nivelTorre
                        ensHit = map (\e -> if idEnemigo e == eid
                                              then aplicarImpacto t e d
                                              else e) ens
                        t' = t { cdActual = enfriamiento }
                    in (ensHit, ts ++ [t'])
          (ensF, tsF) = foldl' aplicar (ejEnemigos, []) ejTorres
          tsCool = map (\t -> if cdActual t > 0 then t else t) tsF
      in est { ejEnemigos = ensF, ejTorres = tsCool }

aplicarImpacto :: Torre -> Enemigo -> Int -> Enemigo
aplicarImpacto Torre{..} e d =
  case tipoTorre of
    Mago  -> e { hpEnemigo = hpEnemigo e - d
               , efecto    = case efecto e of
                               SinEfecto -> efectoMago
                               Lento t f -> Lento (max t (ticks efectoMago)) (min f (factor efectoMago)) }
    _     -> e { hpEnemigo = hpEnemigo e - d }

objetivoMasAvanzado :: [Posicion] -> Torre -> [Enemigo] -> Maybe Int
objetivoMasAvanzado camino Torre{..} ens =
  let posT = posTorre
      dist (ax,ay) (bx,by) =
        let dx = fromIntegral ax - bx
            dy = fromIntegral ay - by
        in sqrt (dx*dx + dy*dy)
      posEn e = posicionContinua camino e
      cand = filter (\e -> dist (fromIntegral (fst posT), fromIntegral (snd posT)) (posEn e)
                           <= fromIntegral rangoTorre + 0.001) ens
  in if null cand
       then Nothing
       else Just (idEnemigo (maximumBy (comparing avance) cand))
  where
    avance e = fromIntegral (indicePosicion e) + progreso e

posicionContinua :: [Posicion] -> Enemigo -> (Double, Double)
posicionContinua camino Enemigo{..} =
  let len = length camino
      idx = min (max 0 indicePosicion) (if len==0 then 0 else len-1)
      prog = max 0.0 (min 1.0 progreso)
      (x0,y0) = if idx < len then camino !! idx else last camino
      siguiente = if idx+1 < len then camino !! (idx+1) else (x0,y0)
      (x1,y1) = siguiente
      fx = fromIntegral x0 + (fromIntegral x1 - fromIntegral x0) * prog
      fy = fromIntegral y0 + (fromIntegral y1 - fromIntegral y0) * prog
  in (fx, fy)

-- ======= muertes, recompensas =======
aplicarMuertesYRecompensas :: EstadoJuego -> EstadoJuego
aplicarMuertesYRecompensas est@EstadoJuego{..} =
  let (muertos, vivos) = partition (\e -> hpEnemigo e <= 0) ejEnemigos
      ganancia = length muertos * recompensaKill
  in if null muertos
        then est
        else traceLog ("Enemigos eliminados: " ++ show (map idEnemigo muertos)
                       ++ " +" ++ show ganancia ++ " monedas")
             $ est { ejEnemigos = vivos, ejMonedas = ejMonedas + ganancia }

-- ======= llegada a base =======
llegarBase :: [Posicion] -> EstadoJuego -> EstadoJuego
llegarBase camino est@EstadoJuego{..} =
  let lenCam = length camino
      (llegados, resto) = partition (\e -> indicePosicion e >= lenCam) ejEnemigos
      perdidas = length llegados
  in if perdidas == 0 then est
     else traceLog ("¡Game Over! Enemigos tocaron la base (" ++ show perdidas ++ ")")
        $ est { ejEnemigos = resto
              , ejVidaBase = max 0 (ejVidaBase - perdidas)
              , ejGameOver = True   -- GAME OVER inmediato
              }

-- ======= cooldown =======
actualizarCooldownTorres :: EstadoJuego -> EstadoJuego
actualizarCooldownTorres est@EstadoJuego{..} =
  let ts' = map (\t -> if cdActual t > 0 then t{cdActual = cdActual t - 1} else t) ejTorres
  in est { ejTorres = ts' }

-- ======= JSON =======
serializarEstadoJSON :: [Posicion] -> EstadoJuego -> Value
serializarEstadoJSON camino EstadoJuego{..} =
  let torresJson = map (\t ->
         let (tx,ty) = posTorre t
         in object
            [ "idTorre"    .= idTorre t
            , "posTorre"   .= [tx,ty]
            , "tipo"       .= show (tipoTorre t)
            , "nivel"      .= nivelTorre t
            , "danioBase"  .= danioBase t
            , "rango"      .= rangoTorre t
            , "cdActual"   .= cdActual t
            , "hpTorre"    .= hpTorre t
            , "hpMaxTorre" .= hpMaxTorre t
            ]) ejTorres
      enemigosJson = map (\e ->
         let (px,py) = posicionContinua camino e
         in object
            [ "idEnemigo"       .= idEnemigo e
            , "hpEnemigo"       .= hpEnemigo e
            , "hpMax"           .= hpMax e
            , "indicePosicion"  .= indicePosicion e
            , "progreso"        .= progreso e
            , "velocidad"       .= velocidadBase e
            , "posX"            .= px, "posY" .= py
            , "efecto"          .= case efecto e of
                                      SinEfecto -> ("none" :: String)
                                      Lento _ _ -> "slow"
            ]) ejEnemigos
  in object
     [ "ejTick"               .= ejTick
     , "ejCamino"             .= ejCamino
     , "ejEntrada"            .= ejEntrada
     , "ejBase"               .= ejBase
     , "ejTorres"             .= torresJson
     , "ejEnemigos"           .= enemigosJson
     , "ejSiguienteIdEnemigo" .= ejSiguienteIdEnemigo
     , "ejMonedas"            .= ejMonedas
     , "ejMaxTorres"          .= ejMaxTorres
     , "ejVidaBase"           .= ejVidaBase
     , "ejGameOver"           .= ejGameOver
     ]

-- ======= util =======
traceLog :: String -> a -> a
traceLog msg x = unsafeLog msg `seq` x

unsafeLog :: String -> ()
unsafeLog s = unsafePerformIO (hPutStrLn stderr ("[motor] " ++ s) >> pure ())
{-# NOINLINE unsafeLog #-}
