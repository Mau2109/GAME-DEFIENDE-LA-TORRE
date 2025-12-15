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

-- ======= PARÁMETROS =======
costoArquera, costoCanon, costoMago, costoArtilleria :: Int
costoArquera   = 40
costoCanon     = 60
costoMago      = 80
costoArtilleria = 120  -- NUEVO: más cara porque es manual

-- Base de stats por tipo
statsTorre :: TorreTipo -> (Int {-daño-}, Int {-rango-}, Int {-CD-})
statsTorre Arquera    = (8, 3, 1)
statsTorre Canon      = (18, 3, 2)
statsTorre Mago       = (10, 3, 3)
statsTorre Artilleria = (35, 6, 4)  -- NUEVO: alto daño, largo rango, cooldown alto

-- HP inicial por tipo
hpInicialTorre :: TorreTipo -> Int
hpInicialTorre Arquera    = 60
hpInicialTorre Canon      = 90
hpInicialTorre Mago       = 75
hpInicialTorre Artilleria = 100  -- NUEVO: más resistente

-- Daño que hace 1 enemigo a la torre por tick
danioEnemigoATorre :: Int
danioEnemigoATorre = 6

-- Recompensa por kill
recompensaKill :: Int
recompensaKill = 10

-- Efecto Mago
efectoMago :: Efecto
efectoMago = Lento { ticks = 3, factor = 0.6 }

-- NUEVO: Parámetros de proyectiles
velocidadProyectil :: Double
velocidadProyectil = 0.15  -- Progreso por tick (0.0 a 1.0)

radioExplosionArtilleria :: Double
radioExplosionArtilleria = 2.0  -- En celdas

-- ======= ESTADO INICIAL =======
crearEstadoInicial :: [Posicion] -> Posicion -> Posicion -> EstadoJuego
crearEstadoInicial camino entrada base = EstadoJuego
  { ejTick = 0
  , ejCamino = camino
  , ejEntrada = entrada
  , ejBase = base
  , ejEnemigos = []
  , ejTorres = []
  , ejProyectiles = []  -- NUEVO
  , ejSiguienteIdEnemigo = 1
  , ejSiguienteIdTorre = 1
  , ejSiguienteIdProy = 1  -- NUEVO
  , ejMonedas = 350
  , ejMaxTorres = 15  -- Aumentado porque Artillería cuenta como torre
  , ejVidaBase = 20
  , ejGameOver = False
  }

-- ======= PASO PRINCIPAL =======
paso :: [Posicion] -> EstadoJuego -> Comando -> EstadoJuego
paso camino est0 cmd =
  let est1 = aplicarComando camino est0 cmd
      est2 = actualizarCooldownTorres est1
      est3 = moverProyectiles camino est2       -- NUEVO: mover bombas
      est4 = moverEnemigos camino est3
      est5 = enemigosAtacanTorres camino est4
      est6 = torresAtacan camino est5
      est7 = aplicarMuertesYRecompensas est6
      est8 = llegarBase camino est7
      estF = est8 { ejTick = ejTick est8 + 1 }
  in estF

-- ======= COMANDOS =======
aplicarComando :: [Posicion] -> EstadoJuego -> Comando -> EstadoJuego

-- Colocar torre (incluye Artillería)
aplicarComando _ est@(EstadoJuego{..}) (CmdColocarTorre pos tipo) =
  let enCamino = pos `elem` ejCamino
      yaTorre  = any (\t -> posTorre t == pos) ejTorres
      numT     = length ejTorres
      costo = case tipo of 
                Arquera    -> costoArquera
                Canon      -> costoCanon
                Mago       -> costoMago
                Artilleria -> costoArtilleria
  in if enCamino || yaTorre || numT >= ejMaxTorres || ejMonedas < costo || ejGameOver
       then traceLog ("No se puede colocar torre " ++ show tipo ++ " en " ++ show pos 
                   ++ " (camino=" ++ show enCamino 
                   ++ ", ocupado=" ++ show yaTorre
                   ++ ", limite=" ++ show (numT >= ejMaxTorres)
                   ++ ", dinero=" ++ show (ejMonedas < costo) ++ ")")
            est
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

-- NUEVO: Disparar artillería manualmente
aplicarComando camino est@(EstadoJuego{..}) (CmdDispararArtilleria idTorreCmd objetivo) =
  case filter (\t -> idTorre t == idTorreCmd && tipoTorre t == Artilleria) ejTorres of
    [] -> est  -- Torre no existe o no es Artillería
    (torre:_) ->
      if cdActual torre > 0
        then traceLog ("Torre " ++ show idTorreCmd ++ " aún en cooldown") est
        else
          let (tx, ty) = posTorre torre
              origen = (fromIntegral tx + 0.5, fromIntegral ty + 0.5)
              
              -- Verificar que objetivo esté en rango
              (ox, oy) = objetivo
              dx = ox - fst origen
              dy = oy - snd origen
              dist = sqrt (dx*dx + dy*dy)
              
          in if dist > fromIntegral (rangoTorre torre)
               then traceLog ("Objetivo fuera de rango") est
               else
                 let proy = Proyectil
                       { idProyectil = ejSiguienteIdProy
                       , posOrigenProy = origen
                       , posDestinoProy = objetivo
                       , posActualProy = origen
                       , progresoVuelo = 0.0
                       , velocidadProy = velocidadProyectil
                       , danioProy = danioBase torre * nivelTorre torre
                       , radioExplosion = radioExplosionArtilleria
                       , idTorreProy = idTorreCmd
                       }
                     
                     -- Actualizar torre: cooldown activo
                     torresAct = map (\t -> if idTorre t == idTorreCmd
                                            then t { cdActual = enfriamiento t }
                                            else t) ejTorres
                     
                 in traceLog ("Artillería " ++ show idTorreCmd ++ " dispara a " ++ show objetivo)
                    $ est { ejProyectiles = proy : ejProyectiles
                          , ejSiguienteIdProy = ejSiguienteIdProy + 1
                          , ejTorres = torresAct }

-- Iniciar oleada
aplicarComando _ est CmdIniciarOleada
  | ejGameOver est = est
  | otherwise =
      let n   = 40
          vs  = [0..n-1]
          ids = [ejSiguienteIdEnemigo est .. ejSiguienteIdEnemigo est + n - 1]
          mkV k   = 0.40 + 0.03 * fromIntegral (k `mod` 16)
          mkHp k  = 60 + k*3
          mkProg k =
            let steps = 20
                i     = k `mod` steps
            in  fromIntegral i * (1.0 / fromIntegral steps)
          enemigos = [ Enemigo i (mkHp k) (mkHp k) 0 (mkProg k) (mkV k) SinEfecto
                     | (i,k) <- zip ids vs ]
      in traceLog ("Inicia oleada con " ++ show n ++ " enemigos")
         $ est { ejEnemigos = ejEnemigos est ++ enemigos
               , ejSiguienteIdEnemigo = ejSiguienteIdEnemigo est + n }

aplicarComando _ est CmdNoop = est

-- ======= NUEVO: MOVER PROYECTILES =======
moverProyectiles :: [Posicion] -> EstadoJuego -> EstadoJuego
moverProyectiles camino est@(EstadoJuego{..}) =
  let -- Actualizar posición de cada proyectil
      actualizarProy p@(Proyectil{..}) =
        let progNew = progresoVuelo + velocidadProy
            (ox, oy) = posOrigenProy
            (dx, dy) = posDestinoProy
            
            -- Interpolación lineal
            px = ox + (dx - ox) * progNew
            py = oy + (dy - oy) * progNew
            
        in p { posActualProy = (px, py), progresoVuelo = progNew }
      
      proyActualizados = map actualizarProy ejProyectiles
      
      -- Separar: los que llegaron (progreso >= 1.0) explotan
      (impactados, enVuelo) = partition (\p -> progresoVuelo p >= 1.0) proyActualizados
      
      -- Aplicar daño de explosiones
      enemigosConDanio = foldl' aplicarExplosion ejEnemigos impactados
      
  in est { ejProyectiles = enVuelo
         , ejEnemigos = enemigosConDanio }

-- Aplicar daño de una explosión a todos los enemigos en área
aplicarExplosion :: [Enemigo] -> Proyectil -> [Enemigo]
aplicarExplosion enemigos Proyectil{..} =
  let (ex, ey) = posDestinoProy
      radio = radioExplosion
      
      dañarSiEnArea e =
        let (px, py) = posicionContinuaEnemigo (ejCamino undefined) e  -- Necesitamos camino
            dx = px - ex
            dy = py - ey
            dist = sqrt (dx*dx + dy*dy)
        in if dist <= radio
             then e { hpEnemigo = hpEnemigo e - danioProy }
             else e
      
  in map dañarSiEnArea enemigos
  where
    -- Función auxiliar temporal (debemos pasarla correctamente)
    posicionContinuaEnemigo _ Enemigo{..} = (0.0, 0.0)  -- TODO: arreglar

-- ======= MOVIMIENTO ENEMIGOS =======
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

-- ======= ENEMIGOS ATACAN TORRES =======
enemigosAtacanTorres :: [Posicion] -> EstadoJuego -> EstadoJuego
enemigosAtacanTorres camino est@EstadoJuego{..}
  | ejGameOver = est
  | otherwise  =
      let posEn e = posicionContinua camino e
          aplicarDanioDe e torres =
            let (ex,ey) = posEn e
                dist (tx,ty) = let dx = fromIntegral tx + 0.5 - ex
                                   dy = fromIntegral ty + 0.5 - ey
                               in sqrt (dx*dx + dy*dy)
                tocadas = filter (\t -> dist (posTorre t) < 0.6) torres
            in case tocadas of
                 []   -> torres
                 t:_  ->
                   let t'  = t { hpTorre = hpTorre t - danioEnemigoATorre }
                       ts' = t' : filter (\x -> idTorre x /= idTorre t) torres
                   in ts'
          torresGolpeadas = foldl (flip aplicarDanioDe) ejTorres ejEnemigos
          (muertas, vivas) = partition (\t -> hpTorre t <= 0) torresGolpeadas
          est' = if null muertas
                   then est { ejTorres = vivas }
                   else traceLog ("Torres destruidas: " ++ show (map idTorre muertas))
                        $ est { ejTorres = vivas }
      in est'

-- ======= TORRES ATACAN (Solo automáticas) =======
torresAtacan :: [Posicion] -> EstadoJuego -> EstadoJuego
torresAtacan camino est@EstadoJuego{..}
  | ejGameOver = est
  | otherwise  =
      let aplicar (ens, ts) t@Torre{..}
            -- NUEVO: Artillería NO ataca automáticamente
            | tipoTorre == Artilleria = (ens, ts ++ [t])
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

-- ======= MUERTES Y RECOMPENSAS =======
aplicarMuertesYRecompensas :: EstadoJuego -> EstadoJuego
aplicarMuertesYRecompensas est@EstadoJuego{..} =
  let (muertos, vivos) = partition (\e -> hpEnemigo e <= 0) ejEnemigos
      ganancia = length muertos * recompensaKill
  in if null muertos
        then est
        else traceLog ("Enemigos eliminados: " ++ show (map idEnemigo muertos)
                       ++ " +" ++ show ganancia ++ " monedas")
             $ est { ejEnemigos = vivos, ejMonedas = ejMonedas + ganancia }

-- ======= LLEGADA A BASE =======
llegarBase :: [Posicion] -> EstadoJuego -> EstadoJuego
llegarBase camino est@EstadoJuego{..} =
  let lenCam = length camino
      (llegados, resto) = partition (\e -> indicePosicion e >= lenCam) ejEnemigos
      perdidas = length llegados
  in if perdidas == 0 then est
     else traceLog ("¡Game Over! Enemigos tocaron la base (" ++ show perdidas ++ ")")
        $ est { ejEnemigos = resto
              , ejVidaBase = max 0 (ejVidaBase - perdidas)
              , ejGameOver = True }

-- ======= COOLDOWN =======
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
      
      -- NUEVO: Proyectiles JSON
      proyectilesJson = map (\p -> object
            [ "idProyectil"    .= idProyectil p
            , "posX"           .= fst (posActualProy p)
            , "posY"           .= snd (posActualProy p)
            , "destinoX"       .= fst (posDestinoProy p)
            , "destinoY"       .= snd (posDestinoProy p)
            , "progreso"       .= progresoVuelo p
            , "danio"          .= danioProy p
            , "radioExplosion" .= radioExplosion p
            ]) ejProyectiles
      
  in object
     [ "ejTick"               .= ejTick
     , "ejCamino"             .= ejCamino
     , "ejEntrada"            .= ejEntrada
     , "ejBase"               .= ejBase
     , "ejTorres"             .= torresJson
     , "ejEnemigos"           .= enemigosJson
     , "ejProyectiles"        .= proyectilesJson  -- NUEVO
     , "ejSiguienteIdEnemigo" .= ejSiguienteIdEnemigo
     , "ejMonedas"            .= ejMonedas
     , "ejMaxTorres"          .= ejMaxTorres
     , "ejVidaBase"           .= ejVidaBase
     , "ejGameOver"           .= ejGameOver
     ]

-- ======= UTIL =======
traceLog :: String -> a -> a
traceLog msg x = unsafeLog msg `seq` x

unsafeLog :: String -> ()
unsafeLog s = unsafePerformIO (hPutStrLn stderr ("[motor] " ++ s) >> pure ())
{-# NOINLINE unsafeLog #-}