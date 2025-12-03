#!/usr/bin/env python3
# visualizador_pygame_avanzado.py
# Visualizador avanzado para "Defiende la torre"
# Requisitos: pygame, Python 3.8+
#
# Comunicación: lanza el motor Haskell con `stack exec defiende --`
# Envía comandos JSON por stdin (una línea por comando) y lee estados JSON por stdout (una línea por estado).

import subprocess
import json
import threading
import queue
import pygame
import sys
import time
import os
from typing import Dict, List, Any, Optional, Tuple

# ---------------- CONFIGURACIÓN ----------------
COMANDO_MOTOR = ["stack", "exec", "defiende", "--"]
# Si prefieres usar binario directo, sustituye COMANDO_MOTOR por:
# COMANDO_MOTOR = ["./.stack-work/install/.../9.2.7/bin/defiende"]

ANCHO_VENTANA = 1000
ALTO_VENTANA = 560
TAMANIO_CELDA = 24         # píxeles por celda (ajusta si quieres)
FPS = 60
ARCHIVO_REPLAY = "replay.jsonl"

# intervalo por defecto en ticks por segundo que enviará "noop" si no está en pausa
TICKS_POR_SEGUNDO_INICIAL = 3.0

# ---------------- ESTADO GLOBAL ----------------
proc = None  # proceso subproceso
cola_estados = queue.Queue()
cola_errores = queue.Queue()
lock_estado = threading.Lock()

# Estados para interpolación
ultimo_estado: Optional[Dict[str, Any]] = None
estado_objetivo: Optional[Dict[str, Any]] = None
tiempo_estado_objetivo = 0.0
tiempo_estado_anterior = 0.0

# Control del tick loop
pausado = False
ticks_por_segundo = TICKS_POR_SEGUNDO_INICIAL
intervalo_tick = 1.0 / ticks_por_segundo

# Flags / UI
guardar_automatico_replay = True

# ---------------- UTILIDADES ----------------
def ahora() -> float:
    return time.time()

def enviar_comando(obj: Dict[str, Any]):
    """Envía un comando JSON al motor Haskell (una línea por comando)."""
    global proc
    if proc is None or proc.poll() is not None:
        print("[visualizador] Error: proceso del motor no disponible.")
        return
    linea = json.dumps(obj, separators=(",",":")) + "\n"
    try:
        proc.stdin.write(linea)
        proc.stdin.flush()
    except BrokenPipeError:
        print("[visualizador] BrokenPipe: no se puede escribir en stdin del motor.")

def guardar_linea_replay(linea_json: str):
    """Añade una línea JSON al archivo de replay."""
    try:
        with open(ARCHIVO_REPLAY, "a", encoding="utf-8") as f:
            f.write(linea_json.rstrip("\n") + "\n")
    except Exception as e:
        print("[visualizador] Error guardando replay:", e)

# ---------------- HILOS PARA I/O ----------------
def hilo_lector_stdout(p_stdout, cola):
    """Lee stdout del proceso (línea por línea) y pone en cola los estados (como dict)."""
    while True:
        linea = p_stdout.readline()
        if linea == "" and proc.poll() is not None:
            # proceso terminó
            break
        if not linea:
            time.sleep(0.01)
            continue
        linea = linea.strip()
        if not linea:
            continue
        try:
            estado = json.loads(linea)
            cola.put((True, estado, linea))
        except Exception as e:
            # si no es JSON, lo mandamos a errores
            cola.put((False, linea, str(e)))
    # signal end
    cola.put((None, None, None))

def hilo_lector_stderr(p_stderr, cola_err):
    """Lee stderr del proceso y pone líneas en cola de errores."""
    while True:
        linea = p_stderr.readline()
        if linea == "" and proc.poll() is not None:
            break
        if not linea:
            time.sleep(0.01)
            continue
        cola_err.put(linea.rstrip("\n"))
    cola_err.put(None)

# ---------------- LOGICA DE TICKS (envío periódico de noop) ----------------
def hilo_tick_sender():
    """Envía comandos noop periódicos cuando no estamos en pausa."""
    global intervalo_tick, pausado
    while True:
        if proc is None or proc.poll() is not None:
            break
        if not pausado:
            enviar_comando({"cmd":"noop"})
        time.sleep(intervalo_tick)

# ---------------- POSICIONES E INTERPOLACIÓN ----------------
def pos_pixels_desde_camino(camino: List[List[int]], posicion_f: Tuple[float,float]) -> Tuple[int,int]:
    """Convierte coordenadas en celdas flotantes (x,y) a pixels en pantalla."""
    x, y = posicion_f
    cx = int(x * TAMANIO_CELDA + TAMANIO_CELDA//2)
    cy = int(y * TAMANIO_CELDA + TAMANIO_CELDA//2)
    return cx, cy

def calcular_posicion_interpolada(en_anterior: Dict[str, Any], en_objetivo: Dict[str, Any], progreso: float,
                                  camino_anterior: List[List[int]], camino_objetivo: List[List[int]]) -> Tuple[int,int]:
    """
    Calcula posición en pixels interpolando entre anterior y objetivo.
    - Si el motor envía 'posX'/'posY' floats se usan directamente.
    - Si solo hay 'indicePosicion', aproximamos interpolando entre camino[idx] y camino[idx+1].
    """
    # si el enemigo trae posX/posY en alguno de los estados, darle prioridad
    if en_anterior is not None and "posX" in en_anterior and "posY" in en_anterior and \
       en_objetivo is not None and "posX" in en_objetivo and "posY" in en_objetivo:
        x0 = float(en_anterior["posX"]); y0 = float(en_anterior["posY"])
        x1 = float(en_objetivo["posX"]); y1 = float(en_objetivo["posY"])
        xf = x0 + (x1 - x0) * progreso
        yf = y0 + (y1 - y0) * progreso
        return pos_pixels_desde_camino(camino_objetivo or camino_anterior, (xf, yf))
    # fallback a indices
    idx0 = int(en_anterior.get("indicePosicion", 0)) if en_anterior else int(en_objetivo.get("indicePosicion",0))
    idx1 = int(en_objetivo.get("indicePosicion", 0)) if en_objetivo else idx0
    camino = camino_objetivo if camino_objetivo else camino_anterior
    if not camino:
        return (TAMANIO_CELDA//2, TAMANIO_CELDA//2)
    # clamp indices
    idx0 = max(0, min(idx0, len(camino)-1))
    idx1 = max(0, min(idx1, len(camino)-1))
    x0,y0 = camino[idx0]
    x1,y1 = camino[idx1]
    fx = x0 + (x1 - x0) * progreso
    fy = y0 + (y1 - y0) * progreso
    return pos_pixels_desde_camino(camino, (fx, fy))

# ---------------- DIBUJO ----------------
def dibujar_interfaz(pantalla, fuente_peq, estado_actual, caminos_cache, ancho_panel=260):
    # panel lateral derecho
    rect_panel = pygame.Rect(ANCHO_VENTANA - ancho_panel, 0, ancho_panel, ALTO_VENTANA)
    pygame.draw.rect(pantalla, (18,18,18), rect_panel)
    pygame.draw.rect(pantalla, (40,40,40), rect_panel, 2)

    y = 12
    def dibujar_texto(txt, tamaño=18, color=(220,220,220)):
        nonlocal y
        surf = pygame.font.SysFont(None, tamaño).render(str(txt), True, color)
        pantalla.blit(surf, (ANCHO_VENTANA - ancho_panel + 12, y))
        y += surf.get_height() + 8

    # stats básicas
    if estado_actual:
        tick = estado_actual.get("ejTick", "?")
        torres = len(estado_actual.get("ejTorres", []))
        enemigos = len(estado_actual.get("ejEnemigos", []))
        prox = estado_actual.get("ejSiguienteIdEnemigo", "?")
        dibujar_texto(f"Tick: {tick}")
        dibujar_texto(f"Enemigos vivos: {enemigos}")
        dibujar_texto(f"Torres: {torres}")
        dibujar_texto(f"SiguienteID: {prox}")
        dibujar_texto("")
    else:
        dibujar_texto("Esperando estado...", tamaño=20)

    dibujar_texto("Controles:", tamaño=16, color=(200,200,120))
    dibujar_texto("Click izquierdo: previsualizar/colocar torre")
    dibujar_texto("Space: iniciar oleada")
    dibujar_texto("P: pausar/reanudar")
    dibujar_texto("S: paso (1 tick)")
    dibujar_texto("+: + velocidad ticks")
    dibujar_texto("-: - velocidad ticks")
    dibujar_texto("G: guardar replay")

    # errores recientes
    errores = []
    while not cola_errores.empty():
        e = cola_errores.get_nowait()
        if e is None: break
        errores.append(e)
    if errores:
        dibujar_texto("")
        dibujar_texto("Errores (stderr):", tamaño=14, color=(220,100,100))
        for err in errores[-4:]:
            dibujar_texto(err if len(err) < 36 else err[:33] + "...", tamaño=14, color=(255,160,160))

def dibujar_mapa_basico(pantalla):
    pantalla.fill((22, 22, 25))
    # grilla ligera
    for x in range(0, ANCHO_VENTANA, TAMANIO_CELDA):
        pygame.draw.line(pantalla, (28,28,28), (x,0), (x,ALTO_VENTANA))
    for y in range(0, ALTO_VENTANA, TAMANIO_CELDA):
        pygame.draw.line(pantalla, (28,28,28), (0,y), (ANCHO_VENTANA - 260, y))

def dibujar_camino(pantalla, camino):
    if not camino:
        return
    puntos = [ (x*TAMANIO_CELDA + TAMANIO_CELDA//2, y*TAMANIO_CELDA + TAMANIO_CELDA//2) for (x,y) in camino ]
    if len(puntos) >= 2:
        pygame.draw.lines(pantalla, (200,160,60), False, puntos, max(6, TAMANIO_CELDA//2))

def dibujar_torres(pantalla, torres):
    for t in torres:
        pos = t.get("posTorre") or t.get("pos") or t.get("posTorreXY") or t.get("posTorre", (0,0))
        if isinstance(pos, list) or isinstance(pos, tuple):
            x,y = pos
        else:
            x,y = 0,0
        cx = x*TAMANIO_CELDA + TAMANIO_CELDA//2
        cy = y*TAMANIO_CELDA + TAMANIO_CELDA//2
        pygame.draw.circle(pantalla, (60,180,60), (cx,cy), max(6, TAMANIO_CELDA//2))
        rango = int(t.get("rango", 3))
        surf = pygame.Surface((ANCHO_VENTANA, ALTO_VENTANA), pygame.SRCALPHA)
        pygame.draw.circle(surf, (60,180,60,30), (cx,cy), rango*TAMANIO_CELDA)
        pantalla.blit(surf, (0,0))

def dibujar_enemigos_interpolados(pantalla, enemigos_anim, camino):
    for e in enemigos_anim:
        cx,cy = e["pos"]
        hp = e.get("hpEnemigo", e.get("hp", 1))
        hpmax = e.get("hpMax", e.get("hpMax", hp))
        radio = max(4, TAMANIO_CELDA//2 - 2)
        pygame.draw.circle(pantalla, (200,60,60), (cx,cy), radio)
        # barra HP encima
        barras_ancho = TAMANIO_CELDA - 6
        porcentaje = max(0.0, min(1.0, float(hp)/float(max(1,hpmax))))
        pygame.draw.rect(pantalla, (10,10,10), (cx - barras_ancho//2, cy - radio - 8, barras_ancho, 5))
        pygame.draw.rect(pantalla, (40,200,120), (cx - barras_ancho//2, cy - radio - 8, int(barras_ancho*porcentaje), 5))

# ---------------- LOGICA PRINCIPAL ----------------
def main():
    global proc, ultimo_estado, estado_objetivo, tiempo_estado_objetivo, tiempo_estado_anterior
    global pausado, ticks_por_segundo, intervalo_tick, guardar_automatico_replay

    # lanzar proceso Haskell
    proc = subprocess.Popen(
        COMANDO_MOTOR,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1
    )

    # iniciar hilos lectores
    hilo_salida = threading.Thread(target=hilo_lector_stdout, args=(proc.stdout, cola_estados), daemon=True)
    hilo_err = threading.Thread(target=hilo_lector_stderr, args=(proc.stderr, cola_errores), daemon=True)
    hilo_salida.start()
    hilo_err.start()

    # iniciar hilo de ticks periódicos
    hilo_ticks = threading.Thread(target=hilo_tick_sender, daemon=True)
    hilo_ticks.start()

    # inicar pygame
    pygame.init()
    pantalla = pygame.display.set_mode((ANCHO_VENTANA, ALTO_VENTANA))
    pygame.display.set_caption("Defiende la torre — Visualizador avanzado")
    reloj = pygame.time.Clock()

    # variables UI / colocación
    mostrar_previsualizacion = False
    previsualizacion_pos = (0,0)
    ultimo_click_t = 0.0

    # para interpolación: lista de enemigos animados (cada frame se recalc)
    enemigos_animados = []

    # si hay replay previo, renombrarlo para no sobrescribir accidentalmente
    if not os.path.exists(ARCHIVO_REPLAY):
        with open(ARCHIVO_REPLAY, "w", encoding="utf-8") as f:
            f.write("")  # crear vacío

    # loop principal
    corriendo = True
    while corriendo:
        # --- Procesar eventos pygame ---
        for evento in pygame.event.get():
            if evento.type == pygame.QUIT:
                corriendo = False
            elif evento.type == pygame.MOUSEMOTION:
                mx,my = evento.pos
                gx = mx // TAMANIO_CELDA
                gy = my // TAMANIO_CELDA
                mostrar_previsualizacion = True
                previsualizacion_pos = (gx,gy)
            elif evento.type == pygame.MOUSEBUTTONDOWN and evento.button == 1:
                # click izquierdo -> colocar torre (envía comando)
                mx,my = evento.pos
                gx = mx // TAMANIO_CELDA
                gy = my // TAMANIO_CELDA
                enviar_comando({"cmd":"colocar_torre","pos":[gx,gy]})
                ultimo_click_t = ahora()
            elif evento.type == pygame.KEYDOWN:
                if evento.key == pygame.K_SPACE:
                    enviar_comando({"cmd":"iniciar_oleada"})
                elif evento.key == pygame.K_p:
                    pausado = not pausado
                elif evento.key == pygame.K_s:
                    # paso a paso: enviar un único noop
                    enviar_comando({"cmd":"noop"})
                elif evento.key == pygame.K_g:
                    # guardar replay (crea copia timestamp)
                    timestamp = int(time.time())
                    nombre = f"replay-{timestamp}.jsonl"
                    try:
                        with open(ARCHIVO_REPLAY, "r", encoding="utf-8") as src:
                            contenido = src.read()
                        with open(nombre, "w", encoding="utf-8") as dst:
                            dst.write(contenido)
                        print(f"[visualizador] Replay guardado en {nombre}")
                    except Exception as e:
                        print("[visualizador] Error guardando replay:", e)
                elif evento.key == pygame.K_PLUS or evento.key == pygame.K_EQUALS:
                    ticks_por_segundo = min(15.0, ticks_por_segundo + 0.5)
                    intervalo_tick = 1.0 / ticks_por_segundo
                elif evento.key == pygame.K_MINUS or evento.key == pygame.K_UNDERSCORE:
                    ticks_por_segundo = max(0.5, ticks_por_segundo - 0.5)
                    intervalo_tick = 1.0 / ticks_por_segundo

        # --- Leer estados nuevos (si hay) ---
        # Puede venir un estado válido o una línea de error
        actualizado = False
        while not cola_estados.empty():
            item = cola_estados.get()
            if item is None:
                continue
            valido, dato, raw = item
            if valido is None:
                # fin del lector
                break
            if valido is False:
                # linea de stderr malformada o json inválido
                cola_errores.put(f"STDOUT NO JSON: {dato} -- {raw}")
                continue
            # dato es el estado (dict), raw es la línea original
            nuevo_estado = dato
            tnow = ahora()
            with lock_estado:
                # rotar estados para interpolación
                ultimo_estado = estado_objetivo
                tiempo_estado_anterior = tiempo_estado_objetivo
                estado_objetivo = nuevo_estado
                tiempo_estado_objetivo = tnow
            # guardar linea en replay
            if guardar_automatico_replay:
                guardar_linea_replay(raw)
            actualizado = True

        # --- Interpolación entre ultimo_estado y estado_objetivo ---
        progreso = 1.0
        with lock_estado:
            if ultimo_estado is None and estado_objetivo is not None:
                # sin anterior -> pintar objetivo directamente
                progreso = 1.0
            elif ultimo_estado is not None and estado_objetivo is not None:
                duracion = max(1e-6, tiempo_estado_objetivo - tiempo_estado_anterior)
                progreso = (ahora() - tiempo_estado_anterior) / duracion if duracion > 0 else 1.0
                progreso = max(0.0, min(1.0, progreso))
            # preparar datos para dibujar
            camino_obj = estado_objetivo.get("ejCamino", []) if estado_objetivo else []
            camino_ant = ultimo_estado.get("ejCamino", []) if ultimo_estado else camino_obj
            enemigos_act = estado_objetivo.get("ejEnemigos", []) if estado_objetivo else []
            enemigos_ant = ultimo_estado.get("ejEnemigos", []) if ultimo_estado else []

        # construir lista de enemigos interpolados
        enemigos_animados = []
        # indexarlos por id para buscar correspondencia
        def index_por_id(lista):
            d = {}
            for item in lista:
                # posibles nombres de ids: idEnemigo / id
                idv = item.get("idEnemigo") or item.get("id") or item.get("id_enemigo")
                if idv is None:
                    # generamos uno basado en posición y hp (fallback)
                    idv = f"anon-{lista.index(item)}"
                d[idv] = item
            return d

        idx_ant = index_por_id(enemigos_ant)
        idx_obj = index_por_id(enemigos_act)

        # iterar por los enemigos objetivo (los visibles ahora)
        for idv, e_obj in idx_obj.items():
            e_ant = idx_ant.get(idv)
            px,py = calcular_posicion_interpolada(e_ant, e_obj, progreso, camino_ant, camino_obj)
            e_anim = {
                "id": idv,
                "pos": (px,py),
                "hpEnemigo": e_obj.get("hpEnemigo", e_obj.get("hp", 1)),
                "hpMax": e_obj.get("hpMax", e_obj.get("hpMax", e_obj.get("hpEnemigo", 1)))
            }
            enemigos_animados.append(e_anim)

        # --- DIBUJAR ESCENA ---
        dibujar_mapa_basico(pantalla)
        # intentar dibujar camino (usamos estado_objetivo si existe)
        camino_para_dibujar = (estado_objetivo.get("ejCamino") if estado_objetivo else None) or []
        dibujar_camino(pantalla, camino_para_dibujar)
        # torres (del estado objetivo)
        torres_para_dibujar = estado_objetivo.get("ejTorres", []) if estado_objetivo else []
        dibujar_torres(pantalla, torres_para_dibujar)
        # enemigos interpolados
        dibujar_enemigos_interpolados(pantalla, enemigos_animados, camino_para_dibujar)
        # UI lateral
        dibujar_interfaz(pantalla, None, estado_objetivo, None)

        # previsualización de colocación
        if mostrar_previsualizacion:
            gx,gy = previsualizacion_pos
            rect = pygame.Rect(gx * TAMANIO_CELDA, gy * TAMANIO_CELDA, TAMANIO_CELDA, TAMANIO_CELDA)
            s = pygame.Surface((TAMANIO_CELDA, TAMANIO_CELDA), pygame.SRCALPHA)
            s.fill((50,200,50,60))
            pantalla.blit(s, rect.topleft)
            # dibujar rango aproximado (ejemplo: 3)
            rango_est = 3
            cx = gx*TAMANIO_CELDA + TAMANIO_CELDA//2
            cy = gy*TAMANIO_CELDA + TAMANIO_CELDA//2
            surf_r = pygame.Surface((ANCHO_VENTANA, ALTO_VENTANA), pygame.SRCALPHA)
            pygame.draw.circle(surf_r, (50,200,50,30), (cx,cy), rango_est*TAMANIO_CELDA)
            pantalla.blit(surf_r, (0,0))

        pygame.display.flip()
        reloj.tick(FPS)

        # Si proceso terminó, salimos
        if proc.poll() is not None:
            print("[visualizador] Proceso Haskell finalizó. Saliendo.")
            corriendo = False

    # cleanup
    try:
        proc.stdin.close()
    except:
        pass
    try:
        proc.terminate()
        proc.wait(timeout=2)
    except:
        pass
    pygame.quit()
    sys.exit(0)

if __name__ == "__main__":
    main()
