#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import sys, json, time, math, threading, queue, subprocess
import pygame

# ================== Config ==================
GRID_W, GRID_H = 28, 18
CELL = 40
PANEL_W = 380
FPS = 60

COLOR_BG = (15, 16, 20)
COLOR_GRID = (40, 44, 52)
COLOR_PATH = (92, 214, 120)
COLOR_ENT = (80, 180, 255)
COLOR_BASE= (255, 80, 120)
COLOR_TOWER = {
    "Arquera": (240, 210, 90),
    "Canon":   (200, 100, 60),
    "Mago":    (140, 140, 255)
}
COLOR_PANEL = (22, 24, 30)
COLOR_CARD  = (28, 30, 38)
COLOR_ACCENT= (255, 210, 90)
COLOR_PLACE_DOT = (75, 95, 125)
COLOR_OK = (90, 220, 140)
COLOR_BAD = (240, 90, 90)

# Radios "en celdas" para detección (cliente)
R_ENEMY = ((CELL//3) / float(CELL))
R_BASE  = 0.50

# Oleadas automáticas SIEMPRE
WAVE_INTERVAL = 5.0  # segundos

# Costos para mostrar en HUD (coinciden con motor)
COSTS = {"arquera":40, "canon":60, "mago":80}

# ================== Proceso motor ==================
class Motor:
    def __init__(self):
        self.proc = subprocess.Popen(
            ["stack","exec","defiende","--"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1
        )
        self.outq = queue.Queue()
        self.errq = queue.Queue()
        self._alive = True
        threading.Thread(target=self._reader, daemon=True).start()
        threading.Thread(target=self._err_reader, daemon=True).start()

    def _reader(self):
        for line in self.proc.stdout:
            self.outq.put(line.strip())
        self._alive = False

    def _err_reader(self):
        for line in self.proc.stderr:
            self.errq.put(line.rstrip())

    def send(self, obj):
        try:
            if self.proc.stdin:
                s = json.dumps(obj) + "\n"
                self.proc.stdin.write(s)
                self.proc.stdin.flush()
        except Exception as e:
            print("[viz] ERROR al escribir al motor:", e)

    def poll(self):
        return self.proc.poll()

    def stop(self):
        try:
            if self.proc and self.poll() is None:
                self.proc.terminate()
                try:
                    self.proc.wait(timeout=0.5)
                except Exception:
                    self.proc.kill()
        except Exception:
            pass
        self._alive = False

# ================== HUD / logs ==================
class LogPanel:
    def __init__(self, font):
        self.font = font
        self.lines = []

    def push(self, msg):
        print("[viz]", msg)
        self.lines.append(msg)
        if len(self.lines) > 15:
            self.lines.pop(0)

    def draw(self, surf, x, y, w, h):
        pygame.draw.rect(surf, COLOR_CARD, (x,y,w,h), border_radius=12)
        py = y + 10
        title = self.font.render("Mensajes", True, (230,230,230))
        surf.blit(title, (x+12, py)); py += 28
        for ln in self.lines[-10:]:
            txt = self.font.render("• " + ln, True, (200,200,200))
            surf.blit(txt, (x+12, py)); py += 22

# ================== Util UI ==================
def _draw_shadow_ellipse(surf, cx, cy, w, h, alpha=90):
    shadow = pygame.Surface((w, h), pygame.SRCALPHA)
    pygame.draw.ellipse(shadow, (0, 0, 0, alpha), shadow.get_rect())
    surf.blit(shadow, (cx - w//2, cy - h//2 + 6))

def _badge(surf, text, x, y, font, bg, fg=(20,20,20), padx=8, pady=4):
    t = font.render(text, True, fg)
    rect = t.get_rect()
    pill = pygame.Rect(x, y, rect.w + 2*padx, rect.h + 2*pady)
    pygame.draw.rect(surf, bg, pill, border_radius=999)
    surf.blit(t, (x+padx, y+pady))
    return pill

def _section_card(surf, x, y, w, title, font, big=False):
    pygame.draw.rect(surf, COLOR_CARD, (x, y, w, 9999), border_radius=14)
    head = pygame.Rect(x, y, w, 40 if big else 34)
    pygame.draw.rect(surf, (32,34,44), head, border_radius=14)
    lab_font = pygame.font.SysFont("DejaVu Sans", 20, bold=True) if big else font
    lab = lab_font.render(title, True, (235,235,235))
    surf.blit(lab, (x+12, y+ (8 if not big else 10)))
    return y + head.h + 8

def _draw_bar(surf, x, y, w, h, frac, bg=(60,60,60), fg=(80,220,100), br=5):
    pygame.draw.rect(surf, bg, (x, y, w, h), border_radius=br)
    pygame.draw.rect(surf, fg, (x, y, max(0, int(w*max(0.0, min(1.0, frac)))), h), border_radius=br)

def _draw_ring_progress(surf, cx, cy, radius_px, frac, width_px=6,
                        bg=(60,60,60), fg=(80,220,100)):
    rect = pygame.Rect(cx-radius_px, cy-radius_px, 2*radius_px, 2*radius_px)
    pygame.draw.circle(surf, bg, (cx, cy), radius_px, width_px)
    frac = max(0.0, min(1.0, float(frac)))
    if frac > 0.001:
        start_angle = -math.pi/2
        end_angle = start_angle + 2*math.pi*frac
        pygame.draw.arc(surf, fg, rect, start_angle, end_angle, width_px)

# ================== Render mapa/base ==================
def draw_grid(surf, offx, offy):
    for x in range(GRID_W):
        for y in range(GRID_H):
            rect = pygame.Rect(offx+x*CELL, offy+y*CELL, CELL, CELL)
            pygame.draw.rect(surf, COLOR_GRID, rect, 1)

def draw_path_and_fort(surf, camino, entrada, base, offx, offy):
    for (x,y) in camino:
        rect = pygame.Rect(offx+x*CELL, offy+y*CELL, CELL, CELL)
        pygame.draw.rect(surf, COLOR_PATH, rect)

    ex,ey = entrada
    ent_rect = pygame.Rect(offx+ex*CELL, offy+ey*CELL, CELL, CELL)
    pygame.draw.rect(surf, COLOR_ENT, ent_rect, border_radius=6)
    pygame.draw.rect(surf, (255,255,255), ent_rect, 2, border_radius=6)

    bx,by = base
    bx0, by0 = offx+bx*CELL, offy+by*CELL
    fort = pygame.Rect(bx0, by0, CELL, CELL)
    pygame.draw.rect(surf, (230,70,100), fort, border_radius=8)
    pygame.draw.rect(surf, (255,220,220), fort, 2, border_radius=8)
    top = pygame.Rect(bx0+4, by0+2, CELL-8, 8)
    pygame.draw.rect(surf, (255,120,140), top, border_radius=3)
    door = pygame.Rect(bx0+CELL//3, by0+CELL//2, CELL//3, CELL//2-4)
    pygame.draw.rect(surf, (90,30,40), door, border_radius=3)
    mast_x = bx0 + CELL - 6
    pygame.draw.line(surf, (240,240,240), (mast_x, by0+4), (mast_x, by0+CELL-6), 2)
    tri = [(mast_x, by0+6), (mast_x-14, by0+12), (mast_x, by0+18)]
    pygame.draw.polygon(surf, (255,210,60), tri)

# ================== Torres ==================
def tower_shape(surf, kind, cx, cy):
    r = CELL//3
    base_color = COLOR_TOWER.get(kind, (200,200,200))
    _draw_shadow_ellipse(surf, cx, cy, r*2, r, alpha=70)
    pygame.draw.circle(surf, base_color, (cx,cy), r)
    pygame.draw.circle(surf, (30,30,36), (cx,cy), r, 2)

    if kind == "Arquera":
        roof = [(cx, cy-r-4), (cx-r+6, cy-6), (cx+r-6, cy-6)]
        pygame.draw.polygon(surf, (255,235,140), roof)
        pygame.draw.rect(surf, (40,40,40), (cx-6, cy-2, 12, 5), border_radius=2)
    elif kind == "Canon":
        pygame.draw.rect(surf, (90,50,40), (cx-r+6, cy-8, 2*r-12, 16), border_radius=6)
        pygame.draw.circle(surf, (40,40,40), (cx, cy), r//2)
        pygame.draw.rect(surf, (60,60,60), (cx+6, cy-4, r+6, 8), border_radius=4)
    else:  # "Mago"
        poly = [(cx,cy-r-2),(cx-r+6,cy-2),(cx,cy+r+2),(cx+r-6,cy-2)]
        pygame.draw.polygon(surf, (160,160,255), poly)
        pygame.draw.circle(surf, (150,150,255), (cx,cy), r+6, 2)

def draw_towers(surf, torres, offx, offy):
    for t in torres:
        tx,ty = t["posTorre"]
        kind = t.get("tipo","Arquera")
        cx = offx + tx*CELL + CELL//2
        cy = offy + ty*CELL + CELL//2

        tower_shape(surf, kind, cx, cy)

        r = int(t.get("rango", 3)) * CELL
        pygame.draw.circle(surf, (255,255,255), (cx,cy), r, 1)

        vida = t.get("hpTorre", t.get("vida", None))
        vmax = t.get("hpMaxTorre", t.get("vidaMax", None))
        if vida is not None and vmax:
            frac = 0.0 if vmax <= 0 else vida/float(vmax)
            ring_r = CELL//2 - 2
            ring_w = max(4, CELL//8)
            _draw_ring_progress(surf, cx, cy, ring_r, frac, ring_w)

# ================== Enemigos ==================
def draw_enemies(surf, enemies, offx, offy):
    for e in enemies:
        cx = offx + int(e["posX"]*CELL)
        cy = offy + int(e["posY"]*CELL)
        _draw_shadow_ellipse(surf, cx, cy, CELL-10, CELL//3, alpha=80)

        if e.get("efecto","none") == "slow":
            pygame.draw.circle(surf, (140, 180, 255, 60), (cx,cy), CELL//2, 0)

        pygame.draw.circle(surf, (255,140,100), (cx,cy), CELL//3)
        pygame.draw.circle(surf, (30,30,36), (cx,cy), CELL//3, 2)

        pygame.draw.circle(surf, (255,255,255), (cx+6, cy-6), 5)
        pygame.draw.circle(surf, (20,20,20), (cx+7, cy-6), 3)

        hp  = e["hpEnemigo"]; hpM = e["hpMax"]
        frac = max(0.0, min(1.0, hp/float(hpM)))
        _draw_bar(surf, cx-CELL//2, cy-CELL//2-8, CELL, 6, frac)

# ================== Colocación (marcas + ghost) ==================
def draw_place_dots(surf, camino, entrada, base, torres, offx, offy):
    path_set = set(tuple(p) for p in camino)
    towers_set = set(tuple(t["posTorre"]) for t in torres)
    bx,by = base
    ex,ey = entrada
    for x in range(GRID_W):
        for y in range(GRID_H):
            if (x,y) in path_set or (x,y) in towers_set or (x,y)==(bx,by) or (x,y)==(ex,ey):
                continue
            cx = offx + x*CELL + CELL//2
            cy = offy + y*CELL + CELL//2
            pygame.draw.circle(surf, COLOR_PLACE_DOT, (cx,cy), 3)

def draw_ghost_tower(surf, mouse_pos, selected_kind, camino, entrada, base, torres, offx, offy):
    mx, my = mouse_pos
    gx = (mx - offx) // CELL
    gy = (my - offy) // CELL
    if not (0 <= gx < GRID_W and 0 <= gy < GRID_H):
        return
    path_set = set(tuple(p) for p in camino)
    towers_set = set(tuple(t["posTorre"]) for t in torres)
    valid = (gx,gy) not in path_set and (gx,gy) not in towers_set and (gx,gy)!=tuple(entrada) and (gx,gy)!=tuple(base)

    cx = offx + gx*CELL + CELL//2
    cy = offy + gy*CELL + CELL//2
    color = COLOR_OK if valid else COLOR_BAD
    pygame.draw.circle(surf, color, (cx,cy), CELL//3, 3)
    lbl = pygame.font.SysFont("DejaVu Sans", 16, bold=True).render(selected_kind[0].upper(), True, color)
    surf.blit(lbl, lbl.get_rect(center=(cx,cy)))

# ================== HUD ==================
def draw_coin_icon(surf, x, y):
    pygame.draw.circle(surf, (255, 204, 0), (x, y), 9)
    pygame.draw.circle(surf, (170, 120, 0), (x, y), 9, 2)

def draw_skull_icon(surf, x, y):
    pygame.draw.circle(surf, (245,245,245), (x, y), 8)
    pygame.draw.rect(surf, (245,245,245), (x-6, y, 12, 6), border_radius=3)
    pygame.draw.circle(surf, (30,30,36), (x-3, y-2), 2)
    pygame.draw.circle(surf, (30,30,36), (x+3, y-2), 2)
    pygame.draw.rect(surf, (30,30,36), (x-2, y+2, 4, 2))

def draw_hud(surf, state, offx, font, big, wave_alive, wave_total, next_wave_in,
             total_kills):
    x = offx + GRID_W*CELL + 16
    y = 16
    pygame.draw.rect(surf, COLOR_PANEL, (x-12, 8, PANEL_W-8, 720), border_radius=18)

    # Título
    t1 = big.render("Defiende la Torre", True, (245,245,245))
    surf.blit(t1, (x, y))
    y += 40

    # Barra superior con monedas y kills
    bar = pygame.Rect(x, y, PANEL_W-40, 42)
    pygame.draw.rect(surf, COLOR_CARD, bar, border_radius=12)
    draw_coin_icon(surf, bar.x+18, bar.y+21)
    coins = state.get('ejMonedas',0)
    surf.blit(font.render(f"{coins}", True, (240,240,200)), (bar.x+32, bar.y+10))
    draw_skull_icon(surf, bar.x+130, bar.y+21)
    surf.blit(font.render(f"{total_kills} / 200", True, (240,240,240)), (bar.x+144, bar.y+10))
    y += 50

    # Estado
    y = _section_card(surf, x, y, PANEL_W-40, "Estado", font, big=False)
    info_font = pygame.font.SysFont("DejaVu Sans", 16)
    data = [
        ("Tick", f"{state.get('ejTick',0)}"),
        ("Torres", f"{len(state.get('ejTorres',[]))}/{state.get('ejMaxTorres',0)}"),
        ("Vida Base", f"{state.get('ejVidaBase',0)}"),
        ("Oleada", f"{wave_alive} / {wave_total}" if wave_total>0 else "—"),
        ("Próx. Oleada", f"{max(0.0,next_wave_in):0.1f}s")
    ]
    px, py = x+12, y
    for k,v in data:
        key = info_font.render(f"{k}:", True, (200,200,200))
        val = info_font.render(v, True, (240,240,240))
        surf.blit(key, (px, py)); surf.blit(val, (px+120, py))
        py += 22
    y = py + 8

    # Tienda (costos)
    y = _section_card(surf, x, y, PANEL_W-40, "Tienda (costos)", font, big=False)
    row_font = pygame.font.SysFont("DejaVu Sans", 15)
    items = [("Arquera","arquera"), ("Cañón","canon"), ("Mago","mago")]
    bx = x+10; by = y
    for nombre, key in items:
        card = pygame.Rect(bx, by, PANEL_W-60, 38)
        pygame.draw.rect(surf, (34,36,46), card, border_radius=10)
        col = COLOR_TOWER.get(nombre, (180,180,180))
        _badge(surf, nombre, card.x+8, card.y+6, row_font, col, (25,25,25))
        # icono moneda + costo
        draw_coin_icon(surf, card.right-70, card.y+19)
        cost_txt = row_font.render(str(COSTS[key]), True, (235,235,210))
        surf.blit(cost_txt, (card.right-56, card.y+7))
        by += 44
    y = by + 8

    # Torres (vida)
    y = _section_card(surf, x, y, PANEL_W-40, "Torres", font, big=False)
    torres = state.get("ejTorres", [])
    by = y
    for t in sorted(torres, key=lambda z: z.get("idTorre", 0))[:10]:
        card = pygame.Rect(x+10, by, PANEL_W-60, 44)
        pygame.draw.rect(surf, (34,36,46), card, border_radius=10)
        kind = t.get("tipo", "?")
        tid  = t.get("idTorre","?")
        tx,ty = t.get("posTorre",[0,0])
        vida = t.get("hpTorre", t.get("vida", None))
        vmax = t.get("hpMaxTorre", t.get("vidaMax", None))
        col = COLOR_TOWER.get(kind, (180,180,180))
        _badge(surf, kind, card.x+8, card.y+8, row_font, col, (25,25,25))
        info = row_font.render(f"#{tid}  ({tx},{ty})", True, (220,220,230))
        surf.blit(info, (card.x+120, card.y+10))
        if vida is not None and vmax:
            frac = 0.0 if vmax<=0 else vida/float(vmax)
            _draw_bar(surf, card.x+120, card.y+26, card.w-130, 8, frac, bg=(55,60,70), fg=(90,220,140))
        by += 50

# ================== Overlays ==================
def draw_game_over_overlay(surf, font, big):
    w, h = surf.get_size()
    overlay = pygame.Surface((w, h), pygame.SRCALPHA)
    overlay.fill((0, 0, 0, 180))
    surf.blit(overlay, (0, 0))
    title = big.render("¡GAME OVER!", True, (255, 90, 90))
    subtitle = font.render("Un enemigo tocó la base.", True, (240,240,240))
    hint = font.render("Presiona R para reiniciar o Q/Esc para salir.", True, (220,220,220))
    surf.blit(title, title.get_rect(center=(w//2, h//2 - 20)))
    surf.blit(subtitle, subtitle.get_rect(center=(w//2, h//2 + 16)))
    surf.blit(hint, hint.get_rect(center=(w//2, h//2 + 44)))

def draw_victory_overlay(surf, font, big, kills):
    w, h = surf.get_size()
    overlay = pygame.Surface((w, h), pygame.SRCALPHA)
    overlay.fill((0, 0, 0, 160))
    surf.blit(overlay, (0, 0))
    title = big.render("¡GANASTE!", True, (90, 255, 140))
    subtitle = font.render(f"Derrotaste {kills} enemigos.", True, (240,240,240))
    reward   = font.render("Recompensa: Medalla de Campeón", True, (240,240,200))
    hint     = font.render("Presiona R para jugar de nuevo o Q/Esc para salir.", True, (230,230,230))
    surf.blit(title, title.get_rect(center=(w//2, h//2 - 28)))
    surf.blit(subtitle, subtitle.get_rect(center=(w//2, h//2 + 4)))
    surf.blit(reward, reward.get_rect(center=(w//2, h//2 + 28)))
    surf.blit(hint, hint.get_rect(center=(w//2, h//2 + 54)))

# ================== Start Screen ==================
def draw_start_screen(surf, font, big):
    w, h = surf.get_size()
    overlay = pygame.Surface((w, h), pygame.SRCALPHA)
    overlay.fill((0, 0, 0, 160))
    surf.blit(overlay, (0, 0))

    # Card central
    cw, ch = int(w*0.70), int(h*0.62)
    cx, cy = (w - cw)//2, (h - ch)//2
    card = pygame.Rect(cx, cy, cw, ch)
    pygame.draw.rect(surf, (26,28,36), card, border_radius=22)
    pygame.draw.rect(surf, (60,64,80), card, 2, border_radius=22)

    title = big.render("Cómo jugar", True, (255, 230, 120))
    surf.blit(title, title.get_rect(center=(w//2, cy + 40)))

    lines = [
        "Objetivo: Protege la base roja al final del camino.",
        "Coloca torres (Arquera, Cañón, Mago) fuera del camino para eliminar enemigos.",
        "Las oleadas son automáticas cada 5 segundos.",
        "Ganas si eliminas 200 enemigos.",
        "Pierdes si un enemigo toca la base.",
        "",
        "Controles:",
        "1 / 2 / 3  → Seleccionar tipo de torre",
        "Click      → Colocar torre",
        "R          → Reiniciar tras Game Over o Victoria",
        "Q / Esc    → Salir"
    ]
    ytxt = cy + 90
    for ln in lines:
        txt = font.render(ln, True, (230,230,235))
        surf.blit(txt, (cx + 28, ytxt))
        ytxt += 28

    # Botón iniciar
    btn_w, btn_h = 240, 56
    btn = pygame.Rect(0, 0, btn_w, btn_h)
    btn.center = (w//2, cy + ch - 60)
    pygame.draw.rect(surf, (255, 210, 90), btn, border_radius=16)
    pygame.draw.rect(surf, (60,50,20), btn, 2, border_radius=16)
    label = pygame.font.SysFont("DejaVu Sans", 22, bold=True).render("Iniciar juego", True, (25,25,25))
    surf.blit(label, label.get_rect(center=btn.center))
    return btn

# ================== Detección de Game Over (cliente) ==================
def detect_game_over_client(state):
    if not state:
        return False
    enemigos = state.get("ejEnemigos", [])
    base     = state.get("ejBase", None)
    if base is None:
        return False
    bx, by = base
    bcx, bcy = (bx + 0.5, by + 0.5)
    for e in enemigos:
        ex = float(e.get("posX", 0.0)); ey = float(e.get("posY", 0.0))
        dx, dy = (ex - bcx), (ey - bcy)
        if (dx*dx + dy*dy) <= (R_ENEMY + R_BASE)**2:
            return True
    return False

# ================== Principal ==================
def run_visualizer():
    pygame.init()
    pygame.display.set_caption("Defiende la Torre")
    screen = pygame.display.set_mode((GRID_W*CELL + PANEL_W, GRID_H*CELL), pygame.FULLSCREEN)
    clock = pygame.time.Clock()
    font = pygame.font.SysFont("DejaVu Sans", 18)
    big  = pygame.font.SysFont("DejaVu Sans", 28, bold=True)

    offx, offy = 20, 20

    def start_motor():
        m = Motor()
        m.send({"cmd":"noop"})
        return m

    # --- Estado general ---
    game_started = False
    start_btn_rect = None

    motor = None
    logs = LogPanel(font)

    last_state = None
    selected_tower = "arquera"
    game_over = False
    game_won  = False
    t_last_noop = 0
    first_end_seen = False

    # Oleadas / score
    wave_total = 0
    wave_alive = 0
    last_wave_time = time.time()
    next_wave_in = WAVE_INTERVAL

    # Kills y tracking de ids
    total_kills = 0
    prev_ids = set()

    running = True
    while running:
        now = time.time()

        # ===== Eventos =====
        for ev in pygame.event.get():
            if ev.type == pygame.QUIT:
                running = False

            elif ev.type == pygame.KEYDOWN:
                if ev.key in (pygame.K_q, pygame.K_ESCAPE):
                    running = False

                if not game_started:
                    if ev.key in (pygame.K_RETURN, pygame.K_SPACE):
                        motor = start_motor()
                        logs.push("Iniciando motor Haskell…")
                        game_started = True
                        last_wave_time = time.time()
                        continue

                if game_over or game_won:
                    if ev.key == pygame.K_r:
                        logs.push("Reiniciando juego…")
                        if motor: motor.stop()
                        motor = start_motor()
                        last_state = None
                        selected_tower = "arquera"
                        game_over = False
                        game_won  = False
                        first_end_seen = False
                        wave_total = wave_alive = 0
                        total_kills = 0
                        prev_ids = set()
                        last_wave_time = time.time()
                        motor.send({"cmd":"noop"})
                else:
                    if ev.key == pygame.K_1:
                        selected_tower = "arquera"; logs.push("Torre seleccionada: Arquera")
                    elif ev.key == pygame.K_2:
                        selected_tower = "canon"; logs.push("Torre seleccionada: Cañón")
                    elif ev.key == pygame.K_3:
                        selected_tower = "mago"; logs.push("Torre seleccionada: Mago")

            elif ev.type == pygame.MOUSEBUTTONDOWN and ev.button == 1:
                mx, my = pygame.mouse.get_pos()
                if not game_started:
                    if start_btn_rect and start_btn_rect.collidepoint(mx, my):
                        motor = start_motor()
                        logs.push("Iniciando motor Haskell…")
                        game_started = True
                        last_wave_time = time.time()
                        continue
                elif game_started and last_state and not (game_over or game_won):
                    gx = (mx - offx) // CELL
                    gy = (my - offy) // CELL
                    if 0 <= gx < GRID_W and 0 <= gy < GRID_H:
                        motor.send({"cmd":"colocar_torre","pos":[int(gx),int(gy)],"tipo":selected_tower})

        # ===== Lógica cuando el juego YA inició =====
        if game_started and motor and not (game_over or game_won):
            # keep-alive con noop
            if now - t_last_noop > 0.05:
                motor.send({"cmd":"noop"})
                t_last_noop = now

            # stdout del motor
            try:
                while True:
                    line = motor.outq.get_nowait()
                    if line.strip():
                        st = json.loads(line)
                        last_state = st
                        if st.get("ejGameOver", False):
                            game_over = True
            except queue.Empty:
                pass

            # stderr del motor
            try:
                while True:
                    e = motor.errq.get_nowait()
                    logs.push(e)
                    if "Game Over" in e:
                        game_over = True
            except queue.Empty:
                pass

            # Actualizar score de oleada
            if last_state:
                curr_alive = len(last_state.get("ejEnemigos", []))
                if curr_alive == 0:
                    wave_total = 0; wave_alive = 0
                else:
                    wave_alive = curr_alive
                    wave_total = max(wave_total, curr_alive)

                # Kills (ids que desaparecieron este tick -> muertes)
                curr_ids = set(e.get("idEnemigo") for e in last_state.get("ejEnemigos", []))
                died = prev_ids - curr_ids
                if died and not game_over:
                    total_kills += len(died)
                prev_ids = curr_ids

                # Victoria
                if total_kills >= 200:
                    game_won = True

            # Oleadas automáticas cada 5s
            elapsed = now - last_wave_time
            next_wave_in = max(0.0, WAVE_INTERVAL - elapsed)
            if elapsed >= WAVE_INTERVAL:
                motor.send({"cmd":"iniciar_oleada"})
                logs.push("Auto: iniciar oleada")
                last_wave_time = now
                wave_total = 0; wave_alive = 0

            # Game over cliente (base)
            if not game_over and detect_game_over_client(last_state):
                logs.push("Detección local: enemigo tocó base.")
                game_over = True

        # ====== Dibujo ======
        screen.fill(COLOR_BG)
        draw_grid(screen, offx, offy)

        if not game_started:
            # Panel derecho vacío + start
            pygame.draw.rect(screen, COLOR_PANEL, (offx + GRID_W*CELL + 4, 8, PANEL_W-8, 720), border_radius=18)
            t1 = big.render("Defiende la Torre", True, (245,245,245))
            screen.blit(t1, (offx + GRID_W*CELL + 20, 24))
            start_btn_rect = draw_start_screen(screen, font, big)

        else:
            if last_state:
                camino = last_state.get("ejCamino", [])
                entrada = last_state.get("ejEntrada", [1, GRID_H//2])
                base    = last_state.get("ejBase", [GRID_W-2, GRID_H//2])

                draw_path_and_fort(screen, camino, entrada, base, offx, offy)
                draw_place_dots(screen, camino, entrada, base, last_state.get("ejTorres",[]), offx, offy)

                mouse_pos = pygame.mouse.get_pos()
                sel_kind_label = {"arquera":"Arquera","canon":"Canon","mago":"Mago"}.get(selected_tower, "Arquera")
                draw_ghost_tower(screen, mouse_pos, sel_kind_label, camino, entrada, base, last_state.get("ejTorres",[]), offx, offy)

                draw_towers(screen, last_state.get("ejTorres",[]), offx, offy)
                draw_enemies(screen, last_state.get("ejEnemigos",[]), offx, offy)

                draw_hud(screen, last_state, offx, font, big,
                         wave_alive, wave_total, next_wave_in, total_kills)
            else:
                msg = big.render("Esperando estado del motor…", True, (230,230,230))
                screen.blit(msg, (offx, offy))

            # Overlays de fin
            if game_over:
                if not first_end_seen:
                    logs.push("GAME OVER detectado. Pulsa R para reiniciar.")
                    first_end_seen = True
                draw_game_over_overlay(screen, font, big)
            elif game_won:
                if not first_end_seen:
                    logs.push("¡Victoria! Pulsa R para jugar de nuevo.")
                    first_end_seen = True
                draw_victory_overlay(screen, font, big, total_kills)

        # si el motor murió…
        if game_started and motor:
            rc = motor.poll()
            if rc is not None and not (game_over or game_won):
                logs.push(f"Motor terminado (returncode={rc})")
                game_over = True

        pygame.display.flip()
        clock.tick(FPS)

    if motor: motor.stop()
    pygame.quit()

if __name__ == "__main__":
    run_visualizer()
