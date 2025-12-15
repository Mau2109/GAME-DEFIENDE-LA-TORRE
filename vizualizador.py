#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#JOSE MAURICIO OSORIO ROJAS

import os, sys, json, time, math, threading, queue, subprocess, random
import pygame

# ---------- OpenCV (opcional) ----------
try:
    import cv2
    CV2_AVAILABLE = True
except Exception:
    CV2_AVAILABLE = False

# ================== Config base ==================
GRID_W, GRID_H = 28, 18
CELL = 40
PANEL_W = 360
FPS = 60
FULLSCREEN = True

# ---------- Paletas ----------
COLOR_BG = (12, 14, 18)

# Camino VERDE
PATH_BASE = (54, 145, 96)
PATH_EDGE = (38, 110, 75)
PATH_CORE = (90, 185, 120)
PATH_FLOW = (140, 215, 155)

# Terreno construible (ARENA)
SAND_MAIN = (204, 184, 142)
SAND_ALT  = (214, 196, 156)
SAND_DOT  = (184, 166, 128)
SAND_EDGE = (168, 150, 120)

# UI/Panel claro (estilo Clash)
PANEL_BG     = (240, 245, 255)
CARD_BG      = (255, 255, 255)
CARD_BORDER  = (210, 220, 235)
CARD_TEXT    = (28, 38, 55)
TITLE_TEXT   = (15, 25, 45)
SUB_TEXT     = (95, 110, 135)
ACCENT       = (26, 170, 120)
ACCENT_DK    = (20, 120, 85)

# Otros
COLOR_PLACE_DOT = (120, 145, 120)
COLOR_OK = (60, 170, 100)
COLOR_BAD = (220, 70, 70)
COLOR_CROSSHAIR = (245, 70, 70)
COLOR_EXPLOSION = (255, 160, 60)
COLOR_BOMB_TRAIL = (255, 210, 0)

# Radios cliente
R_ENEMY = ((CELL//3) / float(CELL))
R_BASE  = 0.50

# Oleadas
WAVE_INTERVAL = 5.0
WAVE_BANNER_TIME = 2.6

# Costos
COSTS = {"arquera":40, "canon":60, "mago":80, "artilleria":120}

# Cadencia visual (seg)
FIRE_RATES = {"Arquera": 0.60, "Canon": 1.00, "Mago": 0.80}

# ================== Paths de assets ==================
HERE = os.path.dirname(os.path.abspath(__file__))
ASSET_DIR = os.path.join(HERE, "assets")

# ================== AUDIO ==================
class AudioManager:
    def __init__(self, base_dir):
        self.base = base_dir
        self.paths = {
            "intro":   os.path.join(base_dir, "audio", "intro.mp3"),
            "count":   os.path.join(base_dir, "audio", "conteo.mp3"),
            "bg":      os.path.join(base_dir, "audio", "fondo.mp3"),
            "lose":    os.path.join(base_dir, "audio", "perder.mp3"),
            "bomb":    os.path.join(base_dir, "audio", "disparo_bomba.mp3"),
        }
        # victoria
        self.paths["win"] = None
        for c in ["ganar.mp3", "victoria.mp3", "win.mp3"]:
            p = os.path.join(base_dir, "audio", c)
            if os.path.exists(p):
                self.paths["win"] = p
                break

        try:
            pygame.mixer.init(frequency=44100, size=-16, channels=2, buffer=512)
        except Exception as e:
            print("[audio] WARNING mixer init:", e)

        self.sfx_bomb = None
        try:
            if os.path.exists(self.paths["bomb"]):
                self.sfx_bomb = pygame.mixer.Sound(self.paths["bomb"])
                self.sfx_bomb.set_volume(0.8)
        except Exception as e:
            print("[audio] WARNING SFX bomb:", e)

        self.current_music = None
        self.state = None

    def _music_play(self, path, loop=True, volume=0.8, fade_ms=350):
        if not path or not os.path.exists(path):
            print(f"[audio] missing music: {path}"); return
        try:
            if pygame.mixer.music.get_busy():
                pygame.mixer.music.fadeout(fade_ms); time.sleep(fade_ms/1000.0)
            pygame.mixer.music.load(path)
            pygame.mixer.music.set_volume(volume)
            pygame.mixer.music.play(-1 if loop else 0, fade_ms=fade_ms)
            self.current_music = path
        except Exception as e:
            print("[audio] WARNING music play:", e)

    def stop_music(self, fade_ms=300):
        try: pygame.mixer.music.fadeout(fade_ms)
        except Exception: pass
        self.current_music = None

    def play_intro(self):      self.state="intro";     self._music_play(self.paths["intro"], loop=True,  volume=0.85)
    def play_countdown(self):  self.state="countdown"; self._music_play(self.paths["count"], loop=False, volume=0.95)
    def play_background(self): self.state="playing";   self._music_play(self.paths["bg"],    loop=True,  volume=0.80)
    def play_lose(self):       self.state="game_over"; self.stop_music(150); self._music_play(self.paths["lose"], loop=False, volume=0.9, fade_ms=0)
    def play_win(self):
        self.state="victory"; self.stop_music(150)
        if self.paths["win"]: self._music_play(self.paths["win"], loop=False, volume=0.9, fade_ms=0)
    def play_bomb(self):
        try:
            if self.sfx_bomb: self.sfx_bomb.play()
        except Exception: pass

# ================== Assets (sprites) ==================
def build_asset_size_tables():
    return (
        {
            # Tus torres (un poco más grandes que la celda)
            "Arquera":   ("towers/archer.png",    int(CELL*1.50)),
            "Canon":     ("towers/cannon.png",    int(CELL*1.50)),
            "Mago":      ("towers/wizard.png",    int(CELL*1.50)),
            "Artilleria":("towers/artillery.png", int(CELL*1.50)),
        },
        {
            # Enemigos igual que antes (si quieres, puedes subirlos luego)
            "Normal": ("enemies/enemy_normal.png", int(CELL*0.70)),
            "Fast":   ("enemies/enemy_fast.png",   int(CELL*1.00)),
            "Tank":   ("enemies/enemy_tank.png",   int(CELL*1.00)),
            "Flying": ("enemies/enemy_flying.png", int(CELL*1.00)),
        },
        {
            # Más grande la entrada (portal) y la base (castillo)
            "Entrada": ("ui/portal_green.png", int(CELL*2.50)),
            "Base":    ("ui/castle_red.png",   int(CELL*2.00)),
        },
        {
            "Bomb":      ("effects/bomb.png",       int(CELL*1.0)),
            "Explosion": ("effects/explosion.png",  int(CELL*2.2)),
        }
    )


ASSETS_TOWERS, ASSETS_ENEMIES, ASSETS_UI, ASSETS_EFFECTS = build_asset_size_tables()

USE_SPRITES = True
_IMG_CACHE = {}
SPRITE_CACHE = {}

def _load_and_scale(rel_path: str, size_px: int):
    img = pygame.image.load(os.path.join(ASSET_DIR, rel_path)).convert_alpha()
    if size_px is not None:
        img = pygame.transform.smoothscale(img, (size_px, size_px))
    return img

def load_image_cached(kind: str, rel_path: str, size_px: int):
    key = (kind, rel_path, size_px)
    if key in _IMG_CACHE:
        return _IMG_CACHE[key]
    img = _load_and_scale(rel_path, size_px)
    _IMG_CACHE[key] = img
    return img

class SpriteGenerator:
    @staticmethod
    def create_tower_sprite(tower_type, size=60):
        surf = pygame.Surface((size, size), pygame.SRCALPHA)
        pygame.draw.circle(surf, (180,180,180), (size//2,size//2), size//3)
        return surf
    @staticmethod
    def create_enemy_sprite(enemy_type, size=40):
        surf = pygame.Surface((size, size), pygame.SRCALPHA)
        pygame.draw.circle(surf, (255,120,120), (size//2,size//2), size//3)
        return surf
    @staticmethod
    def create_portal_sprite(portal_type, size=80):
        surf = pygame.Surface((size, size), pygame.SRCALPHA)
        pygame.draw.rect(surf, (200,200,255), (6,6,size-12,size-12), border_radius=10)
        return surf
    @staticmethod
    def create_projectile_sprite(size=20):
        surf = pygame.Surface((size, size), pygame.SRCALPHA)
        pygame.draw.circle(surf, (50,50,50), (size//2, size//2), size//3)
        return surf

def get_sprite(sprite_type, subtype=None, size_px=None):
    key = (sprite_type, subtype, size_px)
    if key in SPRITE_CACHE:
        return SPRITE_CACHE[key]
    try:
        if USE_SPRITES:
            if sprite_type == "tower":
                rel, base = ASSETS_TOWERS.get(subtype, (None, None))
                if rel: img = load_image_cached("tower", rel, size_px or base); SPRITE_CACHE[key]=img; return img
            if sprite_type == "enemy":
                tag = "Normal"
                n = (subtype or "").lower()
                if "fast" in n: tag = "Fast"
                elif "tank" in n: tag = "Tank"
                elif "flying" in n: tag = "Flying"
                rel, base = ASSETS_ENEMIES.get(tag, (None, None))
                if rel: img = load_image_cached("enemy", rel, size_px or base); SPRITE_CACHE[key]=img; return img
            if sprite_type == "portal":
                rel, base = ASSETS_UI["Entrada" if subtype=="entrada" else "Base"]
                img = load_image_cached("ui", rel, size_px or base); SPRITE_CACHE[key]=img; return img
            if sprite_type == "projectile":
                rel, base = ASSETS_EFFECTS["Bomb"]
                img = load_image_cached("effects", rel, size_px or base); SPRITE_CACHE[key]=img; return img
            if sprite_type == "effect" and subtype == "Explosion":
                rel, base = ASSETS_EFFECTS["Explosion"]
                img = load_image_cached("effects", rel, size_px or base); SPRITE_CACHE[key]=img; return img
    except Exception as e:
        print(f"[viz] WARNING load '{sprite_type}:{subtype}': {e} — fallback.")
    if sprite_type == "tower":
        img = SpriteGenerator.create_tower_sprite(subtype, size_px or int(CELL*0.95))
    elif sprite_type == "enemy":
        img = SpriteGenerator.create_enemy_sprite(subtype, size_px or int(CELL*0.90))
    elif sprite_type == "portal":
        img = SpriteGenerator.create_portal_sprite("entrada" if subtype=="entrada" else "base", size_px or int(CELL*1.4))
    elif sprite_type == "projectile":
        img = SpriteGenerator.create_projectile_sprite(size_px or int(CELL*0.6))
    elif sprite_type == "effect" and subtype == "Explosion":
        s = size_px or int(CELL*2.0)
        img = pygame.Surface((s, s), pygame.SRCALPHA)
        pygame.draw.circle(img, COLOR_EXPLOSION, (s//2, s//2), s//2)
    else:
        img = pygame.Surface((CELL, CELL), pygame.SRCALPHA)
    SPRITE_CACHE[key] = img
    return img

def rebuild_after_cell_change():
    global ASSETS_TOWERS, ASSETS_ENEMIES, ASSETS_UI, ASSETS_EFFECTS, R_ENEMY
    SPRITE_CACHE.clear(); _IMG_CACHE.clear()
    ASSETS_TOWERS, ASSETS_ENEMIES, ASSETS_UI, ASSETS_EFFECTS = build_asset_size_tables()
    R_ENEMY = ((CELL//3) / float(CELL))

# ================== Motor ==================
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
                self.proc.stdin.write(s); self.proc.stdin.flush()
        except Exception as e:
            print("[viz] ERROR al escribir al motor:", e)
    def poll(self): return self.proc.poll()
    def stop(self):
        try:
            if self.proc and self.poll() is None:
                self.proc.terminate()
                try: self.proc.wait(timeout=0.5)
                except Exception: self.proc.kill()
        except Exception: pass
        self._alive = False

# ================== LogPanel ==================
class LogPanel:
    def __init__(self, font): self.font=font; self.lines=[]
    def push(self, msg):
        print("[viz]", msg)
        self.lines.append(msg)
        if len(self.lines) > 15: self.lines.pop(0)
    def draw(self, surf, x, y, w, h):
        pygame.draw.rect(surf, CARD_BG, (x,y,w,h), border_radius=12)
        pygame.draw.rect(surf, CARD_BORDER, (x,y,w,h), 1, border_radius=12)
        py = y + 10
        title = self.font.render("Mensajes", True, TITLE_TEXT); surf.blit(title, (x+12, py)); py += 28
        for ln in self.lines[-10:]:
            txt = self.font.render("• " + ln, True, SUB_TEXT); surf.blit(txt, (x+12, py)); py += 22

# ================== Util UI ==================
def _draw_shadow_ellipse(surf, cx, cy, w, h, alpha=60):
    shadow = pygame.Surface((w, h), pygame.SRCALPHA)
    pygame.draw.ellipse(shadow, (0, 0, 0, alpha), shadow.get_rect())
    surf.blit(shadow, (cx - w//2, cy - h//2 + 6))

def _draw_bar_segmented(surf, x, y, w, h, frac):
    pygame.draw.rect(surf, (35, 40, 50), (x, y, w, h), border_radius=6)
    frac = max(0.0, min(1.0, float(frac)))
    fg = (80, 200, 120) if frac>=0.66 else ((230, 190, 60) if frac>=0.33 else (220, 70, 70))
    fill_w = int(w * frac)
    pygame.draw.rect(surf, fg, (x, y, fill_w, h), border_radius=6)
    step = max(1, w // 10)
    for i in range(1,10):
        px = x + i*step
        pygame.draw.line(surf, (255,255,255,40), (px, y+1), (px, y+h-2), 1)

# ================== Terreno / Camino ==================
def _sand_variation(x, y, seed):
    random.seed((x*73856093) ^ (y*19349663) ^ seed)
    t = random.random()
    if t < 0.6: return SAND_MAIN
    if t < 0.9: return SAND_ALT
    return SAND_MAIN

def draw_buildable_sand(surf, camino, entrada, base, torres, offx, offy, seed=42):
    path_set = set(tuple(p) for p in camino)
    towers_set = set(tuple(t["posTorre"]) for t in torres)
    e = tuple(entrada); b = tuple(base)
    for x in range(GRID_W):
        for y in range(GRID_H):
            if (x,y) in path_set or (x,y) in towers_set or (x,y)==e or (x,y)==b:
                continue
            rx = offx + x*CELL; ry = offy + y*CELL
            base_col = _sand_variation(x,y,seed)
            pygame.draw.rect(surf, base_col, (rx, ry, CELL, CELL))
            adj_path = ((x-1,y) in path_set) or ((x+1,y) in path_set) or ((x,y-1) in path_set) or ((x,y+1) in path_set)
            if adj_path: pygame.draw.rect(surf, SAND_EDGE, (rx, ry, CELL, CELL), 2)
            for _ in range(2):
                px = rx + random.randint(4, CELL-4); py = ry + random.randint(4, CELL-4)
                pygame.draw.circle(surf, SAND_DOT, (px, py), 1)

def draw_path_tiles(surf, camino, offx, offy, anim_t):
    path_set = set(tuple(p) for p in camino)
    for (x, y) in path_set:
        rx = offx + x*CELL; ry = offy + y*CELL
        cell = pygame.Rect(rx, ry, CELL, CELL)
        pygame.draw.rect(surf, PATH_BASE, cell, border_radius=8)
        noise = pygame.Surface((CELL, CELL), pygame.SRCALPHA)
        for _ in range(max(10, CELL//2)):
            nx = random.randint(2, CELL-2); ny = random.randint(2, CELL-2)
            a = random.randint(30, 55)
            pygame.draw.circle(noise, (255,255,255,a), (nx, ny), 1)
        noise.fill((70, 150, 110, 60), special_flags=pygame.BLEND_RGBA_MULT)
        surf.blit(noise, (rx, ry))
        pygame.draw.rect(surf, PATH_EDGE, cell, 2, border_radius=8)
        inner = cell.inflate(-CELL*0.35, -CELL*0.35)
        pygame.draw.rect(surf, PATH_CORE, inner, border_radius=6)
        flow_phase = (anim_t*2.0 + x*0.25 + y*0.22) % 1.0
        fx = int(rx + CELL*flow_phase); fy = int(ry + CELL*0.5)
        pygame.draw.line(surf, PATH_FLOW, (rx+6, fy), (rx+CELL-6, fy), 2)
        pygame.draw.polygon(surf, PATH_FLOW, [(fx, fy), (fx-7, fy-4), (fx-7, fy+4)])

def draw_path_and_fort(surf, camino, entrada, base, offx, offy, anim_t):
    draw_path_tiles(surf, camino, offx, offy, anim_t)
    ex,ey = entrada
    px = offx + ex*CELL + CELL//2; py = offy + ey*CELL + CELL//2
    portal = get_sprite("portal", "entrada"); surf.blit(portal, portal.get_rect(center=(px, py)))
    bx,by = base
    cx = offx + bx*CELL + CELL//2; cy = offy + by*CELL + CELL//2
    cast = get_sprite("portal", "base"); surf.blit(cast, cast.get_rect(center=(cx, cy)))

# ================== Torres / Enemigos / FX ==================
def tower_shape(surf, kind, cx, cy, is_selected=False):
    r = CELL//3
    _draw_shadow_ellipse(surf, cx, cy, r*2, r, alpha=60)
    img = get_sprite("tower", kind); surf.blit(img, img.get_rect(center=(cx, cy)))
    if is_selected: pygame.draw.circle(surf, (230, 90, 90), (cx, cy), int(CELL*0.65), 3)

def draw_towers(surf, torres, offx, offy, selected_artillery_id):
    for t in torres:
        tx,ty = t["posTorre"]; kind = t.get("tipo","Arquera")
        cx = offx + tx*CELL + CELL//2; cy = offy + ty*CELL + CELL//2
        is_selected = (kind == "Artilleria" and t.get("idTorre") == selected_artillery_id)
        tower_shape(surf, kind, cx, cy, is_selected)
        r = int(t.get("rango", 3)) * CELL
        if is_selected:
            s = pygame.Surface((r*2, r*2), pygame.SRCALPHA)
            pygame.draw.circle(s, (230, 90, 90, 40), (r, r), r)
            pygame.draw.circle(s, (230, 90, 90, 140), (r, r), r, 2)
            surf.blit(s, (cx-r, cy-r))
        vida = t.get("hpTorre", t.get("vida", None)); vmax = t.get("hpMaxTorre", t.get("vidaMax", None))
        if vida is not None and vmax:
            frac = 0.0 if vmax<=0 else vida/float(vmax)
            ring_r = CELL//2 - 2; ring_w = max(4, CELL//8)
            rect = pygame.Rect(cx-ring_r, cy-ring_r, 2*ring_r, 2*ring_r)
            pygame.draw.circle(surf, (60,65,70), (cx, cy), ring_r, ring_w)
            if frac > 0.0:
                start_ang = -math.pi/2; end_ang = start_ang + 2*math.pi*frac
                col = (80,200,120) if frac>=0.66 else ((230,190,60) if frac>=0.33 else (220,70,70))
                pygame.draw.arc(surf, col, rect, start_ang, end_ang, ring_w)

def _enemy_sprite_name(e):
    name = (e.get("tipo","") or "").lower()
    if "fast" in name: return "Fast"
    if "tank" in name: return "Tank"
    if "flying" in name: return "Flying"
    return "Normal"

def draw_enemies(surf, enemies, offx, offy):
    for e in enemies:
        cx = offx + int(e["posX"]*CELL); cy = offy + int(e["posY"]*CELL)
        _draw_shadow_ellipse(surf, cx, cy, CELL-10, CELL//3, alpha=70)
        if e.get("efecto","none") == "slow":
            s = pygame.Surface((CELL, CELL), pygame.SRCALPHA)
            pygame.draw.circle(s, (140, 180, 255, 50), (CELL//2, CELL//2), CELL//2); surf.blit(s, (cx-CELL//2, cy-CELL//2))
        tag = _enemy_sprite_name(e); img = get_sprite("enemy", tag); surf.blit(img, img.get_rect(center=(cx, cy)))
        hp  = e["hpEnemigo"]; hpM = e["hpMax"]
        frac = max(0.0, min(1.0, hp/float(hpM)))
        _draw_bar_segmented(surf, cx-CELL//2, cy-CELL//2-10, CELL, 8, frac)

def draw_projectiles(surf, projectiles, offx, offy, animation_time):
    bomb_img = get_sprite("projectile", "Bomb")
    for p in projectiles:
        px = offx + p["posX"] * CELL; py = offy + p["posY"] * CELL
        shadow_y = py + 20 * (1 - p["progreso"])
        pygame.draw.ellipse(surf, (0,0,0,100), (px-10, shadow_y-5, 20,10))
        trail_length = 5
        for i in range(trail_length):
            factor = (i + 1) / trail_length
            trail_x = px - (p["destinoX"] - p["posX"]) * CELL * 0.12 * factor
            trail_y = py - (p["destinoY"] - p["posY"]) * CELL * 0.12 * factor
            alpha = int(150 * (1 - factor)); radius = int(7 * (1 - factor))
            s = pygame.Surface((radius*2, radius*2), pygame.SRCALPHA)
            pygame.draw.circle(s, (255,210,0,alpha), (radius, radius), radius); surf.blit(s, (int(trail_x - radius), int(trail_y - radius)))
        angle = (animation_time * 360) % 360
        rot = pygame.transform.rotate(bomb_img, angle); surf.blit(rot, rot.get_rect(center=(int(px), int(py))))

def draw_explosions(surf, explosions, offx, offy):
    exp_base = get_sprite("effect", "Explosion")
    for exp in explosions[:]:
        ex, ey, progress, radius = exp
        if progress >= 1.0: explosions.remove(exp); continue
        cx = offx + ex*CELL; cy = offy + ey*CELL
        if progress < 0.5: scale=(progress/0.5); alpha=220
        else: scale=1.0; alpha=int(220 * (1 - (progress - 0.5)/0.5))
        px_size = max(int(radius * CELL * 2 * scale), 8)
        img = pygame.transform.smoothscale(exp_base, (px_size, px_size)).copy()
        img.fill((255,255,255,alpha), special_flags=pygame.BLEND_RGBA_MULT)
        surf.blit(img, img.get_rect(center=(int(cx), int(cy))))
        for i in range(1,3):
            r = int((radius*CELL) * (i/3) * (1 + 0.3*math.sin(progress*6)))
            a = max(0, alpha - i*70)
            if r>4 and a>8:
                s = pygame.Surface((r*2, r*2), pygame.SRCALPHA)
                pygame.draw.circle(s, (255,180,90,a), (r, r), r, 2); surf.blit(s, (int(cx-r), int(cy-r)))

# ================== Disparos visuales (Arquera/Canon/Mago) ==================
def _dist2(ax, ay, bx, by): dx, dy = ax-bx, ay-by; return dx*dx + dy*dy

def choose_target_in_range(tower, enemies):
    tx, ty = tower["posTorre"]; rango = float(tower.get("rango", 3))
    best=None; best_d2=1e9
    for e in enemies:
        ex=float(e.get("posX",0.0)); ey=float(e.get("posY",0.0))
        d2=_dist2(tx+0.5, ty+0.5, ex, ey)
        if d2 <= (rango*rango) and d2 < best_d2: best=(ex,ey); best_d2=d2
    return best

def maybe_emit_shot_effects(state, dt, tower_cooldowns, shot_effects, explosions):
    enemies = state.get("ejEnemigos", [])
    for t in state.get("ejTorres", []):
        kind = t.get("tipo","")
        if kind not in FIRE_RATES: continue
        tid = t.get("idTorre"); now_cool = max(0.0, tower_cooldowns.get(tid, 0.0) - dt)
        if now_cool > 0: tower_cooldowns[tid]=now_cool; continue
        target = choose_target_in_range(t, enemies)
        if not target: continue
        tx,ty = t["posTorre"]; sx=tx+0.5; sy=ty+0.5; ex,ey = target
        dur = 0.12 if kind=="Arquera" else (0.16 if kind=="Canon" else 0.14)
        shot_effects.append({"tipo":kind,"sx":sx,"sy":sy,"tx":ex,"ty":ey,"t":0.0,"dur":dur})
        mini = 0.38 if kind=="Canon" else (0.30 if kind=="Mago" else 0.22)
        explosions.append((ex, ey, 0.0, mini))
        tower_cooldowns[tid]=FIRE_RATES[kind]

def draw_shots(surf, shot_effects, offx, offy, dt):
    for s in shot_effects[:]:
        s["t"] += dt
        if s["t"] >= s["dur"]: shot_effects.remove(s); continue
        sx = offx + s["sx"] * CELL; sy = offy + s["sy"] * CELL
        tx = offx + s["tx"] * CELL; ty = offy + s["ty"] * CELL
        if s["tipo"] == "Arquera":
            pygame.draw.line(surf, (245,245,230), (sx, sy), (tx, ty), max(2, int(CELL*0.06)))
        elif s["tipo"] == "Canon":
            pygame.draw.line(surf, (235,210,175), (sx, sy), (tx, ty), max(4, int(CELL*0.10)))
        elif s["tipo"] == "Mago":
            pygame.draw.line(surf, (160,170,255), (sx, sy), (tx, ty), max(5, int(CELL*0.11)))

# ================== Ghost (para colocar torre) ==================
def draw_ghost_tower(surf, mouse_pos, selected_kind, camino, entrada, base, torres, offx, offy):
    mx, my = mouse_pos
    gx = (mx - offx) // CELL; gy = (my - offy) // CELL
    if not (0 <= gx < GRID_W and 0 <= gy < GRID_H): return
    path_set = set(tuple(p) for p in camino)
    towers_set = set(tuple(t["posTorre"]) for t in torres)
    valid = (gx,gy) not in path_set and (gx,gy) not in towers_set and (gx,gy)!=tuple(entrada) and (gx,gy)!=tuple(base)
    cx = offx + gx*CELL + CELL//2; cy = offy + gy*CELL + CELL//2
    color = COLOR_OK if valid else COLOR_BAD
    pygame.draw.circle(surf, color, (cx,cy), CELL//3, 3)
    lbl = pygame.font.SysFont("DejaVu Sans", 16, bold=True).render(selected_kind[0].upper(), True, color)
    surf.blit(lbl, lbl.get_rect(center=(cx,cy)))

# ================== Game Over (cliente) ==================
def detect_game_over_client(state):
    if not state: return False
    enemigos = state.get("ejEnemigos", [])
    base     = state.get("ejBase", None)
    if base is None: return False
    bx, by = base; bcx, bcy = (bx + 0.5, by + 0.5)
    for e in enemigos:
        ex = float(e.get("posX", 0.0)); ey = float(e.get("posY", 0.0))
        dx, dy = (ex - bcx), (ey - bcy)
        if (dx*dx + dy*dy) <= (R_ENEMY + R_BASE)**2: return True
    return False

# ================== Crosshair (mira) ==================
def draw_crosshair(surf, mouse_pos, torre_seleccionada, offx, offy):
    if not torre_seleccionada: return
    mx, my = mouse_pos
    pulse = abs(math.sin(pygame.time.get_ticks() / 200)) * 8
    radius_outer = 22 + pulse
    s = pygame.Surface((int(radius_outer*2), int(radius_outer*2)), pygame.SRCALPHA)
    pygame.draw.circle(s, (245,70,70, 50), (int(radius_outer), int(radius_outer)), int(radius_outer))
    surf.blit(s, (mx - radius_outer, my - radius_outer))
    line_len = 18
    pygame.draw.line(surf, COLOR_CROSSHAIR, (mx - line_len, my), (mx - 5, my), 3)
    pygame.draw.line(surf, COLOR_CROSSHAIR, (mx + 5, my), (mx + line_len, my), 3)
    pygame.draw.line(surf, COLOR_CROSSHAIR, (mx, my - line_len), (mx, my - 5), 3)
    pygame.draw.line(surf, COLOR_CROSSHAIR, (mx, my + 5), (mx, my + line_len), 3)
    pygame.draw.circle(surf, COLOR_CROSSHAIR, (mx, my), 2)

# ================== Banners / Oleadas ==================
def draw_banner_center(surf, text, alpha, bg=(245,250,248), border=(120,190,160), text_color=(22,26,32)):
    w, h = surf.get_size()
    banner_w = min(int(w*0.62), 980)
    banner_h = max(56, int(h*0.06))
    x = (w - banner_w)//2; y = max(12, int(h*0.015))
    s = pygame.Surface((banner_w, banner_h), pygame.SRCALPHA)
    pygame.draw.rect(s, (*bg, int(220*alpha)), (0,0,banner_w,banner_h), border_radius=int(banner_h*0.35))
    pygame.draw.rect(s, (*border, int(255*alpha)), (0,0,banner_w,banner_h), 2, border_radius=int(banner_h*0.35))
    surf.blit(s, (x, y))
    f = pygame.font.SysFont("DejaVu Sans", max(18, int(h*0.022)), bold=True)
    lab = f.render(text, True, text_color)
    surf.blit(lab, lab.get_rect(center=(x + banner_w//2, y + banner_h//2)))

def draw_wave_pill(surf, waves_started, wave_alive):
    if waves_started <= 0: return
    w, h = surf.get_size()
    pill_h = max(38, int(h*0.045))
    pad = max(12, int(h*0.008))
    text = f"Oleada #{waves_started}  •  Enemigos: {wave_alive}"
    font = pygame.font.SysFont("DejaVu Sans", max(16, int(h*0.02)), bold=True)
    txt = font.render(text, True, (35, 45, 55))
    pill_w = txt.get_width() + pad*2
    x = max(20, (w - pill_w)//2); y = max(10, int(h*0.09))
    s = pygame.Surface((pill_w, pill_h), pygame.SRCALPHA)
    pygame.draw.rect(s, (255,255,255,230), (0,0,pill_w,pill_h), border_radius=int(pill_h*0.5))
    pygame.draw.rect(s, (210,220,235,255), (0,0,pill_w,pill_h), 2, border_radius=int(pill_h*0.5))
    surf.blit(s, (x, y)); surf.blit(txt, txt.get_rect(center=(x+pill_w//2, y+pill_h//2)))

# ================== START MENU: video fullscreen + 2 botones ==================
class VideoPlayer:
    def __init__(self, path):
        self.enabled = CV2_AVAILABLE and os.path.exists(path)
        self.path = path
        self.cap = None
        self.last_frame = None
        self.last_tick = 0.0
        self.fps = 30.0
        self.frame_delay = 1 / self.fps
        if self.enabled:
            try:
                self.cap = cv2.VideoCapture(path)
                if not self.cap.isOpened():
                    self.enabled = False
                else:
                    fps = self.cap.get(cv2.CAP_PROP_FPS)
                    if fps and fps > 1:
                        self.fps = fps
                        self.frame_delay = 1 / self.fps
            except Exception:
                self.enabled = False

    def _read_frame(self):
        ok, frame = self.cap.read()
        if not ok:
            # loop
            self.cap.set(cv2.CAP_PROP_POS_FRAMES, 0)
            ok, frame = self.cap.read()
            if not ok:
                return None
        return frame

    def update_surface_cover(self, screen_size):
        sw, sh = screen_size
        if not self.enabled:
            s = pygame.Surface((sw, sh)); s.fill((0, 0, 0))
            f = pygame.font.SysFont("DejaVu Sans", max(22, sh//32), bold=True)
            t = f.render("Video no disponible (instala opencv-python)", True, (220,220,220))
            s.blit(t, t.get_rect(center=(sw//2, sh//2)))
            return s
        now = time.time()
        if self.last_frame is not None and (now - self.last_tick) < self.frame_delay:
            return self.last_frame
        frame = self._read_frame()
        if frame is None:
            s = pygame.Surface((sw, sh)); s.fill((0,0,0)); return s
        fh, fw = frame.shape[:2]
        if fw == 0 or fh == 0:
            s = pygame.Surface((sw, sh)); s.fill((0,0,0)); return s
        # Escalado tipo "cover": llena pantalla conservando aspecto, recortando sobrante
        scale = max(sw / fw, sh / fh)
        new_w, new_h = int(fw * scale), int(fh * scale)
        resized = cv2.resize(frame, (new_w, new_h), interpolation=cv2.INTER_AREA)
        x0 = max(0, (new_w - sw) // 2)
        y0 = max(0, (new_h - sh) // 2)
        cropped = resized[y0:y0+sh, x0:x0+sw]
        rgb = cv2.cvtColor(cropped, cv2.COLOR_BGR2RGB)
        surf = pygame.image.frombuffer(rgb.tobytes(), (sw, sh), "RGB")
        self.last_frame = surf
        self.last_tick = now
        return surf

    def stop(self):
        if self.cap is not None:
            try: self.cap.release()
            except Exception: pass

def draw_green_button(surf, rect, label):
    pygame.draw.rect(surf, ACCENT, rect, border_radius=14)
    pygame.draw.rect(surf, ACCENT_DK, rect, 3, border_radius=14)
    f = pygame.font.SysFont("DejaVu Sans", max(22, rect.h//3), bold=True)
    lab = f.render(label, True, (255,255,255))
    surf.blit(lab, lab.get_rect(center=rect.center))

def draw_start_menu_fullscreen(surf, video_player, show_about):
    frame = video_player.update_surface_cover(surf.get_size())
    surf.blit(frame, (0, 0))
    w, h = surf.get_size()
    # Botones
    btn_w, btn_h = max(220, w//7), max(58, h//14)
    gap = max(24, w//60)
    btn_play  = pygame.Rect(0, 0, btn_w, btn_h)
    btn_about = pygame.Rect(0, 0, btn_w, btn_h)
    center_y = h - int(h*0.12)
    btn_play.center  = (w//2 - btn_w//2 - gap//2, center_y)
    btn_about.center = (w//2 + btn_w//2 + gap//2, center_y)
    draw_green_button(surf, btn_play, "Comenzar")
    draw_green_button(surf, btn_about, "Acerca de")

    # Panel "Acerca de" (overlay transparente encima del video)
    if show_about:
        bar_h = max(220, h//3)
        overlay = pygame.Surface((w, bar_h), pygame.SRCALPHA)
        overlay.fill((0, 0, 0, 120))
        surf.blit(overlay, (0, int(h*0.08)))
        y0 = int(h*0.08) + 24
        title = pygame.font.SysFont("DejaVu Sans", max(30, h//28), bold=True)
        body  = pygame.font.SysFont("DejaVu Sans", max(18, h//42))
        lines = [
            "Objetivo: Evita que los enemigos lleguen al castillo rojo. ¡Derrota 200 para ganar!",
            "Cómo jugar: 1/2/3/4 para elegir torre • Clic en arena para colocar.",
            "Artillería: clic sobre una artillería y luego en el mapa para disparar.",
            "Oleadas automáticas cada 5 s • Pierdes si tocan el castillo.",
            "Controles: R reinicia • Q/Esc salir."
        ]
        surf.blit(title.render("Acerca de", True, (255,255,255)), (40, y0)); y0 += 40
        for ln in lines:
            surf.blit(body.render(ln, True, (235,235,235)), (40, y0)); y0 += 28
    return btn_play, btn_about

# ================== Panel estilo "Clash Royale" ==================
def draw_chip(surf, x, y, text, icon=None):
    f = pygame.font.SysFont("DejaVu Sans", 18, bold=True)
    t = f.render(text, True, (35,45,60))
    tw, th = t.get_size()
    padx, pady = 14, 8
    w = tw + (padx*2) + (28 if icon else 0)
    h = th + (pady*2)
    r = pygame.Rect(x, y, w, h)
    pygame.draw.rect(surf, (255,255,255), r, border_radius=h//2)
    pygame.draw.rect(surf, (210,220,235), r, 2, border_radius=h//2)
    if icon:
        surf.blit(icon, (r.x+10, r.y + (h-20)//2))
        surf.blit(t, (r.x+10+22, r.y+(h-th)//2))
    else:
        surf.blit(t, (r.x+padx, r.y+(h-th)//2))
    return r

def draw_store_button(surf, rect, name, price, sprite=None):
    pygame.draw.rect(surf, CARD_BG, rect, border_radius=14)
    pygame.draw.rect(surf, (210,220,235), rect, 2, border_radius=14)
    name_font = pygame.font.SysFont("DejaVu Sans", 18, bold=True)
    price_font= pygame.font.SysFont("DejaVu Sans", 16, bold=True)
    if sprite:
        icon = pygame.transform.smoothscale(sprite, (rect.h-14, rect.h-14))
        surf.blit(icon, (rect.x+10, rect.y+7))
    surf.blit(name_font.render(name, True, TITLE_TEXT), (rect.x + (rect.h if sprite else 12) + 12, rect.y+10))
    # Moneda
    px = rect.right - 72; py = rect.y + rect.h//2
    pygame.draw.circle(surf, (255, 200, 50), (px, py), 12)
    pygame.draw.circle(surf, (180, 140, 20), (px, py), 12, 2)
    surf.blit(price_font.render(str(price), True, SUB_TEXT), (px+18, py-10))

def draw_panel_clash(surf, state, offx, total_kills, selected_artillery_id):
    x = offx + GRID_W*CELL + 16
    y = 16
    # Fondo panel
    pygame.draw.rect(surf, PANEL_BG, (x-12, 8, PANEL_W-8, surf.get_height()-16), border_radius=18)

    title_font = pygame.font.SysFont("DejaVu Sans", 28, bold=True)
    surf.blit(title_font.render("Defiende la Torre", True, TITLE_TEXT), (x, y)); y += 46

    # chips
    coin_icon = pygame.Surface((20,20), pygame.SRCALPHA)
    pygame.draw.circle(coin_icon, (255, 200, 50), (10,10), 10); pygame.draw.circle(coin_icon, (180,140,20), (10,10), 10,2)
    skull_icon = pygame.Surface((20,20), pygame.SRCALPHA)
    pygame.draw.circle(skull_icon, (75,85,100), (10,10), 9); pygame.draw.circle(skull_icon, (255,255,255), (8,8), 2)
    _ = draw_chip(surf, x, y, f"Monedas: {state.get('ejMonedas',0)}", coin_icon); y += _.h + 10
    _ = draw_chip(surf, x, y, f"Derrotados: {total_kills}/200", skull_icon); y += _.h + 10
    _ = draw_chip(surf, x, y, f"Torres: {len(state.get('ejTorres',[]))}/{state.get('ejMaxTorres',0)}"); y += _.h + 10
    _ = draw_chip(surf, x, y, f"Vida Base: {state.get('ejVidaBase',0)}"); y += _.h + 16

    # aviso artillería (si hay una seleccionada para disparar)
    if selected_artillery_id:
        alert = pygame.Rect(x, y, PANEL_W-40, 60)
        pygame.draw.rect(surf, (255,255,255), alert, border_radius=12)
        pygame.draw.rect(surf, (220,100,100), alert, 2, border_radius=12)
        lab = pygame.font.SysFont("DejaVu Sans", 16, bold=True).render("🎯 Artillería activa — Click para disparar", True, (180,40,40))
        surf.blit(lab, lab.get_rect(center=alert.center))
        y += 68

    # Tienda
    store_title = pygame.font.SysFont("DejaVu Sans", 20, bold=True).render("Tienda", True, TITLE_TEXT)
    surf.blit(store_title, (x, y)); y += 30
    btn_h, gap = 60, 12
    for name,key in [("Arquera","arquera"), ("Cañón","canon"), ("Mago","mago"), ("Artillería","artilleria")]:
        r = pygame.Rect(x, y, PANEL_W-40, btn_h)
        spr = get_sprite("tower", {"arquera":"Arquera","canon":"Canon","mago":"Mago","artilleria":"Artilleria"}[key], size_px=btn_h-14)
        draw_store_button(surf, r, name, COSTS[key], sprite=spr)
        y += btn_h + gap

    # Lista de torres
    y += 2
    list_title = pygame.font.SysFont("DejaVu Sans", 20, bold=True).render("Tus torres", True, TITLE_TEXT)
    surf.blit(list_title, (x, y)); y += 28
    for t in sorted(state.get("ejTorres", []), key=lambda z: z.get("idTorre", 0))[:8]:
        card = pygame.Rect(x, y, PANEL_W-40, 58)
        pygame.draw.rect(surf, CARD_BG, card, border_radius=12)
        pygame.draw.rect(surf, CARD_BORDER, card, 2, border_radius=12)
        kind = t.get("tipo","?"); tid=t.get("idTorre","?"); tx,ty=t.get("posTorre",[0,0])
        name_font = pygame.font.SysFont("DejaVu Sans", 16, bold=True)
        sub_font  = pygame.font.SysFont("DejaVu Sans", 14)
        spr = get_sprite("tower", kind, size_px=46)
        surf.blit(spr, (card.x+8, card.y+6))
        surf.blit(name_font.render(f"{kind}  #{tid}", True, TITLE_TEXT), (card.x+62, card.y+10))
        surf.blit(sub_font.render(f"({tx},{ty})", True, SUB_TEXT), (card.x+62, card.y+30))
        vida = t.get("hpTorre", t.get("vida", None)); vmax = t.get("hpMaxTorre", t.get("vidaMax", None))
        if vida is not None and vmax:
            frac = 0.0 if vmax<=0 else vida/float(vmax)
            _draw_bar_segmented(surf, card.x+150, card.y+36, card.w-160, 10, frac)
        y += 66

# ================== Principal ==================
def run_visualizer():
    global CELL

    pygame.init()
    pygame.display.set_caption("Defiende la Torre")

    if FULLSCREEN:
        info = pygame.display.Info()
        screen = pygame.display.set_mode((info.current_w, info.current_h), pygame.FULLSCREEN)
    else:
        screen = pygame.display.set_mode((GRID_W*CELL + PANEL_W, GRID_H*CELL))

    audio = AudioManager(ASSET_DIR)
    audio.play_intro()  # música del menú

    # Ajuste de CELL para ocupar pantalla
    screen_w, screen_h = screen.get_size()
    avail_w = screen_w - PANEL_W - 60
    avail_h = screen_h - 40
    CELL = int(max(24, min(avail_w / GRID_W, avail_h / GRID_H)))
    rebuild_after_cell_change()
    offx = 20
    offy = max(20, (screen_h - GRID_H*CELL) // 2)

    clock = pygame.time.Clock()
    font = pygame.font.SysFont("DejaVu Sans", 18)
    big  = pygame.font.SysFont("DejaVu Sans", 28, bold=True)

    def start_motor():
        m = Motor(); m.send({"cmd":"noop"}); return m

    # --- Estado del menú de video ---
    show_about = False
    video_path = os.path.join(ASSET_DIR, "video", "video1.mp4")
    video_player = VideoPlayer(video_path)

    # --- Estado de juego ---
    game_started = False
    motor = None
    logs = LogPanel(font)
    last_state = None
    selected_tower = "arquera"
    game_over = False
    game_won  = False
    t_last_noop = 0
    first_end_seen = False

    selected_artillery_id = None
    explosions = []
    animation_time = 0.0
    tower_cooldowns = {}
    shot_effects = []

    wave_total = 0; wave_alive = 0
    last_wave_time = time.time()
    wave_banner_until = 0.0
    waves_started = 0

    total_kills = 0
    prev_ids = set()

    countdown_active = False
    countdown_start_time = 0.0
    defend_message_until = 0.0

    # Botones de victoria (si usas overlay tipo botones)
    victory_btn_restart = None
    victory_btn_quit = None

    running = True
    while running:
        now = time.time()
        dt = clock.tick(FPS) / 1000.0
        animation_time += dt

        # Avanza explosiones
        for i, exp in enumerate(explosions):
            x, y, progress, radius = exp
            explosions[i] = (x, y, progress + dt * 2, radius)

        # Eventos
        for ev in pygame.event.get():
            if ev.type == pygame.QUIT:
                running = False

            elif ev.type == pygame.KEYDOWN:
                if ev.key in (pygame.K_q, pygame.K_ESCAPE):
                    running = False  # salir siempre con ESC o Q


                if not game_started:
                    if ev.key in (pygame.K_a,):
                        show_about = not show_about
                    if ev.key in (pygame.K_RETURN, pygame.K_SPACE):
                        motor = start_motor()
                        logs.push("Iniciando motor Haskell…")
                        game_started = True
                        countdown_active = True
                        countdown_start_time = now
                        audio.play_countdown()
                        video_player.stop()
                        continue

                elif game_started and (game_over or game_won):
                    if ev.key == pygame.K_r:
                        logs.push("Reiniciando juego…")
                        if motor: motor.stop()
                        motor = start_motor()
                        last_state = None
                        selected_tower = "arquera"
                        selected_artillery_id = None
                        explosions.clear()
                        shot_effects.clear()
                        tower_cooldowns.clear()
                        game_over = False
                        game_won  = False
                        first_end_seen = False
                        wave_total = wave_alive = 0
                        waves_started = 0
                        total_kills = 0
                        prev_ids = set()
                        countdown_active = True
                        countdown_start_time = now
                        defend_message_until = 0.0
                        wave_banner_until = 0.0
                        audio.play_countdown()
                        motor.send({"cmd":"noop"})

                else:
                    if ev.key == pygame.K_1:
                        selected_tower = "arquera"; selected_artillery_id = None
                    elif ev.key == pygame.K_2:
                        selected_tower = "canon"; selected_artillery_id = None
                    elif ev.key == pygame.K_3:
                        selected_tower = "mago"; selected_artillery_id = None
                    elif ev.key == pygame.K_4:
                        selected_tower = "artilleria"; selected_artillery_id = None

            elif ev.type == pygame.MOUSEBUTTONDOWN and ev.button == 1:
                mx, my = pygame.mouse.get_pos()

                if not game_started:
                    # Recalcular botones sobre el frame actual
                    btn_play, btn_about = draw_start_menu_fullscreen(screen, video_player, show_about)
                    if btn_play.collidepoint(mx, my):
                        motor = start_motor()
                        logs.push("Iniciando motor Haskell…")
                        game_started = True
                        countdown_active = True
                        countdown_start_time = now
                        audio.play_countdown()
                        video_player.stop()
                        continue
                    if btn_about.collidepoint(mx, my):
                        show_about = not show_about
                        continue

                elif game_started and last_state and not (game_over or game_won):
                    if selected_artillery_id:
                        game_x = (mx - offx) / CELL; game_y = (my - offy) / CELL
                        if 0 <= game_x < GRID_W and 0 <= game_y < GRID_H:
                            motor.send({"cmd":"disparar_artilleria","id_torre":selected_artillery_id,"objetivo":[game_x,game_y]})
                            logs.push(f"Artillería dispara a ({game_x:.1f}, {game_y:.1f})")
                            audio.play_bomb()
                    else:
                        gx = (mx - offx) // CELL; gy = (my - offy) // CELL
                        if 0 <= gx < GRID_W and 0 <= gy < GRID_H:
                            clicked_artillery = False
                            for torre in last_state.get("ejTorres", []):
                                tx, ty = torre.get("posTorre", [0, 0])
                                if (tx, ty) == (gx, gy) and torre.get("tipo") == "Artilleria":
                                    selected_artillery_id = torre.get("idTorre")
                                    clicked_artillery = True
                                    logs.push(f"Artillería #{selected_artillery_id} activada - Click para disparar")
                                    break
                            if not clicked_artillery:
                                motor.send({"cmd":"colocar_torre","pos":[int(gx), int(gy)],"tipo":selected_tower})
                                logs.push(f"Colocando torre {selected_tower} en ({gx}, {gy})")

                elif game_won and victory_btn_restart and victory_btn_quit:
                    if victory_btn_restart.collidepoint(mx, my):
                        if motor: motor.stop()
                        motor = start_motor()
                        last_state = None
                        selected_tower = "arquera"
                        selected_artillery_id = None
                        explosions.clear()
                        shot_effects.clear()
                        tower_cooldowns.clear()
                        game_over = False
                        game_won  = False
                        first_end_seen = False
                        wave_total = wave_alive = 0
                        waves_started = 0
                        total_kills = 0
                        prev_ids = set()
                        countdown_active = True
                        countdown_start_time = now
                        defend_message_until = 0.0
                        wave_banner_until = 0.0
                        audio.play_countdown()
                        motor.send({"cmd":"noop"})
                    if victory_btn_quit.collidepoint(mx, my):
                        running = False

        # Comunicación con motor
        if game_started and motor and not (game_over or game_won):
            if now - t_last_noop > 0.05:
                motor.send({"cmd":"noop"}); t_last_noop = now
            try:
                while True:
                    line = motor.outq.get_nowait()
                    if line.strip():
                        st = json.loads(line)
                        last_state = st
                        for p in st.get("ejProyectiles", []):
                            if p.get("progreso", 0) >= 1.0:
                                explosions.append((p.get("destinoX", 0), p.get("destinoY", 0), 0.0, p.get("radioExplosion", 2.0)))
                        if st.get("ejGameOver", False):
                            game_over = True
            except queue.Empty:
                pass
            try:
                while True:
                    e = motor.errq.get_nowait()
                    logs.push(e)
                    if "Game Over" in e:
                        game_over = True
            except queue.Empty:
                pass

            if last_state:
                curr_alive = len(last_state.get("ejEnemigos", []))
                if curr_alive == 0:
                    wave_total = 0; wave_alive = 0
                else:
                    wave_alive = curr_alive
                    wave_total = max(wave_total, curr_alive)

                curr_ids = set(e.get("idEnemigo") for e in last_state.get("ejEnemigos", []))
                died = prev_ids - curr_ids
                if died and not game_over:
                    total_kills += len(died)
                prev_ids = curr_ids

                if total_kills >= 200:
                    game_won = True

                if not countdown_active and now >= defend_message_until:
                    maybe_emit_shot_effects(last_state, dt, tower_cooldowns, shot_effects, explosions)

            if not countdown_active and now >= defend_message_until:
                if (now - last_wave_time) >= WAVE_INTERVAL:
                    motor.send({"cmd":"iniciar_oleada"})
                    last_wave_time = now
                    wave_total = 0; wave_alive = 0
                    waves_started += 1
                    wave_banner_until = now + WAVE_BANNER_TIME

            if not game_over and detect_game_over_client(last_state):
                logs.push("Detección local: enemigo tocó base.")
                game_over = True

        # Audio por estado
        if not game_started:
            if audio.state != "intro":
                audio.play_intro()
        else:
            if countdown_active:
                if audio.state != "countdown": audio.play_countdown()
            elif game_over:
                if audio.state != "game_over": audio.play_lose()
            elif game_won:
                if audio.state != "victory": audio.play_win()
            else:
                if audio.state != "playing": audio.play_background()

        # Render
        screen.fill(COLOR_BG)

        if not game_started:
            # Menú con video + botones
            draw_start_menu_fullscreen(screen, video_player, show_about)

        else:
            if last_state:
                camino = last_state.get("ejCamino", [])
                entrada = last_state.get("ejEntrada", [1, GRID_H//2])
                base    = last_state.get("ejBase", [GRID_W-2, GRID_H//2])

                draw_buildable_sand(screen, camino, entrada, base, last_state.get("ejTorres",[]), offx, offy, seed=123)
                draw_path_and_fort(screen, camino, entrada, base, offx, offy, animation_time)

                if not selected_artillery_id:
                    mouse_pos = pygame.mouse.get_pos()
                    sel_kind_label = {"arquera":"Arquera", "canon":"Canon", "mago":"Mago", "artilleria":"Artilleria"}[selected_tower]
                    draw_ghost_tower(screen, mouse_pos, sel_kind_label, camino, entrada, base, last_state.get("ejTorres",[]), offx, offy)

                draw_towers(screen, last_state.get("ejTorres",[]), offx, offy, selected_artillery_id)
                draw_enemies(screen, last_state.get("ejEnemigos",[]), offx, offy)
                draw_shots(screen, shot_effects, offx, offy, dt)
                draw_projectiles(screen, last_state.get("ejProyectiles", []), offx, offy, animation_time)
                draw_explosions(screen, explosions, offx, offy)

                if selected_artillery_id:
                    draw_crosshair(screen, pygame.mouse.get_pos(), True, offx, offy)

                # Panel Clash Royale
                draw_panel_clash(screen, last_state, offx, total_kills, selected_artillery_id)

                # Mensajería de oleadas
                if now < wave_banner_until and not countdown_active and not game_over and not game_won:
                    a = (wave_banner_until - now) / WAVE_BANNER_TIME
                    draw_banner_center(screen, f"¡Oleada #{waves_started} iniciada! Defiende tu torre", max(0.0, min(1.0, a)))
                if not countdown_active and not game_over and not game_won:
                    draw_wave_pill(screen, waves_started, wave_alive)
            else:
                msg = big.render("Esperando estado del motor…", True, (230,230,230))
                screen.blit(msg, (offx, offy))

            # Overlays fin de juego
            if game_over:
                if not first_end_seen:
                    logs.push("GAME OVER detectado. Pulsa R para reiniciar."); first_end_seen = True
                w,h = screen.get_size()
                overlay = pygame.Surface((w,h), pygame.SRCALPHA); overlay.fill((0,0,0,160))
                screen.blit(overlay,(0,0))
                draw_banner_center(screen, "Game Over — Presiona R para reiniciar", 1.0)

            elif game_won:
                if not first_end_seen:
                    logs.push("¡Victoria! Pulsa R para jugar de nuevo."); first_end_seen = True
                w,h = screen.get_size()
                overlay = pygame.Surface((w,h), pygame.SRCALPHA); overlay.fill((0,0,0,150))
                screen.blit(overlay,(0,0))
                draw_banner_center(screen, "¡Felicidades! Pulsa R para reiniciar o Esc para salir.", 1.0)

        # Contador 3-2-1 + “¡Defiende la torre!”
        if game_started and countdown_active:
            elapsed = now - countdown_start_time
            phase = 3 - int(elapsed)
            if phase > 0:
                w, h = screen.get_size()
                num_font = pygame.font.SysFont("DejaVu Sans", 120, bold=True)
                num = num_font.render(str(phase), True, (210, 40, 40))
                box_w, box_h = 220, 160
                box = pygame.Rect(0,0,box_w,box_h); box.center = (w//2, h//2)
                s = pygame.Surface((box_w, box_h), pygame.SRCALPHA)
                pygame.draw.rect(s, (255,255,255, 235), (0,0,box_w,box_h), border_radius=20)
                pygame.draw.rect(s, (210, 40, 40,255), (0,0,box_w,box_h), 3, border_radius=20)
                screen.blit(s, box.topleft); screen.blit(num, num.get_rect(center=box.center))
            else:
                countdown_active = False
                defend_message_until = now + 1.2
                last_wave_time = now
                audio.play_background()

        if game_started and not countdown_active and now < defend_message_until and not game_over and not game_won:
            a = (defend_message_until - now) / 1.2
            draw_banner_center(screen, "¡Defiende la torre!", max(0.0, min(1.0, a)))

        if game_started and motor:
            rc = motor.poll()
            if rc is not None and not (game_over or game_won):
                logs.push(f"Motor terminado (returncode={rc})")
                game_over = True
                audio.play_lose()

        pygame.display.flip()

    # Salida limpia
    try:
        pygame.mixer.music.stop(); pygame.mixer.quit()
    except Exception: pass
    if 'video_player' in locals():
        try: video_player.stop()
        except Exception: pass
    if 'motor' in locals() and motor:
        motor.stop()
    pygame.quit()

if __name__ == "__main__":
    run_visualizer()
