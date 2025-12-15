# 🏰 Defiende la Torre

Juego **Tower Defense** de próxima generación desarrollado con **Haskell** (motor de simulación funcional) y **Python/Pygame** (interfaz visual moderna), demostrando cómo la programación funcional puede controlar dinámicamente un juego de estrategia en tiempo real con gráficos avanzados.

<p align="center">
  <img src="https://img.shields.io/badge/Haskell-5D4F85?style=for-the-badge&logo=haskell&logoColor=white" />
  <img src="https://img.shields.io/badge/Python-3776AB?style=for-the-badge&logo=python&logoColor=white" />
  <img src="https://img.shields.io/badge/Pygame-00599C?style=for-the-badge&logo=python&logoColor=white" />
  <img src="https://img.shields.io/badge/OpenCV-5C3EE8?style=for-the-badge&logo=opencv&logoColor=white" />
</p>

---

## 📋 Descripción del Proyecto

**Defiende la Torre** es un videojuego tipo tower defense de alta calidad donde:
- El **motor del juego** está implementado en **Haskell**, utilizando principios de programación funcional como recursión, funciones puras, composición y evaluación perezosa.
- La **interfaz gráfica moderna** está desarrollada en **Python con Pygame**, incluyendo sistema de partículas, efectos visuales avanzados (glow, explosiones, proyectiles animados) y diseño estilo Clash Royale.
- La comunicación entre ambos lenguajes se realiza mediante **JSON por stdin/stdout**, permitiendo una integración fluida y robusta.

El juego incluye **menú cinematográfico con video fullscreen**, **artillería táctica con disparo manual**, **sistema de audio dinámico** y **efectos visuales de última generación** que crean una experiencia inmersiva y profesional.

---

## ✨ Características Principales

### 🎮 Jugabilidad Avanzada
- **4 tipos de torres** con roles especializados:
  - 🏹 **Arquera**: Ataque rápido de bajo costo (40💰)
  - 💣 **Cañón**: Daño medio, cadencia equilibrada (60💰)
  - 🔮 **Mago**: Ataque mágico con ralentización (80💰)
  - 🎯 **Artillería**: Disparo manual táctico con explosiones de área (120💰)
- **4 tipos de enemigos** con características únicas:
  - 🔴 **Normal**: Equilibrado y básico
  - 💨 **Rápido**: Veloz pero frágil
  - 🛡️ **Tanque**: Lento pero muy resistente
  - 🦅 **Volador**: Rápido con vida media
- **Sistema de artillería interactivo**: Click en torre → Click en mapa para disparo dirigido
- **Oleadas automáticas** cada 5 segundos con contador visual
- **Objetivo de victoria**: Eliminar 200 enemigos
- **Sistema económico**: Gana monedas eliminando enemigos
- **Detección inteligente de colisiones** para Game Over cuando enemigo alcanza la base

### 🎨 Interfaz Visual de Próxima Generación
- **Menú de inicio cinematográfico** con video fullscreen en loop y dos botones estilizados
- **Panel lateral estilo Clash Royale** con diseño moderno de chips informativos y cards
- **Sistema de partículas** para efectos de colocación, explosiones y disparos
- **Efectos de iluminación (glow)** pulsantes en portales, torres y proyectiles
- **Glassmorphism** en elementos UI con transparencias y bordes sutiles
- **Countdown 3-2-1** con diseño visual impactante en rojo antes de iniciar
- **Proyectiles de artillería** con estela de humo animada, rotación y sombra dinámica
- **Explosiones multicapa** con escala progresiva, fade out y ondas de choque
- **Camino verde texturizado** con flujo animado, flechas direccionales y ruido procedural
- **Arena construible** con variaciones de color y detección de adyacencia al camino
- **Barras de vida modernas** con gradientes, segmentación visual y colores según estado
- **Sprites de alta calidad** (60x60px a 100x100px) para torres, enemigos, portales y efectos
- **Efecto visual de slow** con aura azul pulsante en enemigos ralentizados
- **Sombras suaves** bajo todos los objetos para profundidad
- **Ghost tower** con indicador verde/rojo para validación de colocación
- **Crosshair dinámico** con pulso para modo artillería
- **Banners animados** para oleadas con fade in/out
- **Píldora flotante** mostrando oleada actual y enemigos vivos

### 🎵 Sistema de Audio Dinámico
- **6 pistas musicales** con transiciones automáticas:
  - `intro.mp3`: Música ambiental del menú
  - `conteo.mp3`: Audio sincronizado con countdown 3-2-1
  - `fondo.mp3`: Música durante gameplay en loop
  - `perder.mp3`: Audio emocional al perder
  - `ganar.mp3`: Música de victoria
- **Efectos de sonido (SFX)**:
  - `disparo_bomba.mp3`: Efecto al disparar artillería con sensación táctica
- Transiciones fluidas según estado del juego (menú → countdown → gameplay → fin)

### 🔧 Arquitectura Funcional Robusta
- **Motor en Haskell**:
  - Tipos de datos inmutables (`Tipos.hs`)
  - Sistema de simulación paso a paso (`Simulacion.hs`)
  - Generación procedural de mapas (`Generador.hs`)
  - Serialización JSON completa con Aeson
  - Comunicación por stdin/stdout con Python
- **Cliente en Python**:
  - Renderizado 60 FPS con Pygame
  - Sistema de partículas procedurales
  - Manejo robusto de eventos y entrada
  - Reproductor de video con OpenCV (cv2)
  - Interfaz responsiva y adaptativa

---

## 🛠️ Tecnologías Utilizadas

### Lenguajes y Frameworks
- **Haskell** (Stack) - Motor funcional del juego
- **Python 3.10+** - Cliente gráfico avanzado
- **Pygame 2.5+** - Renderizado y efectos visuales
- **OpenCV (cv2)** - Reproducción de video fullscreen

### Bibliotecas Haskell
```yaml
dependencies:
  - base >= 4.12
  - aeson                    # Serialización JSON
  - bytestring              # Manejo eficiente de datos
  - containers              # Estructuras de datos funcionales
  - text                    # Manipulación de texto
```

### Bibliotecas Python
```bash
pygame>=2.5.0
opencv-python>=4.8.0      # Para video del menú (opcional)
```

---

## 📁 Estructura del Proyecto

```
defiende-la-torre/
├── src/
│   ├── Tipos.hs              # Tipos de datos del juego (Estado, Torres, Enemigos)
│   ├── Simulacion.hs         # Lógica de simulación y actualización por tick
│   ├── Generador.hs          # Generación procedural de mapas y oleadas
│   └── Main.hs               # Punto de entrada del motor Haskell
├── assets/
│   ├── audio/                # Música y efectos de sonido (6 archivos)
│   ├── towers/               # Sprites de torres (4 tipos)
│   ├── enemies/              # Sprites de enemigos (4 variantes)
│   ├── effects/              # Sprites de bombas y explosiones
│   ├── ui/                   # Portal verde y castillo rojo
│   └── video/                # Video de intro (video1.mp4)
├── vizualizador.py           # Cliente Pygame con interfaz completa
├── defiende-la-torre.cabal   # Configuración de Cabal
├── stack.yaml               # Configuración de Stack
├── package.yaml             # Configuración alternativa
└── README.md                # Este archivo
```

---

## 🚀 Instalación y Ejecución

### Prerrequisitos

1. **Haskell Stack**
   ```bash
   # Ubuntu/Debian
   curl -sSL https://get.haskellstack.org/ | sh
   
   # macOS
   brew install haskell-stack
   
   # Verificar instalación
   stack --version
   ```

2. **Python 3.10+ y dependencias**
   ```bash
   # Ubuntu/Debian
   sudo apt-get install python3 python3-pip
   
   # Instalar librerías Python
   pip3 install pygame opencv-python
   ```

### Compilación y Ejecución

```bash
# 1. Clonar el repositorio
git clone https://github.com/tu-usuario/defiende-la-torre.git
cd defiende-la-torre

# 2. Compilar el motor Haskell
stack build

# 3. Ejecutar el juego completo (lanza automáticamente motor + interfaz)
python3 vizualizador.py
```

El visualizador se encarga de:
- Iniciar el proceso del motor Haskell automáticamente
- Establecer comunicación bidireccional por stdin/stdout
- Mostrar el menú con video y manejar toda la interfaz gráfica

---

## 🎮 Cómo Jugar

### Controles

| Tecla/Acción | Función |
|--------------|---------|
| **Click en "Comenzar"** | Iniciar partida desde menú de video |
| **Click en "Acerca de"** | Ver instrucciones y controles |
| **1** | Seleccionar Torre Arquera (40💰) |
| **2** | Seleccionar Torre Cañón (60💰) |
| **3** | Seleccionar Torre Mago (80💰) |
| **4** | Seleccionar Torre Artillería (120💰) |
| **Click Izquierdo (arena)** | Colocar torre seleccionada |
| **Click en Artillería** | Activar modo de disparo manual |
| **Click en mapa (artillería activa)** | Disparar a la posición indicada |
| **ESPACIO/ENTER (menú)** | Iniciar juego |
| **A (menú)** | Toggle panel "Acerca de" |
| **R** | Reiniciar partida (victoria/derrota) |
| **Q / ESC** | Salir del juego |

### Objetivo del Juego

- **Defender tu castillo rojo** del ataque de oleadas automáticas
- **Colocar torres estratégicamente** en celdas de arena (desierto)
- **Eliminar 200 enemigos** para alcanzar la victoria
- **No dejar que ningún enemigo** alcance el castillo (Game Over instantáneo)

### Estrategias Avanzadas

1. 🎯 **Coloca Arqueras al inicio** para maximizar tiempo de disparo
2. 💰 **Administra monedas sabiamente** - equilibra cantidad vs calidad
3. 🔄 **Combina tipos de torres** para cobertura completa del camino
4. ❄️ **Usa Magos estratégicamente** para ralentizar enemigos Tanque
5. 💥 **La Artillería es táctica** - úsala para eliminar grupos concentrados
6. 🏹 **Prioriza cobertura** sobre poder individual al inicio
7. 🎯 **Observa el countdown de oleadas** para preparar defensas

---

## 🔧 Configuración Avanzada

### Modificar estadísticas de torres

En `src/Tipos.hs` o `src/Simulacion.hs`:
```haskell
-- Costos de torres
costTorre :: TipoTorre -> Int
costTorre Arquera = 40
costTorre Canon = 60
costTorre Mago = 80
costTorre Artilleria = 120

-- Estadísticas de torres
rangoTorre :: TipoTorre -> Double
damageTorre :: TipoTorre -> Int
cooldownTorre :: TipoTorre -> Double
```

### Ajustar parámetros visuales

En `vizualizador.py`:
```python
# Tamaño de celda base
CELL = 40

# Intervalo entre oleadas (segundos)
WAVE_INTERVAL = 5.0

# FPS del juego
FPS = 60

# Objetivo de victoria
VICTORY_KILLS = 200
```

### Cambiar assets gráficos

Reemplaza los archivos en `assets/` manteniendo nombres y dimensiones:
- Torres: 60x60px PNG con transparencia
- Enemigos: 40x40px PNG con transparencia
- Efectos: Tamaños variables según tipo
- Video: MP4 compatible con OpenCV

---

## 🌐 Protocolo de Comunicación JSON

### Comandos Cliente → Servidor (stdin)

**Colocar Torre:**
```json
{
  "cmd": "colocar_torre",
  "pos": [15, 8],
  "tipo": "mago"
}
```

**Disparar Artillería:**
```json
{
  "cmd": "disparar_artilleria",
  "id_torre": 5,
  "objetivo": [20.5, 12.3]
}
```

**Iniciar Oleada:**
```json
{
  "cmd": "iniciar_oleada"
}
```

**Actualizar Tick:**
```json
{
  "cmd": "noop"
}
```

### Estado Servidor → Cliente (stdout)

```json
{
  "ejCamino": [[1, 9], [2, 9], [3, 9], ...],
  "ejEntrada": [1, 9],
  "ejBase": [26, 9],
  "ejEnemigos": [
    {
      "idEnemigo": 1,
      "tipo": "Normal",
      "posX": 5.45,
      "posY": 9.0,
      "hpEnemigo": 80,
      "hpMax": 100,
      "efecto": "none"
    }
  ],
  "ejTorres": [
    {
      "idTorre": 1,
      "posTorre": [10, 5],
      "tipo": "Arquera",
      "rango": 3.5,
      "hpTorre": 100,
      "hpMaxTorre": 100
    }
  ],
  "ejProyectiles": [
    {
      "posX": 15.2,
      "posY": 8.7,
      "destinoX": 18.0,
      "destinoY": 9.0,
      "progreso": 0.65,
      "radioExplosion": 2.0
    }
  ],
  "ejMonedas": 340,
  "ejVidaBase": 20,
  "ejTick": 1523,
  "ejMaxTorres": 15,
  "ejGameOver": false
}
```

---

## 🎨 Sistema de Efectos Visuales

### Partículas Procedurales
```python
class ParticleSystem:
    def emit(x, y, count, color, velocity_range, size_range, lifetime)
    def update(dt)
    def draw(surface)
```

Usos:
- Colocación de torres (15 partículas verdes)
- Disparos de artillería (10 partículas amarillas/doradas)
- Explosiones (partículas radiales con fade out)

### Sistema de Glow (Resplandor)
```python
def _draw_glow(surf, cx, cy, radius, color, intensity):
    # Dibuja 3 capas de círculos concéntricos con alpha decreciente
```

Aplicado a:
- Portales (verde pulsante)
- Base (rojo constante)
- Torres (color según tipo, suave)

### Animaciones Principales
- **Flujo del camino**: Flechas que se mueven continuamente
- **Proyectiles**: Rotación, estela de humo, sombra dinámica
- **Explosiones**: Escala progresiva, múltiples ondas, fade out
- **Selección de artillería**: Anillo pulsante con breathing effect
- **Countdown**: Números grandes con borde rojo impactante

---

## 📝 Principios de Programación Funcional Aplicados

### 🔹 Funciones Puras
Toda la lógica del juego es determinista:
```haskell
actualizarEnemigos :: Double -> Estado -> Estado
moverEnemigo :: Enemigo -> Camino -> Enemigo
```

### 🔹 Inmutabilidad
El estado nunca se modifica, siempre se crea uno nuevo:
```haskell
estado { ejEnemigos = nuevosEnemigos, ejMonedas = nuevasMonedas }
```

### 🔹 Composición de Funciones
La simulación se construye componiendo transformaciones:
```haskell
tick dt = actualizarProyectiles 
        . dispararTorres 
        . moverEnemigos dt
```

### 🔹 Recursión
Procesamiento de listas sin bucles imperativos:
```haskell
foldr procesarEnemigo estadoInicial listaEnemigos
```

### 🔹 Pattern Matching
Manejo expresivo de diferentes casos:
```haskell
case comando of
  ColocarTorre pos tipo -> ...
  DispararArtilleria id obj -> ...
  IniciarOleada -> ...
```

---

## 🐛 Solución de Problemas

### El visualizador no inicia el motor
```bash
# Verificar que stack esté instalado
stack --version

# Compilar explícitamente
stack build

# Verificar el ejecutable
stack exec which defiende
```

### No se reproduce el video del menú
```bash
# Instalar OpenCV (opcional, fallback a pantalla negra)
pip3 install opencv-python

# Si persiste, el juego usará fondo negro con mensaje
```

### Pantalla negra después del countdown
- Esperar 1-2 segundos, el estado inicial puede tardar
- Verificar en terminal si hay errores del motor Haskell
- Presionar ESPACIO para forzar primera oleada

### Audio no se escucha
- Verificar que los archivos en `assets/audio/` existan
- Verificar volumen del sistema
- El juego funciona sin audio si faltan archivos

### Torres no disparan
- Las torres tienen cooldown (0.6-1.0 segundos)
- Enemigos deben estar dentro del rango (círculo visible con artillería)
- Verificar que la torre tenga vida (barra circular)

---

## 🤝 Uso de IA en el Desarrollo

Este proyecto utilizó asistencia de IA generativa para:

✅ **Arquitectura del motor funcional** en Haskell con tipos inmutables  
✅ **Sistema de comunicación JSON** bidireccional robusto  
✅ **Diseño del sistema de partículas** procedurales en Python  
✅ **Implementación de efectos visuales** (glow, explosiones, proyectiles)  
✅ **Optimización del renderizado** para 60 FPS estables  
✅ **Diseño de interfaz** estilo Clash Royale con glassmorphism  
✅ **Sistema de audio dinámico** con transiciones automáticas  
✅ **Balanceo de mecánicas** de juego y estadísticas  

### Prompts clave utilizados:
- *"Implementa en Haskell un sistema de torres con cooldown y detección de rango usando funciones puras"*
- *"Crea un sistema de partículas en Pygame con física básica (velocidad, gravedad, fade out)"*
- *"Diseña un protocolo JSON para comunicar estado de juego desde Haskell a Python via stdin/stdout"*
- *"Implementa efectos de glow pulsantes en Pygame usando superficies con alpha"*

---

## 📚 Recursos y Referencias

- [Documentación de Haskell](https://www.haskell.org/documentation/)
- [Stack - Herramienta de construcción](https://docs.haskellstack.org/)
- [Aeson - JSON en Haskell](https://hackage.haskell.org/package/aeson)
- [Pygame Documentation](https://www.pygame.org/docs/)
- [OpenCV Python](https://docs.opencv.org/4.x/d6/d00/tutorial_py_root.html)
- [Programación Funcional en Juegos](https://wiki.haskell.org/Game_Development)

---

## 👨‍💻 Autor

**Jose Mauricio Osorio Rojas**  
Proyecto Final de Programación Funcional  
Universidad Tecnológica de la Mixteca - Séptimo Semestre  
Profesor: Dr. Manuel Hernandez Gutierrez

---

## 📄 Licencia

Este proyecto está bajo la Licencia MIT. Consulta el archivo `LICENSE` para más detalles.

---

## 🎯 Características Futuras Planeadas

- [ ] Sistema de mejora de torres (upgrades)
- [ ] Más variedades de enemigos con habilidades especiales
- [ ] Mapas procedurales con diferentes dificultades
- [ ] Sistema de logros y estadísticas persistentes
- [ ] Modo desafío con condiciones especiales
- [ ] Editor de mapas personalizado
- [ ] Replay system para revisar partidas
- [ ] Leaderboard local con mejores puntuaciones

---

## 🎮 Capturas de Pantalla

### Menú de Inicio
- Video fullscreen en loop
- Botones "Comenzar" y "Acerca de" estilizados
- Panel informativo con overlay semitransparente

### Gameplay
- Panel lateral Clash Royale con chips informativos
- Camino verde con flujo animado
- Torres con efectos glow según tipo
- Enemigos con barras de vida modernas
- Sistema de partículas en acción

### Efectos Especiales
- Countdown 3-2-1 con diseño impactante
- Proyectiles de artillería con estela
- Explosiones multicapa con ondas de choque
- Banners animados de oleadas

---

<p align="center">
  <strong>⭐ Si te gustó el proyecto, dale una estrella en GitHub ⭐</strong>
</p>

<p align="center">
  <img src="https://img.shields.io/badge/Made%20with-Haskell-5D4F85?style=flat-square&logo=haskell" />
  <img src="https://img.shields.io/badge/UI-Pygame-3776AB?style=flat-square&logo=python" />
  <img src="https://img.shields.io/badge/Graphics-Next%20Gen-00D9FF?style=flat-square" />
</p>

<p align="center">
  Hecho con ❤️ y programación funcional
</p>