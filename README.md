# 🏰 Defiende la Torre

Juego **Tower Defense** desarrollado con **Haskell** (lógica funcional del motor) y **Python/Pygame** (interfaz gráfica e interacción), demostrando cómo la programación funcional puede controlar dinámicamente un juego de estrategia en tiempo real.

<p align="center">
  <img src="https://img.shields.io/badge/Haskell-5D4F85?style=for-the-badge&logo=haskell&logoColor=white" />
  <img src="https://img.shields.io/badge/Python-3776AB?style=for-the-badge&logo=python&logoColor=white" />
  <img src="https://img.shields.io/badge/Pygame-00599C?style=for-the-badge&logo=python&logoColor=white" />
</p>

---

## 📋 Descripción del Proyecto

**Defiende la Torre** es un videojuego tipo tower defense donde:
- El **motor del juego** está implementado en **Haskell** (70%), utilizando principios de programación funcional como recursión, funciones puras, composición y evaluación perezosa.
- La **interfaz gráfica** está desarrollada en **Python con Pygame** (30%), encargándose de la visualización, renderizado y manejo de eventos del usuario.
- La comunicación entre ambos lenguajes se realiza mediante **sockets TCP** intercambiando mensajes **JSON**.

El mapa, las oleadas de enemigos y los patrones de ataque se generan de forma **procedural y funcional**, mostrando cómo un enfoque declarativo puede controlar dinámicamente la lógica de un juego de estrategia.

---

## ✨ Características Principales

### 🎮 Jugabilidad
- **Generación procedural de caminos** usando algoritmos fractales (Curva del Dragón)
- **4 tipos de torres** con características únicas:
  - 🔵 **Básica**: Daño medio, cadencia equilibrada (100💰)
  - 🎯 **Francotirador**: Alto daño, largo alcance (150💰)
  - ❄️ **Congelación**: Ralentiza enemigos (120💰)
  - 💥 **Explosiva**: Daño en área (180💰)
- **4 tipos de enemigos** con diferentes estadísticas:
  - 🔴 **Normal**: Equilibrado
  - 💨 **Rápido**: Veloz pero frágil
  - 🛡️ **Tanque**: Lento pero resistente
  - 🦅 **Volador**: Rápido con vida media
- **Sistema de oleadas progresivas** con dificultad incremental
- **Sistema económico**: Gana oro eliminando enemigos, gasta oro colocando torres
- **Detección de colisiones** y sistema de proyectiles

### 🎨 Interfaz Gráfica Mejorada
- Fondo degradado tipo cielo
- Patrón de césped animado
- Camino procedural con sombras y efectos visuales
- Torres con animaciones de disparo y cooldown
- Enemigos con diferentes colores según tipo
- Barras de vida coloridas (verde→amarillo→rojo)
- Preview de torre antes de colocar (muestra rango)
- HUD completo con estadísticas en tiempo real
- Efectos visuales para portales de inicio y fin

### 🔧 Arquitectura Funcional
- **Motor en Haskell**:
  - Generación procedural de caminos (`PathGeneration.hs`)
  - Sistema de movimiento de enemigos (`EnemyMovement.hs`)
  - Sistema de oleadas (`WaveSystem.hs`)
  - Sistema de torres y disparo (`TowerSystem.hs`)
  - Sistema de proyectiles y colisiones (`ProjectileSystem.hs`)
  - Servidor de red con JSON (`NetworkServer.hs`)
- **Cliente en Python**:
  - Renderizado con Pygame
  - Manejo de eventos y entrada del usuario
  - Comunicación por sockets TCP
  - Interfaz gráfica responsiva

---

## 🛠️ Tecnologías Utilizadas

### Lenguajes y Frameworks
- **Haskell** (GHC 8.8.4+) - Motor del juego
- **Python 3.10+** - Cliente gráfico
- **Pygame** - Renderizado y manejo de eventos

### Bibliotecas Haskell
```yaml
dependencies:
  - base >= 4.12 && < 4.15
  - aeson                    # Serialización JSON
  - bytestring              # Manejo de datos binarios
  - network                 # Comunicación por sockets
  - containers              # Estructuras de datos
  - linear                  # Operaciones vectoriales
```

### Bibliotecas Python
```bash
pygame>=2.5.0
```

---

## 📁 Estructura del Proyecto

```
defiende-la-torre/
├── src/
│   ├── Types.hs              # Tipos de datos del juego
│   ├── PathGeneration.hs     # Generación procedural de caminos
│   ├── EnemyMovement.hs      # Lógica de movimiento de enemigos
│   ├── WaveSystem.hs         # Sistema de oleadas
│   ├── TowerSystem.hs        # Sistema de torres y disparo
│   ├── ProjectileSystem.hs   # Sistema de proyectiles
│   ├── NetworkServer.hs      # Servidor TCP con JSON
│   └── MainServer.hs         # Punto de entrada del servidor
├── game_client.py            # Cliente Pygame (interfaz gráfica)
├── defiende-la-torre.cabal   # Configuración de Cabal
├── stack.yaml               # Configuración de Stack (opcional)
└── README.md                # Este archivo
```

---

## 🚀 Instalación y Ejecución

### Prerrequisitos

1. **Haskell Stack o Cabal**
   ```bash
   # Ubuntu/Debian
   sudo apt-get install haskell-stack
   
   # macOS
   brew install haskell-stack
   
   # O instalar Cabal directamente
   sudo apt-get install cabal-install ghc
   ```

2. **Python y Pygame**
   ```bash
   # Instalar Python 3.10+
   sudo apt-get install python3 python3-pip
   
   # Instalar Pygame
   pip3 install pygame
   ```

### Compilación y Ejecución

#### Opción 1: Usando Cabal

```bash
# 1. Clonar el repositorio
git clone https://github.com/tu-usuario/defiende-la-torre.git
cd defiende-la-torre

# 2. Compilar el servidor Haskell
cabal update
cabal build

# 3. En una terminal, ejecutar el servidor
cabal run

# 4. En otra terminal, ejecutar el cliente Python
python3 game_client.py
```

#### Opción 2: Usando Stack

```bash
# 1. Compilar con Stack
stack build

# 2. Ejecutar el servidor
stack run

# 3. En otra terminal, ejecutar el cliente
python3 game_client.py
```

---

## 🎮 Cómo Jugar

### Controles

| Tecla/Acción | Función |
|--------------|---------|
| **Click Izquierdo** | Colocar torre en la posición del cursor |
| **ESPACIO** | Iniciar oleada de enemigos |
| **1** | Seleccionar Torre Básica (100💰) |
| **2** | Seleccionar Torre Francotirador (150💰) |
| **3** | Seleccionar Torre Congelación (120💰) |
| **4** | Seleccionar Torre Explosiva (180💰) |
| **ESC** | Salir del juego |

### Objetivo del Juego

- **Defender tu base** del ataque de oleadas de enemigos
- **Colocar torres estratégicamente** para eliminar enemigos antes de que lleguen al final
- **Administrar tu oro** para construir torres efectivas
- **Sobrevivir el mayor número de oleadas** posible

### Consejos Estratégicos

1. 🎯 **Coloca torres al inicio del camino** para maximizar el tiempo de disparo
2. 💰 **Administra tu oro sabiamente** - no coloques todas las torres al inicio
3. 🔄 **Combina diferentes tipos de torres** para efectividad máxima
4. ❄️ **Usa torres de congelación** para ralentizar enemigos tanque
5. 💥 **Torres explosivas** son efectivas contra grupos de enemigos

---

## 🔧 Configuración Avanzada

### Modificar parámetros del juego

En `src/Types.hs` puedes ajustar:
```haskell
initialGameState :: Path -> GameState
initialGameState path =
  GameState
    { gsLives = 20        -- Vidas iniciales
    , gsGold = 200        -- Oro inicial
    , gsWave = 1          -- Oleada inicial
    -- ...
    }
```

En `src/WaveSystem.hs` puedes modificar:
```haskell
enemyStats :: EnemyType -> (Health, Double, Gold)
enemyStats Normal = (50, 30.0, 10)   -- HP, Velocidad, Recompensa
enemyStats Fast = (30, 60.0, 15)
enemyStats Tank = (150, 15.0, 25)
enemyStats Flying = (40, 50.0, 20)
```

En `src/TowerSystem.hs` puedes ajustar:
```haskell
towerCost :: TowerType -> Int
towerCost Basic  = 100
towerCost Sniper = 150
towerCost Freeze = 120
towerCost Splash = 180
```

### Cambiar el tipo de camino

En `src/PathGeneration.hs`, línea 11:
```haskell
-- Usar curva del dragón (complejo)
generatePath level = normalizePath $ generateDragonCurve level

-- O usar camino simple (para debugging)
generatePath level = generateSimplePath level
```

---

## 🌐 Protocolo de Comunicación JSON

### Comandos Cliente → Servidor

```json
{
  "type": "PlaceTower",
  "cmdX": 150.5,
  "cmdY": 200.3,
  "cmdType": "Sniper"
}
```

```json
{
  "type": "StartWave"
}
```

```json
{
  "type": "Tick",
  "cmdDeltaTime": 0.016
}
```

### Estado Servidor → Cliente

```json
{
  "rspPath": [[100, 80], [150, 80], ...],
  "rspEnemies": [
    {
      "edId": 1,
      "edType": "Normal",
      "edX": 150.5,
      "edY": 200.3,
      "edHP": 45,
      "edMaxHP": 50
    }
  ],
  "rspTowers": [
    {
      "tdX": 300,
      "tdY": 200,
      "tdType": "Sniper",
      "tdLevel": 1,
      "tdCooldown": 15,
      "tdRange": 250,
      "tdDamage": 50
    }
  ],
  "rspProjectiles": [...],
  "rspLives": 18,
  "rspMoney": 350,
  "rspWaveNumber": 3,
  "rspWaveActive": true
}
```

---

## 🤝 Uso de IA en el Desarrollo

Este proyecto utilizó asistencia de IA generativa para:

- ✅ Diseñar la estructura funcional del motor de oleadas
- ✅ Optimizar algoritmos de generación procedural de caminos
- ✅ Implementar el protocolo de comunicación JSON entre Haskell y Python
- ✅ Mejorar el diseño visual de la interfaz gráfica
- ✅ Generar ideas para mecánicas de juego y balanceo

### Ejemplos de prompts utilizados:
- *"Crea en Haskell una función pura que genere un patrón fractal de caminos para un juego tipo tower defense"*
- *"Modela en Haskell una estructura de datos inmutable para representar el estado de cada torre y enemigo"*
- *"Convierte el estado funcional del juego en un JSON interpretable por Pygame"*

---

## 📝 Principios de Programación Funcional Aplicados

### 🔹 Funciones Puras
Todas las funciones de la lógica del juego son deterministas y sin efectos secundarios:
```haskell
moveEnemyAlongPath :: Double -> Path -> Enemy -> Enemy
updateProjectiles :: GameState -> GameState
```

### 🔹 Inmutabilidad
El estado del juego nunca se modifica directamente, siempre se crean nuevas versiones:
```haskell
state { gsEnemies = newEnemies, gsGold = newGold }
```

### 🔹 Composición de Funciones
La lógica del juego se construye componiendo funciones simples:
```haskell
updateGame dt = updateProjectiles . updateTowers . moveEnemies dt
```

### 🔹 Recursión
Se usa recursión en lugar de loops para procesar listas:
```haskell
foldr (moveEnemy dt path) ([], 0) enemies
```

### 🔹 Evaluación Perezosa
Las listas infinitas y generación procedural aprovechan lazy evaluation:
```haskell
generateDragonCurve :: Int -> Path
```

---

## 🐛 Solución de Problemas

### El servidor no inicia
```bash
# Limpiar y reconstruir
cabal clean
cabal build
```

### El cliente no se conecta
- Verificar que el servidor esté corriendo en el puerto 3000
- Verificar que no haya firewall bloqueando el puerto
- Intentar con `localhost` o `127.0.0.1`

### No se ven los gráficos correctamente
```bash
# Reinstalar Pygame
pip3 uninstall pygame
pip3 install pygame --upgrade
```

### Errores de compilación en Haskell
```bash
# Actualizar dependencias
cabal update
cabal install --only-dependencies
```

---

## 📚 Recursos y Referencias

- [Documentación de Haskell](https://www.haskell.org/documentation/)
- [Pygame Documentation](https://www.pygame.org/docs/)
- [Aeson - JSON en Haskell](https://hackage.haskell.org/package/aeson)
- [Network.Socket](https://hackage.haskell.org/package/network)
- [Programación Funcional en Juegos](https://wiki.haskell.org/Game_Development)

---

## 👨‍💻 Autor

**Mauricio** - Proyecto de Programación Funcional

Universidad Tecnologica de la Mixteca - Séptimo Semestre

---

## 📄 Licencia

Este proyecto está bajo la Licencia BSD-3-Clause. Ver el archivo `LICENSE` para más detalles.

---

## 🎯 Trabajo Futuro

- [ ] Sistema de mejora de torres (upgrades)
- [ ] Más tipos de torres y enemigos
- [ ] Efectos de sonido y música
- [ ] Sistema de puntuación y leaderboard
- [ ] Multijugador cooperativo
- [ ] Guardar/cargar partidas
- [ ] Editor de mapas personalizado
- [ ] Achievements y desafíos diarios

---

<p align="center">
  <strong>⭐ Si te gustó el proyecto, dale una estrella en GitHub ⭐</strong>
</p>

<p align="center">
  Hecho con ❤️ usando Haskell y Python
</p>