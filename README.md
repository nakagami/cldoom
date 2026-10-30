# CL-DOOM for ExLisp

A terminal-based 3D raycasting FPS (DOOM) running on **ExLisp** (Elixir / BEAM).

## Features

- **3D Raycasting Rendering Engine**: Fast pseudo-3D wall, floor, ceiling, and lighting rendering powered by the DDA (Digital Differential Analyzer) algorithm
- **Billboard Sprites**: Monsters (Imp, Zombie, Cacodemon), explosive barrels, and items (Medkit, Armor, Ammo, Shotgun, BFG)
- **Combat & Weapon System**:
  - `[1]` Fist
  - `[2]` Pistol
  - `[3]` Shotgun (with muzzle flash rendering)
  - `[4]` BFG 9000 (powerful energy weapon)
- **Enemy AI**: Line-of-sight and distance-based target detection, chasing, and attacking
- **Explosive Barrels**: Explode upon taking damage, dealing area-of-effect damage to nearby enemies and the player
- **DOOM Status Bar (HUD)**:
  - Health (HP), Armor (ARM), dynamic DOOMGUY face (`^_^`, `Ò_Ó`, `>_<`, `•д•`, `x_x`), ammo count, equipped weapon, and kill count
- **Automap (Minimap)**: Real-time radar display showing explored areas, enemies, and items
- **3 Episodes / Maps**:
  - E1M1: HANGAR
  - E1M2: NUCLEAR PLANT
  - E1M8: PHOBOS ANOMALY (Boss stage)

## Requirements

- Elixir / Erlang
- ExLisp (`mix escript.install hex exlisp` or `~/.mix/escripts/exlisp`)

## How to Run

```bash
./doom
```

## Controls

| Key | Action |
| :--- | :--- |
| `W` / `S` / `↑` / `↓` | Move forward / backward |
| `A` / `D` | Strafe left / right |
| `Q` / `E` / `←` / `→` | Turn left / right |
| `Space` / `F` | Fire weapon |
| `1` / `2` / `3` / `4` | Select weapon (Fist / Pistol / Shotgun / BFG) |
| `M` | Toggle automap (minimap) |
| `X` | Quit game |
