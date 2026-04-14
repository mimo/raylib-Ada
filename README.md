# raylib-Ada

Ada binding for the Raylib library.
Many functions are missing and some are not tested !

## Prerequisites

- GNAT toolsuite : [www.adacore.com](https://www.adacore.com/community)
- Raylib 5.6 : [raysan5/raylib](https://github.com/raysan5/raylib)

## Installation rapide

### Option 1 : Installation automatique de Raylib (recommandé)

Le script `build.sh` peut télécharger et compiler Raylib automatiquement :

```bash
# Télécharge, compile et installe raylib 5.6 dans ~/.local
./build.sh raylib

# Copier et configurer
cp config.example config
# Éditer 'config' et définir RAYLIB_PATH="$HOME/.local"

# Compiler le binding Ada
./build.sh
```

### Option 2 : Installation manuelle de Raylib

Installer Raylib depuis votre gestionnaire de paquets ou depuis les sources :

```bash
# Debian/Ubuntu
sudo apt install libraylib-dev

# macOS
brew install raylib

# OpenBSD
doas pkg_add raylib

# Ou compiler depuis les sources
git clone https://github.com/raysan5/raylib.git
cd raylib
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$HOME/.local
make && make install
```

Puis configurer le projet :

```bash
cp config.example config
# Éditer 'config' selon votre installation
./build.sh
```

## Development

Porting examples (full list [here](https://www.raylib.com/examples.html)) :

- audio
  - [x] sound loading and playing
- core
  - [x] input gamepad
  - [ ] input keys
  - [ ] input mouse
  - [ ] input mouse_wheel
  - [x] 3d camera first person
  - [ ] 2d camera
- shapes
  - [ ] collision area
  - [ ] draw ring
  - [ ] draw rectangle_rounded
- textures
  - [x] rectangle
  - [ ] srcrec dstrec
  - [ ] sprite button
- text
  - [ ] format text
  - [x] input box
  - [ ] rectangle bounds
- models
  - [ ] box collisions
