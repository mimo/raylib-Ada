#!/bin/sh

set -e
cd $(dirname $0)
. "`pwd`/config"

# Fonction de téléchargement portable (OpenBSD/Linux)
download_file() {
  url="$1"
  output="$2"

  if command -v ftp >/dev/null 2>&1; then
    # OpenBSD ftp supporte HTTP/HTTPS
    ftp -o "$output" "$url"
  elif command -v curl >/dev/null 2>&1; then
    curl -L -o "$output" "$url"
  elif command -v wget >/dev/null 2>&1; then
    wget -O "$output" "$url"
  else
    echo "Erreur: aucun outil de téléchargement trouvé (ftp, curl, wget)"
    exit 1
  fi
}

case $1 in
  clean)
    gprclean -P raylib.gpr
    gprclean -P raylib-test.gpr
    gprclean -P examples/raylib-examples.gpr;;

  edit)
    gcc -fdump-ada-spec "$RAYLIB_PATH/include/raylib.h"
    $EDITOR;;

  raylib)
    RAYLIB_VERSION="5.6"
    RAYLIB_DIR="raylib-${RAYLIB_VERSION}"
    RAYLIB_ARCHIVE="${RAYLIB_VERSION}.tar.gz"
    RAYLIB_URL="https://github.com/raysan5/raylib/archive/refs/tags/${RAYLIB_VERSION}.tar.gz"
    INSTALL_PREFIX="${INSTALL_PREFIX:-$HOME/.local}"

    echo "==> Installation de raylib ${RAYLIB_VERSION}"

    # Télécharger si pas déjà présent
    if [ ! -f "$RAYLIB_ARCHIVE" ]; then
      echo "==> Téléchargement depuis GitHub..."
      download_file "$RAYLIB_URL" "$RAYLIB_ARCHIVE"
    else
      echo "==> Archive déjà téléchargée"
    fi

    # Extraire
    if [ ! -d "$RAYLIB_DIR" ]; then
      echo "==> Extraction de l'archive..."
      tar xzf "$RAYLIB_ARCHIVE"
    else
      echo "==> Sources déjà extraites"
    fi

    # Compiler
    echo "==> Compilation de raylib..."
    cd "$RAYLIB_DIR"

    # Créer le dossier build
    mkdir -p build
    cd build

    # Configuration avec cmake
    cmake .. \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
      -DBUILD_SHARED_LIBS=ON \
      -DBUILD_EXAMPLES=OFF

    # Compilation
    make -j$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 2)

    # Installation
    echo "==> Installation dans $INSTALL_PREFIX"
    make install

    echo ""
    echo "==> Raylib ${RAYLIB_VERSION} installé avec succès !"
    echo "    Bibliothèque : $INSTALL_PREFIX/lib"
    echo "    En-têtes     : $INSTALL_PREFIX/include"
    echo ""
    echo "Pour utiliser raylib avec ce projet, mettez à jour config :"
    echo "    RAYLIB_PATH=\"$INSTALL_PREFIX\""
    ;;

  *)
    gprbuild -v -p -P raylib.gpr
    gprbuild -v -p -P raylib-test.gpr -XRAYLIB_PATH="$RAYLIB_PATH"
    gprbuild -v -p -P examples/raylib-examples.gpr -XRAYLIB_PATH="$RAYLIB_PATH"
esac
