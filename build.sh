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
    # Nettoyer avec gprbuild si disponible
    if command -v gprclean >/dev/null 2>&1; then
      gprclean -P raylib.gpr 2>/dev/null || true
      gprclean -P raylib-test.gpr 2>/dev/null || true
      gprclean -P examples/raylib-examples.gpr 2>/dev/null || true
    fi
    # Nettoyer avec make (pour OpenBSD)
    make clean 2>/dev/null || true
    echo "Nettoyage terminé"
    ;;

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

  openbsd)
    echo "==> Compilation pour OpenBSD"
    echo "    Compilateur: egcc"
    echo "    RAYLIB_PATH: $RAYLIB_PATH"

    # Créer les répertoires
    mkdir -p obj lib

    # Flags de compilation
    ADAFLAGS="-gnat2012 -gnatwa"
    INCLUDES="-Isrc -I$RAYLIB_PATH/include"

    echo ""
    echo "==> Compilation des sources Ada..."

    # Compiler tous les .adb dans src/ (dans l'ordre alphabétique)
    # Les dépendances seront résolues par gnatmake plus tard
    for adb_file in src/*.adb; do
      if [ -f "$adb_file" ]; then
        module=$(basename "$adb_file" .adb)
        echo "  Compiling ${module}..."
        egcc -c $ADAFLAGS $INCLUDES -o obj/${module}.o "$adb_file" || {
          echo "Erreur lors de la compilation de ${module}"
          exit 1
        }
      fi
    done

    echo ""
    echo "==> Création de la bibliothèque statique..."
    ar rcs lib/libraylib_ada.a obj/*.o
    ranlib lib/libraylib_ada.a
    cp obj/*.ali lib/ 2>/dev/null || true

    echo ""
    echo "==> Compilation de test.adb..."
    gnatmake test.adb \
      $ADAFLAGS \
      -Isrc \
      -aOlib \
      -aIlib \
      -D obj \
      -o test \
      -largs \
      -Llib -lraylib_ada \
      -L$RAYLIB_PATH/lib -lraylib -lm

    echo ""
    echo "==> Build terminé !"
    echo "    Bibliothèque: lib/libraylib_ada.a"
    echo "    Exécutable:   ./test"
    echo ""
    echo "Pour exécuter: ./test"
    ;;

  *)
    gprbuild -v -p -P raylib.gpr
    gprbuild -v -p -P raylib-test.gpr -XRAYLIB_PATH="$RAYLIB_PATH"
    gprbuild -v -p -P examples/raylib-examples.gpr -XRAYLIB_PATH="$RAYLIB_PATH"
esac
