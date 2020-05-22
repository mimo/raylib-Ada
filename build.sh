#!/bin/sh

set -e
cd $(dirname $0)
. `pwd`/config

case $1 in
  clean)
    gprclean -P raylib.gpr
    gprclean -P raylib-test.gpr
    gprclean -P examples/raylib-examples.gpr;;
  edit)
    gcc -fdump-ada-spec "$RAYLIB_PATH/include/raylib.h"
    $EDITOR;;
  *)
    gprbuild -v -p -P raylib.gpr
    gprbuild -v -p -P raylib-test.gpr -XRAYLIB_PATH="$RAYLIB_PATH"
    gprbuild -v -p -P examples/raylib-examples.gpr -XRAYLIB_PATH="$RAYLIB_PATH"
esac
