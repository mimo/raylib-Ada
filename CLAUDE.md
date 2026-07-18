# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Prerequisites

- GNAT Ada toolsuite
- Raylib 5.5 (see setup below)

## Build & Run

```bash
# Copy and edit config first (set RAYLIB_PATH)
cp config.example config

# Auto-install Raylib 5.5 into ~/.local (then set RAYLIB_PATH="$HOME/.local" in config)
./build.sh raylib

# Build the library + test binary + examples
./build.sh

# Build only the library
gprbuild -P raylib.gpr

# Build and run the test program
gprbuild -P raylib-test.gpr -XRAYLIB_PATH="$RAYLIB_PATH"
./test_rlada

# Build examples
gprbuild -P examples/raylib-examples.gpr -XRAYLIB_PATH="$RAYLIB_PATH"

# Clean all build artifacts
./build.sh clean

# Dump Ada spec from the installed raylib.h (useful when porting new bindings)
./build.sh edit
```

The library builds as a static archive (`lib/libraylibada.a`) targeting `-gnat12 -O2`.

## Architecture

This is an Ada binding to the [Raylib](https://www.raylib.com/) C library. The binding wraps C functions with Ada-idiomatic interfaces (Ada strings instead of `char_array`, `Boolean` instead of `int`, etc.).

**Package structure:**

- `Raylib` (`src/raylib.ads/.adb`) — root package. Declares all shared C-interop types (`Vector2`, `Vector3`, `Color`, `Rectangle`, `Font`, `Image`, `Texture2D`, `Camera3D`, `Wave`, `Sound`, etc.) and nested child packages:
  - `Raylib.Textures` — image and texture loading
  - `Raylib.Text` — font loading and text drawing (`Draw`, `Draw_Pro`, `Measure`, `Get_Font_Default`)
  - `Raylib.Shapes` — shape drawing primitives (`Draw_Line`, `Draw_Line_V`, `Draw_Line_Ex`, etc.)
  - `Raylib.Input` — keyboard, mouse, gamepad queries (`Is_Key_Pressed`, `Is_Mouse_Button_Down`, etc.)
- `Raylib.Window` (`src/raylib-window.ads/.adb`) — window lifecycle, drawing loop (`Init`, `Begin_Drawing`, `End_Drawing`, `Should_Close`), camera modes, timing (`Set_Target_FPS`, `Get_Frame_Time`), and cursor management.
- `Raylib.Audio` (`src/raylib-audio.ads/.adb`) — audio device, `Wave`, `Sound`, and `Music` management.
- `Raylib.UI` (`src/raylib-ui.ads/.adb`) — immediate-mode GUI widgets (wrapping raygui). Includes a theme system (`Load`/`Unload`), global control state (`Set_State`, `Set_Alpha`, `Lock`/`Unlock`), and widgets: `Button`, `Label`, `Toggle`, `Checkbox`, `Textbox`, `Textbox_Multi`, `Panel`, `Statusbar`.
- `Raylib.Extras` (`src/raylib-extras.ads`) — currently empty, reserved for future helpers.

**C interop pattern:**

Functions that take Ada `String` parameters have a thin private wrapper that converts `String → char_array` via `Interfaces.C.To_C`, then calls the raw imported C function. Functions that can be imported directly (no string arguments) use `with Import, Convention => C, External_Name => "..."` inline in the spec. Records passed to C use `Convention => C_Pass_By_Copy`.

**test.adb** at the repository root is the interactive test harness — it exercises Lines, Perspective, and GUI modes switchable with `[TAB]`.

**`config`** (git-ignored shell fragment) is sourced by `build.sh` and must define `RAYLIB_PATH`. Use `config.example` as the template.
