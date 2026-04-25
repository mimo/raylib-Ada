# Makefile for raylib-Ada (OpenBSD compatible)

# Installation prefix
PREFIX ?= $(HOME)/.local
RAYLIB_PATH ?= $(PREFIX)

# Build directories
SRC_DIR = src
OBJ_DIR = obj
LIB_DIR = lib

# Compilateur
GNATMAKE = gnatmake
GCC = gcc
AR = ar
RANLIB = ranlib

# Flags (Ada 2012 pour compatibilité OpenBSD)
ADAFLAGS = -gnat2012 -gnatwae
INCLUDES = -I$(SRC_DIR) -I$(RAYLIB_PATH)/include
LDFLAGS = -L$(RAYLIB_PATH)/lib -lraylib -lm

# Sources (ordre important pour compilation)
SPECS = raylib.ads raylib-colors.ads raylib-window.ads \
        raylib-shapes.ads raylib-text.ads raylib-input.ads \
        raylib-utils.ads raylib-ui.ads

BODIES = raylib.adb raylib-colors.adb raylib-window.adb \
         raylib-shapes.adb raylib-text.adb raylib-input.adb \
         raylib-utils.adb raylib-ui.adb

# Ajouter le préfixe SRC_DIR
SPEC_SRCS = $(addprefix $(SRC_DIR)/,$(SPECS))
BODY_SRCS = $(addprefix $(SRC_DIR)/,$(BODIES))

OBJECTS = $(patsubst $(SRC_DIR)/%.adb,$(OBJ_DIR)/%.o,$(BODY_SRCS)) \
          $(patsubst $(SRC_DIR)/%.ads,$(OBJ_DIR)/%.o,$(SPEC_SRCS))

# Bibliothèque
LIBNAME = $(LIB_DIR)/libraylib_ada.a

.PHONY: all clean dirs test help

all: dirs $(LIBNAME)

# Créer les répertoires (seulement si nécessaire)
dirs:
	@test -d $(OBJ_DIR) || mkdir -p $(OBJ_DIR)
	@test -d $(LIB_DIR) || mkdir -p $(LIB_DIR)

$(LIBNAME): $(OBJECTS)
	@echo "Creating static library..."
	@echo "Target: $@"
	@echo "Objects: $(OBJECTS)"
	@test -d $(LIB_DIR) || mkdir -p $(LIB_DIR)
	@ls -la $(OBJ_DIR)/ | head -20
	$(AR) rcs $@ $(OBJECTS)
	$(RANLIB) $@
	@cp $(OBJ_DIR)/*.ali $(LIB_DIR)/ 2>/dev/null || true
	@echo "Library created: $@"
	@ls -la $(LIB_DIR)/

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.ads
	@echo "Compiling $<..."
	@test -d $(OBJ_DIR) || mkdir -p $(OBJ_DIR)
	$(GCC) -c $(ADAFLAGS) $(INCLUDES) -o $@ $<

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.adb
	@echo "Compiling $<..."
	@test -d $(OBJ_DIR) || mkdir -p $(OBJ_DIR)
	$(GCC) -c $(ADAFLAGS) $(INCLUDES) -o $@ $<

# Compilation du test
test: $(LIBNAME)
	@echo "Building test executable..."
	@echo "Library location: $(LIBNAME)"
	@echo "LIB_DIR: $(LIB_DIR)"
	@ls -la $(LIB_DIR)/ || echo "LIB_DIR not found!"
	$(GNATMAKE) -v test.adb \
		$(ADAFLAGS) \
		-I$(SRC_DIR) \
		-aO$(LIB_DIR) \
		-aI$(LIB_DIR) \
		-D $(OBJ_DIR) \
		-o test \
		-largs \
		-L$(LIB_DIR) -lraylib_ada \
		$(LDFLAGS)
	@echo "Test executable created: ./test"

clean:
	@echo "Cleaning build artifacts..."
	@rm -rf $(OBJ_DIR) $(LIB_DIR)
	@rm -f test *.o *.ali b~*.ad?
	@echo "Clean complete"

install: $(LIBNAME)
	@echo "Installing to $(PREFIX)..."
	install -d $(PREFIX)/lib
	install -m 644 $(LIBNAME) $(PREFIX)/lib/
	install -d $(PREFIX)/include/raylib-ada
	install -m 644 $(SRC_DIR)/*.ads $(PREFIX)/include/raylib-ada/
	install -d $(PREFIX)/lib/gnat
	install -m 644 $(LIB_DIR)/*.ali $(PREFIX)/lib/gnat/
	@echo "Installation complete"

help:
	@echo "Targets disponibles :"
	@echo "  make          - Compile la bibliothèque"
	@echo "  make test     - Compile test.adb"
	@echo "  make clean    - Nettoie les fichiers générés"
	@echo "  make install  - Installe dans PREFIX (défaut: $(PREFIX))"
	@echo "  make debug    - Affiche les variables du Makefile"
	@echo ""
	@echo "Variables :"
	@echo "  PREFIX        - Préfixe d'installation (défaut: $(PREFIX))"
	@echo "  RAYLIB_PATH   - Chemin de raylib (défaut: $(RAYLIB_PATH))"

debug:
	@echo "=== Makefile Variables ==="
	@echo "PREFIX:      $(PREFIX)"
	@echo "RAYLIB_PATH: $(RAYLIB_PATH)"
	@echo "SRC_DIR:     $(SRC_DIR)"
	@echo "OBJ_DIR:     $(OBJ_DIR)"
	@echo "LIB_DIR:     $(LIB_DIR)"
	@echo "LIBNAME:     $(LIBNAME)"
	@echo "CURDIR:      $(CURDIR)"
	@echo ""
	@echo "=== Directory Structure ==="
	@ls -la . | grep "^d"
	@echo ""
	@echo "=== Objects ==="
	@echo "$(OBJECTS)" | tr ' ' '\n' | head -10
