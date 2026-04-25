# Makefile for raylib-Ada (compatible BSD/GNU make)

# Installation prefix
PREFIX ?= $(HOME)/.local
RAYLIB_PATH ?= $(PREFIX)

# Build directories
SRC_DIR = src
OBJ_DIR = obj
LIB_DIR = lib

# Compilateur
GCC = gcc
AR = ar
RANLIB = ranlib
GNATMAKE = gnatmake

# Flags (Ada 2012 pour compatibilité OpenBSD)
ADAFLAGS = -gnat2012 -gnatwae
INCLUDES = -I$(SRC_DIR) -I$(RAYLIB_PATH)/include
LDFLAGS = -L$(RAYLIB_PATH)/lib -lraylib -lm

# Liste explicite des sources (dans l'ordre de dépendance)
ADS_FILES = \
	$(SRC_DIR)/raylib.ads \
	$(SRC_DIR)/raylib-colors.ads \
	$(SRC_DIR)/raylib-window.ads \
	$(SRC_DIR)/raylib-shapes.ads \
	$(SRC_DIR)/raylib-text.ads \
	$(SRC_DIR)/raylib-input.ads \
	$(SRC_DIR)/raylib-utils.ads \
	$(SRC_DIR)/raylib-ui.ads

ADB_FILES = \
	$(SRC_DIR)/raylib.adb \
	$(SRC_DIR)/raylib-colors.adb \
	$(SRC_DIR)/raylib-window.adb \
	$(SRC_DIR)/raylib-shapes.adb \
	$(SRC_DIR)/raylib-text.adb \
	$(SRC_DIR)/raylib-input.adb \
	$(SRC_DIR)/raylib-utils.adb \
	$(SRC_DIR)/raylib-ui.adb

# Fichiers objets correspondants
OBJ_FILES = \
	$(OBJ_DIR)/raylib.o \
	$(OBJ_DIR)/raylib-colors.o \
	$(OBJ_DIR)/raylib-window.o \
	$(OBJ_DIR)/raylib-shapes.o \
	$(OBJ_DIR)/raylib-text.o \
	$(OBJ_DIR)/raylib-input.o \
	$(OBJ_DIR)/raylib-utils.o \
	$(OBJ_DIR)/raylib-ui.o

# Bibliothèque
LIBNAME = $(LIB_DIR)/libraylib_ada.a

.PHONY: all clean test help debug

all: $(LIBNAME)

# Créer les répertoires
$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

$(LIB_DIR):
	mkdir -p $(LIB_DIR)

# Compiler les .ads
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.ads | $(OBJ_DIR)
	@echo "Compiling $<..."
	$(GCC) -c $(ADAFLAGS) $(INCLUDES) -o $@ $<

# Compiler les .adb
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.adb | $(OBJ_DIR)
	@echo "Compiling $<..."
	$(GCC) -c $(ADAFLAGS) $(INCLUDES) -o $@ $<

# Créer la bibliothèque
$(LIBNAME): $(OBJ_FILES) | $(LIB_DIR)
	@echo "Creating static library $@..."
	$(AR) rcs $@ $(OBJ_FILES)
	$(RANLIB) $@
	@echo "Copying .ali files..."
	@cp $(OBJ_DIR)/*.ali $(LIB_DIR)/ 2>/dev/null || true
	@echo "Library created successfully: $@"
	@ls -lh $@

# Compilation du test
test: $(LIBNAME)
	@echo "Building test executable..."
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
	@echo "Success! Run with: ./test"

clean:
	@echo "Cleaning build artifacts..."
	rm -rf $(OBJ_DIR) $(LIB_DIR)
	rm -f test *.o *.ali b~*.ad?
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
	@echo "Available targets:"
	@echo "  make          - Build the library"
	@echo "  make test     - Build test.adb"
	@echo "  make clean    - Clean build artifacts"
	@echo "  make install  - Install to PREFIX (default: $(PREFIX))"
	@echo "  make debug    - Show Makefile variables"
	@echo ""
	@echo "Variables:"
	@echo "  PREFIX        - Installation prefix (default: $(PREFIX))"
	@echo "  RAYLIB_PATH   - Raylib location (default: $(RAYLIB_PATH))"

debug:
	@echo "=== Makefile Variables ==="
	@echo "PREFIX:      $(PREFIX)"
	@echo "RAYLIB_PATH: $(RAYLIB_PATH)"
	@echo "SRC_DIR:     $(SRC_DIR)"
	@echo "OBJ_DIR:     $(OBJ_DIR)"
	@echo "LIB_DIR:     $(LIB_DIR)"
	@echo "LIBNAME:     $(LIBNAME)"
	@echo ""
	@echo "=== Source Files ($(words $(ADS_FILES)) .ads + $(words $(ADB_FILES)) .adb) ==="
	@echo "$(ADS_FILES)" | tr ' ' '\n' | head -5
	@echo "..."
	@echo ""
	@echo "=== Object Files ($(words $(OBJ_FILES)) total) ==="
	@echo "$(OBJ_FILES)" | tr ' ' '\n' | head -5
	@echo "..."
	@echo ""
	@echo "=== Current Directory ==="
	@pwd
	@ls -la | grep "^d" | head -10
