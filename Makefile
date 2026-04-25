# Makefile for raylib-Ada (OpenBSD compatible)

RAYLIB_PATH ?= $(HOME)/.local
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

OBJECTS = $(patsubst %.adb,$(OBJ_DIR)/%.o,$(BODIES)) \
          $(patsubst %.ads,$(OBJ_DIR)/%.o,$(SPECS))

# Bibliothèque
LIBNAME = $(LIB_DIR)/libraylib_ada.a

.PHONY: all clean dirs test help

all: dirs $(LIBNAME)

dirs:
	@mkdir -p $(OBJ_DIR) $(LIB_DIR)

$(LIBNAME): $(OBJECTS)
	@echo "Creating static library..."
	$(AR) rcs $@ $(OBJECTS)
	$(RANLIB) $@
	@cp $(OBJ_DIR)/*.ali $(LIB_DIR)/ 2>/dev/null || true
	@echo "Library created: $@"

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.ads
	@echo "Compiling $<..."
	$(GCC) -c $(ADAFLAGS) $(INCLUDES) -o $@ $<

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.adb
	@echo "Compiling $<..."
	$(GCC) -c $(ADAFLAGS) $(INCLUDES) -o $@ $<

# Compilation du test
test: $(LIBNAME)
	@echo "Building test executable..."
	$(GNATMAKE) test.adb \
		$(ADAFLAGS) \
		-I$(SRC_DIR) \
		-aO$(LIB_DIR) \
		-D $(OBJ_DIR) \
		-o test \
		-largs \
		-L$(LIB_DIR) -lraylib_ada \
		$(LDFLAGS)
	@echo "Test executable created: ./test"

clean:
	rm -rf $(OBJ_DIR) $(LIB_DIR)
	rm -f test *.o *.ali b~*.ad?
	@echo "Cleaned build artifacts"

install: $(LIBNAME)
	install -d $(DESTDIR)/usr/local/lib
	install -m 644 $(LIBNAME) $(DESTDIR)/usr/local/lib/
	install -d $(DESTDIR)/usr/local/include/raylib-ada
	install -m 644 $(SRC_DIR)/*.ads $(DESTDIR)/usr/local/include/raylib-ada/
	install -d $(DESTDIR)/usr/local/lib/gnat
	install -m 644 $(LIB_DIR)/*.ali $(DESTDIR)/usr/local/lib/gnat/
	@echo "Installed to $(DESTDIR)/usr/local"

help:
	@echo "Targets disponibles :"
	@echo "  make          - Compile la bibliothèque"
	@echo "  make test     - Compile test.adb"
	@echo "  make clean    - Nettoie les fichiers générés"
	@echo "  make install  - Installe la bibliothèque"
