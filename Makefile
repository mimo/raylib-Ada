# Makefile for raylib-Ada (BSD make compatible)

# Installation prefix
PREFIX ?= ${HOME}/.local
RAYLIB_PATH ?= ${PREFIX}

# Build directories
SRC_DIR = src
OBJ_DIR = obj
LIB_DIR = lib

# Compilateur (OpenBSD utilise egcc)
GCC ?= egcc
AR ?= ar
RANLIB ?= ranlib
GNATMAKE ?= gnatmake

# Flags (Ada 2012 pour compatibilité OpenBSD)
ADAFLAGS = -gnat2012 -gnatwae
INCLUDES = -I${SRC_DIR} -I${RAYLIB_PATH}/include
LDFLAGS = -L${RAYLIB_PATH}/lib -lraylib -lm

# Sources dans l'ordre de compilation
SOURCES = \
	raylib \
	raylib-colors \
	raylib-window \
	raylib-shapes \
	raylib-text \
	raylib-input \
	raylib-utils \
	raylib-ui

# Bibliothèque
LIBNAME = ${LIB_DIR}/libraylib_ada.a

.PHONY: all clean test help debug install

all: ${LIBNAME}

# Créer les répertoires
${OBJ_DIR}:
	mkdir -p ${OBJ_DIR}

${LIB_DIR}:
	mkdir -p ${LIB_DIR}

# Compiler chaque module (BSD make style)
${OBJ_DIR}/raylib.o: ${SRC_DIR}/raylib.ads ${SRC_DIR}/raylib.adb
	@test -d ${OBJ_DIR} || mkdir -p ${OBJ_DIR}
	@echo "Compiling raylib..."
	${GCC} -c ${ADAFLAGS} ${INCLUDES} -o ${OBJ_DIR}/raylib.o ${SRC_DIR}/raylib.adb

${OBJ_DIR}/raylib-colors.o: ${SRC_DIR}/raylib-colors.ads ${SRC_DIR}/raylib-colors.adb ${OBJ_DIR}/raylib.o
	@test -d ${OBJ_DIR} || mkdir -p ${OBJ_DIR}
	@echo "Compiling raylib-colors..."
	${GCC} -c ${ADAFLAGS} ${INCLUDES} -o ${OBJ_DIR}/raylib-colors.o ${SRC_DIR}/raylib-colors.adb

${OBJ_DIR}/raylib-window.o: ${SRC_DIR}/raylib-window.ads ${SRC_DIR}/raylib-window.adb ${OBJ_DIR}/raylib.o
	@test -d ${OBJ_DIR} || mkdir -p ${OBJ_DIR}
	@echo "Compiling raylib-window..."
	${GCC} -c ${ADAFLAGS} ${INCLUDES} -o ${OBJ_DIR}/raylib-window.o ${SRC_DIR}/raylib-window.adb

${OBJ_DIR}/raylib-shapes.o: ${SRC_DIR}/raylib-shapes.ads ${SRC_DIR}/raylib-shapes.adb ${OBJ_DIR}/raylib.o
	@test -d ${OBJ_DIR} || mkdir -p ${OBJ_DIR}
	@echo "Compiling raylib-shapes..."
	${GCC} -c ${ADAFLAGS} ${INCLUDES} -o ${OBJ_DIR}/raylib-shapes.o ${SRC_DIR}/raylib-shapes.adb

${OBJ_DIR}/raylib-text.o: ${SRC_DIR}/raylib-text.ads ${SRC_DIR}/raylib-text.adb ${OBJ_DIR}/raylib.o
	@test -d ${OBJ_DIR} || mkdir -p ${OBJ_DIR}
	@echo "Compiling raylib-text..."
	${GCC} -c ${ADAFLAGS} ${INCLUDES} -o ${OBJ_DIR}/raylib-text.o ${SRC_DIR}/raylib-text.adb

${OBJ_DIR}/raylib-input.o: ${SRC_DIR}/raylib-input.ads ${SRC_DIR}/raylib-input.adb ${OBJ_DIR}/raylib.o
	@test -d ${OBJ_DIR} || mkdir -p ${OBJ_DIR}
	@echo "Compiling raylib-input..."
	${GCC} -c ${ADAFLAGS} ${INCLUDES} -o ${OBJ_DIR}/raylib-input.o ${SRC_DIR}/raylib-input.adb

${OBJ_DIR}/raylib-utils.o: ${SRC_DIR}/raylib-utils.ads ${SRC_DIR}/raylib-utils.adb ${OBJ_DIR}/raylib.o
	@test -d ${OBJ_DIR} || mkdir -p ${OBJ_DIR}
	@echo "Compiling raylib-utils..."
	${GCC} -c ${ADAFLAGS} ${INCLUDES} -o ${OBJ_DIR}/raylib-utils.o ${SRC_DIR}/raylib-utils.adb

${OBJ_DIR}/raylib-ui.o: ${SRC_DIR}/raylib-ui.ads ${SRC_DIR}/raylib-ui.adb ${OBJ_DIR}/raylib.o ${OBJ_DIR}/raylib-colors.o ${OBJ_DIR}/raylib-text.o ${OBJ_DIR}/raylib-shapes.o ${OBJ_DIR}/raylib-input.o
	@test -d ${OBJ_DIR} || mkdir -p ${OBJ_DIR}
	@echo "Compiling raylib-ui..."
	${GCC} -c ${ADAFLAGS} ${INCLUDES} -o ${OBJ_DIR}/raylib-ui.o ${SRC_DIR}/raylib-ui.adb

# Créer la bibliothèque
${LIBNAME}: ${OBJ_DIR}/raylib.o ${OBJ_DIR}/raylib-colors.o ${OBJ_DIR}/raylib-window.o ${OBJ_DIR}/raylib-shapes.o ${OBJ_DIR}/raylib-text.o ${OBJ_DIR}/raylib-input.o ${OBJ_DIR}/raylib-utils.o ${OBJ_DIR}/raylib-ui.o
	@test -d ${LIB_DIR} || mkdir -p ${LIB_DIR}
	@echo "Creating static library ${LIBNAME}..."
	${AR} rcs ${LIBNAME} ${OBJ_DIR}/*.o
	${RANLIB} ${LIBNAME}
	@echo "Copying .ali files..."
	@cp ${OBJ_DIR}/*.ali ${LIB_DIR}/ 2>/dev/null || true
	@echo "Library created successfully!"
	@ls -lh ${LIBNAME}

# Compilation du test
test: ${LIBNAME}
	@echo "Building test executable..."
	${GNATMAKE} test.adb \
		${ADAFLAGS} \
		-I${SRC_DIR} \
		-aO${LIB_DIR} \
		-aI${LIB_DIR} \
		-D ${OBJ_DIR} \
		-o test \
		-largs \
		-L${LIB_DIR} -lraylib_ada \
		${LDFLAGS}
	@echo "Success! Run with: ./test"

clean:
	@echo "Cleaning build artifacts..."
	rm -rf ${OBJ_DIR} ${LIB_DIR}
	rm -f test *.o *.ali b~*.ad?
	@echo "Clean complete"

install: ${LIBNAME}
	@echo "Installing to ${PREFIX}..."
	install -d ${PREFIX}/lib
	install -m 644 ${LIBNAME} ${PREFIX}/lib/
	install -d ${PREFIX}/include/raylib-ada
	install -m 644 ${SRC_DIR}/*.ads ${PREFIX}/include/raylib-ada/
	install -d ${PREFIX}/lib/gnat
	install -m 644 ${LIB_DIR}/*.ali ${PREFIX}/lib/gnat/
	@echo "Installation complete"

help:
	@echo "Available targets:"
	@echo "  make          - Build the library"
	@echo "  make test     - Build test.adb"
	@echo "  make clean    - Clean build artifacts"
	@echo "  make install  - Install to PREFIX (default: ${PREFIX})"
	@echo ""
	@echo "Variables:"
	@echo "  PREFIX        - Installation prefix (default: ${PREFIX})"
	@echo "  RAYLIB_PATH   - Raylib location (default: ${RAYLIB_PATH})"

debug:
	@echo "=== Makefile Variables ==="
	@echo "PREFIX:      ${PREFIX}"
	@echo "RAYLIB_PATH: ${RAYLIB_PATH}"
	@echo "SRC_DIR:     ${SRC_DIR}"
	@echo "OBJ_DIR:     ${OBJ_DIR}"
	@echo "LIB_DIR:     ${LIB_DIR}"
	@echo "LIBNAME:     ${LIBNAME}"
	@echo ""
	@echo "=== Current Directory ==="
	@pwd
	@echo ""
	@echo "=== Source Files ==="
	@ls -1 ${SRC_DIR}/*.ad[sb] 2>/dev/null | head -10 || echo "No source files found"
