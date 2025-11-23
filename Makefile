CC ?= gcc
CFLAGS ?= -std=c99 -Wall -D_CP_RUNTIME_CHECKS -D_GNU_SOURCE

RUNTIME_SRC := examples/runtime.c
OUT := build/metc
RUNTIME_OBJ := build/runtime.o

SRCS := \
	src/main.c \
	src/preprocessor.c \
	src/lexer.c \
	src/parser.c \
	src/parser_expr.c \
	src/tac_ir_gen.c

ifeq ($(OS),Windows_NT)
	MKDIR_CMD = if not exist build mkdir build
	RM_CMD = if exist build rmdir /s /q build
else
	MKDIR_CMD = mkdir -p build
	RM_CMD = rm -rf build
endif

.PHONY: all build clean

all: build

build:
	@echo "Creating build dir..."
	@$(MKDIR_CMD)
	@echo "Compiling runtime..."
	@$(CC) -c $(RUNTIME_SRC) -o $(RUNTIME_OBJ)
	@echo "Compiling metc..."
	@$(CC) $(CFLAGS) -ggdb $(SRCS) -o $(OUT)
	@echo "Built $(OUT)"

clean:
	@echo "Removing build dir..."
	@$(RM_CMD)
