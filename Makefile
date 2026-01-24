CC ?= gcc
CFLAGS ?= -std=c99 -D_GNU_SOURCE
SRCS := \
src/main.c \
src/preprocessor.c \
src/lexer.c \
src/parser.c \
src/parser_expr.c \
src/tac_ir_dump.c \
src/tac_ir_gen.c

ifeq ($(OS),Windows_NT)
EXE := .exe
else
EXE :=
endif

DEBUG_DIR := build/debug
RELEASE_DIR := build/release
OUT_DEBUG := $(DEBUG_DIR)/m7c$(EXE)
OUT_RELEASE := $(RELEASE_DIR)/m7c$(EXE)

ifeq ($(OS),Windows_NT)
ifdef COMSPEC
# For Windows cmd.exe
define MKDIR
@if not exist $(1) mkdir $(1)
endef
define RM
@if exist $(1) rmdir /s /q $(1)
endef
else
# For Unix-like shell on Windows (e.g., Git Bash)
define MKDIR
@mkdir -p $(1)
endef
define RM
@rm -rf $(1)
endef
endif
else
# For Unix-like systems
define MKDIR
@mkdir -p $(1)
endef
define RM
@rm -rf $(1)
endef
endif

.PHONY: all debug release clean

all: debug

debug: $(OUT_DEBUG)

$(OUT_DEBUG): $(SRCS) | $(DEBUG_DIR)
	@echo "Compiling metc (debug)..."
	@$(CC) $(CFLAGS) -ggdb -O0 -D_CP_RUNTIME_CHECKS $(SRCS) -o $@
	@echo "Built $@"

release: $(OUT_RELEASE)

$(OUT_RELEASE): $(SRCS) | $(RELEASE_DIR)
	@echo "Compiling metc (release)..."
	@$(CC) $(CFLAGS) -O3 -DNDEBUG $(SRCS) -o $@
	@echo "Built $@"

$(DEBUG_DIR):
	$(call MKDIR,$@)

$(RELEASE_DIR):
	$(call MKDIR,$@)

clean:
	@echo "Removing build dir..."
	$(call RM,build)
