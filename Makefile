CC ?= gcc
CFLAGS ?= -std=c99 -pedantic -D_GNU_SOURCE
INC := -I include/ -I thirdparty/
SRCS := \
src/main.c \
src/preprocessor.c \
src/lexer.c \
src/parser.c \
src/parser_expr.c \
src/tac_ir_dump.c \
src/tac_ir_gen.c \
src/codegen/off_table.c \
src/codegen/reg_allocator.c \
src/codegen/nasm_amd64.c \
src/codegen/gas_arm64.c

ifeq ($(OS),Windows_NT)
EXE := .exe
else
EXE :=
endif

DEBUG_DIR   := build/debug
RELEASE_DIR := build/release
OUT_DEBUG   := $(DEBUG_DIR)/m7c$(EXE)
OUT_RELEASE := $(RELEASE_DIR)/m7c$(EXE)

OBJS_DEBUG   := $(patsubst %.c,$(DEBUG_DIR)/%.o,$(SRCS))
OBJS_RELEASE := $(patsubst %.c,$(RELEASE_DIR)/%.o,$(SRCS))

DEPS_DEBUG   := $(OBJS_DEBUG:.o=.d)
DEPS_RELEASE := $(OBJS_RELEASE:.o=.d)

ifeq ($(OS),Windows_NT)
ifdef COMSPEC
# For Windows cmd.exe
define MKDIR
@if not exist $(subst /,\,$(1)) mkdir $(subst /,\,$(1))
endef
define RM
@if exist $(subst /,\,$(1)) rmdir /s /q $(subst /,\,$(1))
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

release: $(OUT_RELEASE)

# Linking steps
$(OUT_DEBUG): $(OBJS_DEBUG)
	@echo "Linking metc (debug)..."
	@$(CC) $(OBJS_DEBUG) -o $@
	@echo "Built $@"

$(OUT_RELEASE): $(OBJS_RELEASE)
	@echo "Linking metc (release)..."
	@$(CC) $(OBJS_RELEASE) -o $@
	@echo "Built $@"

$(DEBUG_DIR)/%.o: %.c
	$(call MKDIR,$(dir $@))
	@echo "Compiling $< (debug)..."
	@$(CC) $(CFLAGS) -MMD -MP -ggdb -O0 -D_CP_RUNTIME_CHECKS $(INC) -c $< -o $@

$(RELEASE_DIR)/%.o: %.c
	$(call MKDIR,$(dir $@))
	@echo "Compiling $< (release)..."
	@$(CC) $(CFLAGS) -MMD -MP -O3 -DNDEBUG $(INC) -c $< -o $@

clean:
	@echo "Removing build dir..."
	$(call RM,build)

-include $(DEPS_DEBUG)
-include $(DEPS_RELEASE)
