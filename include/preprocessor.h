#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include "lexer.h"
#include <stdbool.h>

typedef DA(Token) Tokens;

typedef struct {
	enum {
		MACRO_OBJ,
		MACRO_FUNC,
	} kind;
	union {
		struct {
			DA(char*) args;
			Tokens body;
		} func;
		struct {
			Tokens body;
		} obj;
	} as;
} Macro;

typedef DA(char*) Imports;
HT_DECL(ImportedTable, char*, bool)
HT_DECL_STR(MacroTable, Macro)

typedef struct {
	Imports *imported_folders;
	ImportedTable import_registry;
	MacroTable macro_definitions;
	bool inserted_macro;
	Tokens output;
	Tokens input;
	size_t count;
} PreprocCtx;

char *read_file(const char *filename);
void preprocessor(PreprocCtx *p);

#endif
