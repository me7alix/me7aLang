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
	ImportedTable it;
	MacroTable mt;
	Imports *imports;
	Lexer *lexer;
	size_t cur_tok;	
} PreprocCtx;

char *read_file(const char *filename);
void preprocessor(PreprocCtx *p, bool skip);

#endif
