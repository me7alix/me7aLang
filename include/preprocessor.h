#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include "lexer.h"

typedef DA(char*) Imports;

typedef struct {
	Imports *imports;
	Lexer lexer;
	Token *token;
} PreprocCtx;

char *read_file(const char *filename);
void preprocessor(PreprocCtx *p);

#endif
