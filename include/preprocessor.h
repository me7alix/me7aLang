#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include "lexer.h"
#include <stdbool.h>

typedef DA(char*) Imports;

typedef struct {
	Imports *imports;
	Lexer lexer;
	size_t cur_tok;	
} PreprocCtx;

char *read_file(const char *filename);
void preprocessor(PreprocCtx *p, bool skip);

#endif
