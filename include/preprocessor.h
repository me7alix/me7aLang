#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include "lexer.h"

typedef da(Lexer) Sources;
typedef da(char*) Imports;
char *read_file(const char *filename);
void preprocessor(Imports *imports, Lexer *entry);

#endif
