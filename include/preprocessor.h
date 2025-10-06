#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include "lexer.h"

typedef da(Lexer) Sources;
void preprocessor(Sources *sources, Lexer *entry);

#endif
