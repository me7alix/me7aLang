#ifndef LEXER_H
#define LEXER_H

#include "../thirdparty/da.h"

#include <stdlib.h>
#include <stdint.h>

typedef enum {
	TOK_FUNC, TOK_CPAR, TOK_OPAR, TOK_ID, TOK_STRUCT,
	TOK_SEMI, TOK_EQ, TOK_EQ_EQ, TOK_LESS, TOK_GREAT, TOK_CHAR,
	TOK_TYPE, TOK_STRING, TOK_LPAR, TOK_EXC, TOK_PIPE, TOK_LESS_EQ,
	TOK_RPAR, TOK_PLUS, TOK_MINUS,TOK_FOR_SYM, TOK_RET, TOK_GREAT_EQ,
	TOK_IF_SYM, TOK_WHILE_SYM, TOK_STAR, TOK_AMP, TOK_COM,
	TOK_SLASH, TOK_INT, TOK_FLOAT, TOK_OBRA, TOK_COL,
	TOK_CBRA, TOK_DOT, TOK_EOF,
} TokenType;

typedef struct {
	size_t line_num;
	char *line_start;
} Location;

typedef struct {
	TokenType type;
	Location location;
	char *data;
} Token;

typedef struct {
	da(Token) tokens;
	char *cur_char;
	Location cur_loc;
} Lexer;

void lexer_lex(Lexer *lexer, char *code);
void lexer_free(Lexer *lexer);
const char *tok_to_str(TokenType tok_type);

#endif
