#ifndef LEXER_H
#define LEXER_H

#include "../thirdparty/cplus.h"

#include <stdlib.h>
#include <stdint.h>

typedef enum {
	TOK_CPAR, TOK_OPAR, TOK_ID, TOK_STRUCT, TOK_ASSIGN, TOK_EXTERN,
	TOK_SEMI, TOK_EQ, TOK_EQ_EQ, TOK_NOT_EQ, TOK_LESS, TOK_GREAT, TOK_CHAR,
	TOK_STRING, TOK_LPAR, TOK_EXC, TOK_PIPE, TOK_OR, TOK_AND, TOK_LESS_EQ,
	TOK_RPAR, TOK_PLUS, TOK_MINUS,TOK_FOR_SYM, TOK_RET, TOK_GREAT_EQ, TOK_NULL,
	TOK_IF_SYM, TOK_WHILE_SYM, TOK_STAR, TOK_AMP, TOK_COM, TOK_TRUE, TOK_FALSE,
	TOK_SLASH, TOK_INT, TOK_FLOAT, TOK_OBRA, TOK_COL, TOK_SIZEOF, TOK_OSQBRA,
	TOK_CSQBRA, TOK_CBRA, TOK_DOT, TOK_PLUS_EQ, TOK_MINUS_EQ, TOK_STAR_EQ,
	TOK_SLASH_EQ, TOK_EOF, TOK_BREAK, TOK_CONTINUE, TOK_IMPORT, TOK_MACRO,
	TOK_ELSE_SYM, TOK_PS, TOK_ANY, TOK_FUNC, TOK_LEFT_SHIFT, TOK_RIGHT_SHIFT,
	TOK_TILDA, TOK_XOR,
} TokenKind;

typedef struct {
	char *file;
	size_t line_num;
	char *line_start;
	char *line_char;
} Location;

typedef struct {
	TokenKind kind;
	Location loc;
	char *data;
} Token;

typedef struct {
	DA(Token) tokens;
	char *cur_char;
	Location cur_loc;
} Lexer;

void lexer_error(Location loc, char *error);
Lexer lexer_lex(char *file, char *code);
void lexer_free(Lexer *lexer);

#endif
