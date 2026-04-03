#ifndef LEXER_H
#define LEXER_H

#include "../thirdparty/cplus.h"

#include <stdlib.h>
#include <stdint.h>

typedef enum {
	TOK_EOF,  TOK_ID,
	TOK_CPAR, TOK_OPAR,

	TOK_EXTERN, TOK_STATIC,
	TOK_STRUCT, TOK_FUNC,

	TOK_ASSIGN,
	TOK_SEMI,
	TOK_COL,

	TOK_EQ,
	TOK_STAR, TOK_AMP,
	TOK_COM, TOK_SLASH,
	TOK_LESS_EQ, TOK_GREAT_EQ,
	TOK_EQ_EQ, TOK_NOT_EQ,
	TOK_LESS, TOK_GREAT,
	TOK_EXC, TOK_PIPE,
	TOK_OR, TOK_AND,

	TOK_SIZEOF,
	TOK_INT, TOK_FLOAT,
	TOK_TRUE, TOK_FALSE,
	TOK_CHAR, TOK_STRING,
	TOK_NULL,	

	TOK_PLUS, TOK_MINUS,
	TOK_FOR_SYM, TOK_RET,

	TOK_IF_SYM, TOK_WHILE_SYM,
	TOK_ELSE_SYM,

	TOK_OBRA, TOK_CBRA,
	TOK_OSQBRA, TOK_CSQBRA,

	TOK_DOT, TOK_IMPL,
	TOK_PLUS_EQ, TOK_MINUS_EQ,
	TOK_STAR_EQ, TOK_SLASH_EQ,

	TOK_BREAK, TOK_CONTINUE,
	TOK_IMPORT, TOK_MACRO_OBJ,
	TOK_ANY, TOK_MACRO_FUNC,
	TOK_TO_STR,
	
	TOK_PS, TOK_BLOCK,
	TOK_LEFT_SHIFT, TOK_RIGHT_SHIFT,
	TOK_TILDA, TOK_XOR, TOK_ID_CONCAT,
	TOK_ARROW, TOK_ARROW_EQ,
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

void throw_error(Location loc, char *error);
Lexer lexer_lex(char *file, char *code);
void lexer_free(Lexer *lexer);

#endif
