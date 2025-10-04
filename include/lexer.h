#ifndef LEXER_H
#define LEXER_H

#include <stdlib.h>
#include <stdint.h>

typedef enum {
	TOK_FUNC, TOK_RBRA, TOK_LBRA, TOK_ID, TOK_STRUCT,
	TOK_SEMI, TOK_EQ, TOK_LESS, TOK_GREAT, TOK_CHAR,
	TOK_TYPE, TOK_STRING, TOK_LPAR, TOK_EXC, TOK_PIPE,
	TOK_RPAR, TOK_PLUS, TOK_MINUS,TOK_FOR_SYM, TOK_RET,
	TOK_IF_SYM, TOK_WHILE_SYM, TOK_STAR, TOK_AMP, TOK_COM,
	TOK_SLASH, TOK_INT, TOK_FLOAT, TOK_LBRC, TOK_COL,
	TOK_RBRC, TOK_DOT, TOK_EOF,
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
	Token *tokens;
	size_t tokens_num;
	size_t tokens_cap;
	char *cur_char;
	Location cur_loc;	
} Lexer;

Lexer lexer_alloc(char *code);
void lexer_lex(Lexer *lexer);
void lexer_free(Lexer *lexer);
const char *tok_to_str(TokenType tok_type);

#endif
