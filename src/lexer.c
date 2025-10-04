#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include "../include/lexer.h"

void get_word(Lexer *lexer, char **start, char **end) {
	while (*lexer->cur_char == ' ')
		lexer->cur_char++;
	*start = lexer->cur_char;
	while (isalpha(*lexer->cur_char) ||
		isdigit(*lexer->cur_char) ||
		*lexer->cur_char == '_')
		lexer->cur_char++;
	*end = lexer->cur_char;
}

char *str_slice(char *str, size_t start, size_t len) {
	if (!str) return NULL;

	size_t str_len = strlen(str);
	if (start >= str_len) {
		char *empty = malloc(1);
		if (!empty) { errno = ENOMEM; return NULL; }
		empty[0] = '\0';
		return empty;
	}

	size_t max_avail = str_len - start;
	size_t slice_len = len < max_avail ? len : max_avail;

	char *slice = malloc(slice_len + 1);
	if (!slice) { errno = ENOMEM; return NULL; }

	memcpy(slice, str + start, slice_len);
	slice[slice_len] = '\0';
	return slice;
}

void add_token(Lexer *lexer, TokenType type, char *data) {
	if (lexer->tokens_num >= lexer->tokens_cap) {
		lexer->tokens_cap *= 2;
		lexer->tokens = realloc(lexer->tokens, lexer->tokens_cap * sizeof(Token));
	}

	lexer->tokens[lexer->tokens_num++] = (Token) {
		.type = type,
		.data = data,
		.location = lexer->cur_loc,
	};
}

bool is_tok(Lexer *lexer, char *tok, TokenType type, char *str) {
	for (size_t i = 0; i < strlen(tok); i++) {
		if (tok[i] != str[i]) return false;
	}

	add_token(lexer, type, tok);

	for (size_t i = 0; i < strlen(tok) - 1; i++) 
		lexer->cur_char++;

	return true;
}

Lexer lexer_alloc(char *code) {
	return (Lexer) {
		.cur_char = code,
		.tokens = malloc(sizeof(Token) * 32),
		.tokens_num = 0,
		.tokens_cap = 32,
		.cur_loc = (Location) {0, code},
	};
}

void lexer_error(Lexer *lexer, char *error) {
	size_t lines_num = lexer->cur_loc.line_num + 1;
	size_t chars_num = lexer->cur_char-lexer->cur_loc.line_start + 1;
	printf("%zu:%zu lexer error: %s\n", lines_num, chars_num, error);

	lexer->cur_char = lexer->cur_loc.line_start;
	char error_pointer[128];
	size_t cnt = 0;

	while (*lexer->cur_char != '\n' && *lexer->cur_char != '\0'){
		printf("%c", *lexer->cur_char);

		if (cnt < chars_num - 1) {
			if (*lexer->cur_char != '\t')
				error_pointer[cnt++] = ' ';
			else
				error_pointer[cnt++] = '\t';
		}

		lexer->cur_char++;
	}

	printf("\n");
	error_pointer[cnt++] = '^';
	error_pointer[cnt] = '\0';
	printf("%s\n", error_pointer);
	exit(1);
}

void lexer_lex(Lexer *lexer) {
	while (*lexer->cur_char != '\0') {
		switch (*lexer->cur_char) {
			case ' ':
				break;
			case '\t':
				break;
			case '\n':
				if (lexer->tokens[lexer->tokens_num-1].type != TOK_SEMI &&
					lexer->tokens[lexer->tokens_num-1].type != TOK_RBRC &&
					lexer->tokens[lexer->tokens_num-1].type != TOK_LBRC &&
					lexer->tokens[lexer->tokens_num-1].type != TOK_COM)
					add_token(lexer, TOK_SEMI, ";");
				lexer->cur_loc.line_num++;
				lexer->cur_loc.line_start = lexer->cur_char + 1;
				break;
			case '{':
				add_token(lexer, TOK_LBRC, "{");
				break;
			case '}':
				add_token(lexer, TOK_RBRC, "}");
				break;
			case '(':
				add_token(lexer, TOK_LBRA, "(");
				break;
			case ')':
				add_token(lexer, TOK_RBRA, ")");
				break;
			case '+':
				add_token(lexer, TOK_PLUS, "+");
				break;
			case '-':
				add_token(lexer, TOK_MINUS, "-");
				break;
			case '*':
				add_token(lexer, TOK_STAR, "*");
				break;
			case '=':
				add_token(lexer, TOK_EQ, "=");
				break;
			case '>':
				add_token(lexer, TOK_GREAT, ">");
				break;
			case '<':
				add_token(lexer, TOK_LESS, "<");
				break;
			case ';':
				add_token(lexer, TOK_SEMI, ";");
				break;
			case '.':
				add_token(lexer, TOK_DOT, ".");
				break;
			case '&':
				add_token(lexer, TOK_AMP, "&");
				break;
			case '!':
				add_token(lexer, TOK_EXC, "!");
				break;
			case ',':
				add_token(lexer, TOK_COM, ",");
				break;
			case '|':
				add_token(lexer, TOK_PIPE, "|");
				break;
			case ':':
				char *semi = lexer->cur_char;
				lexer->cur_char++;
				char *start, *end;
				get_word(lexer, &start, &end);
				add_token(lexer, TOK_TYPE, str_slice(start, 0, end - start));
				lexer->cur_char = end - 1;
				//add_token(lexer, TOK_COL, ":");
				break;
			case '/':
				if (*(lexer->cur_char + 1) == '/') {
					while (*(lexer->cur_char) != '\n')
						lexer->cur_char += 1;
					break;
				}
				add_token(lexer, TOK_SLASH, "/");
				break;
			default:
				if (isdigit(*lexer->cur_char)) {
					char *start = lexer->cur_char;
					bool isFloat = 0;
					while (true) {	
						if (*(lexer->cur_char) == '.')
							isFloat = 1;
						if (!(isdigit(*(lexer->cur_char+1)) || *(lexer->cur_char+1) == '.'))
							break;
						lexer->cur_char++;
					}
					if (isFloat) add_token(lexer, TOK_FLOAT, str_slice(start, 0, lexer->cur_char - start + 1));
					else add_token(lexer, TOK_INT, str_slice(start, 0, lexer->cur_char - start + 1));
				} else if (*(lexer->cur_char) == '"') {
					char *lmark = lexer->cur_char++;
					char *start = lexer->cur_char;
					while (!(*(lexer->cur_char+1) == '\"' && *(lexer->cur_char) != '\\')) {
						if (*(lexer->cur_char++) == '\0') {
							lexer->cur_char = lmark;
							lexer_error(lexer, "unclosed string");
						}
					}
					add_token(lexer, TOK_STRING, str_slice(start, 0, lexer->cur_char - start + 1));
					lexer->cur_char++;
				} else if (*(lexer->cur_char) == '\'') {
					lexer->cur_char++;
					if ((*(lexer->cur_char + 1) != '\'')) {
						lexer->cur_char++;
						lexer_error(lexer, "' expected");
					}

					add_token(lexer, TOK_CHAR, lexer->cur_char);
					lexer->cur_char++;
				} else if (is_tok(lexer, "for", TOK_FOR_SYM, lexer->cur_char)) {
				} else if (is_tok(lexer, "while", TOK_WHILE_SYM, lexer->cur_char)) {
				} else if (is_tok(lexer, "if", TOK_IF_SYM, lexer->cur_char)) {
				} else if (is_tok(lexer, "func", TOK_FUNC, lexer->cur_char)) {
				} else if (is_tok(lexer, "struct", TOK_STRUCT, lexer->cur_char)) {
				} else if (is_tok(lexer, "return", TOK_RET, lexer->cur_char)) {
				} else if (isalpha(*lexer->cur_char)) {
					char *start, *end;
					get_word(lexer, &start, &end);
					add_token(lexer, TOK_ID, str_slice(start, 0, end - start));
					lexer->cur_char = end - 1;
				} else {
					lexer_error(lexer, "unknown token");
				}

				break;
		}
		lexer->cur_char++;
	}

	add_token(lexer, TOK_EOF, "EOF");
}

void lexer_free(Lexer *lexer) {
	free(lexer->tokens);
}

const char *tok_to_str(TokenType tok_type) {
	switch (tok_type) {
		case TOK_FUNC:		return "TOK_FUNC";
		case TOK_RBRA:		return "TOK_RBRA";
		case TOK_LBRA:		return "TOK_LBRA";
		case TOK_ID:		return "TOK_ID";
		case TOK_SEMI:		return "TOK_SEMI";
		case TOK_EQ:		return "TOK_EQ";
		case TOK_LESS:		return "TOK_LESS";
		case TOK_GREAT:		return "TOK_GREAT";
		case TOK_TYPE:		return "TOK_TYPE";
		case TOK_STRUCT:	return "TOK_STRUCT";
		case TOK_STRING:	return "TOK_STRING";
		case TOK_LPAR:		return "TOK_LPAR";
		case TOK_RPAR:		return "TOK_RPAR";
		case TOK_PLUS:		return "TOK_PLUS";
		case TOK_MINUS:		return "TOK_MINUS";
		case TOK_FOR_SYM:	return "TOK_FOR_SYM";
		case TOK_IF_SYM:	return "TOK_IF_SYM";
		case TOK_WHILE_SYM: return "TOK_WHILE_SYM";
		case TOK_STAR:		return "TOK_STAR";
		case TOK_SLASH:		return "TOK_SLASH";
		case TOK_INT:		return "TOK_INT";
		case TOK_FLOAT:		return "TOK_FLOAT";
		case TOK_LBRC:		return "TOK_LBRC";
		case TOK_RBRC:		return "TOK_RBRC";
		case TOK_COL:		return "TOK_COL";
		case TOK_DOT:		return "TOK_DOT";
		case TOK_COM:		return "TOK_COM";
		case TOK_RET:		return "TOK_RET";
		case TOK_AMP:		return "TOK_AMP";
		case TOK_PIPE:		return "TOK_PIPE";
		case TOK_EXC:		return "TOK_EXC";
		case TOK_CHAR:		return "TOK_CHAR";
		default:			return "UNKNOWN_TOKEN";
	}
}

