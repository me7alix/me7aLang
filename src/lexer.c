#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdio.h>
#include <ctype.h>

#include "../include/lexer.h"

char *get_id(Lexer *l) {
	char *start = l->cur_char;

	while (isalpha(*l->cur_char) ||
		isdigit(*l->cur_char) ||
		*l->cur_char == '_')
		l->cur_char++;
	l->cur_char--;

	size_t len = l->cur_char - start + 1;

	char *word = malloc(sizeof(char) * (len+1));
	memcpy(word, start, len);
	word[len] = '\0';

	return word;
}

void add_token(Lexer *l, TokenKind type, char *data) {
	da_append(&l->tokens, ((Token) {
		.kind = type,
		.data = data,
		.loc = l->cur_loc,
	}));
}

bool is_tok(Lexer *l, char *tok, TokenKind type, char *str) {
	for (size_t i = 0; i < strlen(tok); i++) {
		if (tok[i] != str[i]) return false;
	}

	if (isalpha(str[strlen(tok)]) || str[strlen(tok)] == '_')
		return false;

	add_token(l, type, tok);

	for (size_t i = 0; i < strlen(tok) - 1; i++)
		l->cur_char++;

	return true;
}

void lexer_error(Location loc, char *error) {
	size_t lines_num = loc.line_num + 1;
	size_t chars_num = loc.line_char-loc.line_start + 1;
	printf("%s:%zu:%zu: %s\n", loc.file, lines_num, chars_num, error);

	loc.line_char = loc.line_start;
	char error_pointer[128];
	size_t cnt = 0;

	while (*loc.line_char != '\n' && *loc.line_char != '\0'){
		printf("%c", *loc.line_char);
		if (cnt < chars_num - 1) {
			if (*loc.line_char != '\t') error_pointer[cnt++] = ' ';
			else                        error_pointer[cnt++] = '\t';
		}
		loc.line_char++;
	}

	printf("\n");
	error_pointer[cnt++] = '^';
	error_pointer[cnt] = '\0';
	printf("%s\n", error_pointer);
	exit(1);
}

Lexer lexer_lex(char *file, char *code) {
	Lexer l = {0};
	l.cur_loc.file = file;
	l.cur_char = code;
	l.cur_loc.line_start = code;

	while (*l.cur_char != '\0') {
		l.cur_loc.line_char = l.cur_char;
		switch (*l.cur_char) {
			case ' ': case '\t': break;
			case '{': add_token(&l, TOK_OBRA,  "{"); break;
			case '}': add_token(&l, TOK_CBRA,  "}"); break;
			case '(': add_token(&l, TOK_OPAR,  "("); break;
			case ')': add_token(&l, TOK_CPAR,  ")"); break;
			case ';': add_token(&l, TOK_SEMI,  ";"); break;
			case ',': add_token(&l, TOK_COM,   ","); break;
			case '[': add_token(&l, TOK_OSQBRA,"["); break;
			case ']': add_token(&l, TOK_CSQBRA,"]"); break;
			case '%': add_token(&l, TOK_PS,    "%"); break;
			case '^': add_token(&l, TOK_XOR,   "^"); break;
			case '~': add_token(&l, TOK_TILDA, "~"); break;

			case '.': {
				if (l.cur_char[1] == '.' && l.cur_char[2] == '.') {
					add_token(&l, TOK_ANY,   "...");
					l.cur_char += 2;
				} else add_token(&l, TOK_DOT,   ".");
			} break;

			case '+': {
				if (l.cur_char[1] == '=') {
					add_token(&l, TOK_PLUS_EQ, "+=");
					l.cur_char++;
				} else add_token(&l, TOK_PLUS, "+");
			} break;

			case '-': {
				if (l.cur_char[1] == '=') {
					add_token(&l, TOK_MINUS_EQ, "-=");
					l.cur_char++;
				} else add_token(&l, TOK_MINUS, "-");
			} break;

			case '*': {
				if (l.cur_char[1] == '=') {
					add_token(&l, TOK_STAR_EQ, "*=");
					l.cur_char++;
				} else add_token(&l, TOK_STAR, "*");
			} break;

			case '/': {
				if (l.cur_char[1] == '/') {
					while (l.cur_char[1] != '\n')
						l.cur_char++;
				} else if (l.cur_char[1] == '=') {
					add_token(&l, TOK_SLASH_EQ, "/=");
					l.cur_char++;
				} else add_token(&l, TOK_SLASH, "/");
			} break;

			case '!': {
				if (l.cur_char[1] == '=') {
					add_token(&l, TOK_NOT_EQ, "!=");
					l.cur_char++;
				} else {
					add_token(&l, TOK_EXC, "!");
				}
			} break;

			case '&': {
				if (l.cur_char[1] == '&') {
					add_token(&l, TOK_AND, "&&");
					l.cur_char++;
				} else {
					add_token(&l, TOK_AMP, "&");
				}
			} break;

			case '|': {
				if (l.cur_char[1] == '|') {
					add_token(&l, TOK_OR, "||");
					l.cur_char++;
				} else {
					add_token(&l, TOK_PIPE, "|");
				}
			} break;

			case '>': {
				if (l.cur_char[1] == '=') {
					add_token(&l, TOK_GREAT_EQ, ">=");
					l.cur_char++;
				} else if (l.cur_char[1] == '>') {
					add_token(&l, TOK_RIGHT_SHIFT, ">>");
					l.cur_char++;
				} else {
					add_token(&l, TOK_GREAT, ">");
				}
			} break;

			case '<': {
				if (l.cur_char[1] == '=') {
					add_token(&l, TOK_LESS_EQ, "<=");
					l.cur_char++;
				} else if (l.cur_char[1] == '<') {
					add_token(&l, TOK_LEFT_SHIFT, "<<");
					l.cur_char++;
				} else {
					add_token(&l, TOK_LESS, "<");
				}
			} break;

			case '=': {
				if (l.cur_char[1] == '=') {
					add_token(&l, TOK_EQ_EQ, "==");
					l.cur_char++;
				} else {
					add_token(&l, TOK_EQ, "=");
				}
			} break;

			case '\r':
			case '\n': {
				switch (da_last(&l.tokens).kind) {
					case TOK_SEMI: case TOK_CBRA:
					case TOK_OBRA: case TOK_COM: break;
					default: add_token(&l, TOK_SEMI, ";");
				}

				if (l.cur_char[0] == '\r' && l.cur_char[1] == '\n')
					l.cur_char++;

				l.cur_loc.line_num++;
				l.cur_loc.line_start = l.cur_char + 1;
			} break;

			case ':': {
				if (l.cur_char[1] == '=') {
					add_token(&l, TOK_ASSIGN, ":=");
					l.cur_char++;
				} else {
					add_token(&l, TOK_COL, ":");
				}
			} break;

			default: {
				if (isdigit(*l.cur_char)) {
					char *start = l.cur_char;
					bool isFloat = 0;
					while (true) {
						if (*l.cur_char == '.')
							isFloat = 1;
						if (!(isdigit(l.cur_char[1]) ||
							isalpha(l.cur_char[1]) ||
							l.cur_char[1] == '.')) break;
						l.cur_char++;
					}

					size_t len = l.cur_char - start + 1;
					char *num = malloc(sizeof(char) * (len+1));
					memcpy(num, start, len); num[len] = '\0';
					if (isFloat) add_token(&l, TOK_FLOAT, num);
					else add_token(&l, TOK_INT, num);
				}

				else if (*(l.cur_char) == '"') {
					StringBuilder sb = {0};
					l.cur_char++;

					while (!(l.cur_char[0] == '\"' && l.cur_char[-1] != '\\')) {
						if (l.cur_char[0] == '\\') {
							switch (l.cur_char[1]) {
								case '\\': sb_append(&sb, '\\'); break;
								case '0':  sb_append(&sb, '\0'); break;
								case 'n':  sb_append(&sb, '\n'); break;
								case '\"': sb_append(&sb, '\"'); break;
								default: lexer_error(l.cur_loc, "error: wrong character");
							}
							l.cur_char++;
						} else if (l.cur_char[0] == '\0') {
							lexer_error(l.cur_loc, "error: unclosed string");
						} else {
							sb_append(&sb, l.cur_char[0]);
						}

						l.cur_char++;
					}

					sb_append(&sb, '\0');
					add_token(&l, TOK_STRING, sb.items);
				}

				else if (*l.cur_char == '\'') {
					l.cur_char++;
					if (*l.cur_char == '\\') {
						l.cur_char++;
						switch (*l.cur_char) {
							case '0':  add_token(&l, TOK_CHAR, "\0"); break;
							case 'n':  add_token(&l, TOK_CHAR, "\n"); break;
							case '\\': add_token(&l, TOK_CHAR, "\\"); break;
							case '\'': add_token(&l, TOK_CHAR, "'");  break;
							default: lexer_error(l.cur_loc, "error: wrong character");
						}
					} else add_token(&l, TOK_CHAR, l.cur_char);

					l.cur_char++;
					if (*l.cur_char != '\'') {
						lexer_error(l.cur_loc, "error: ' expected");
					}
				}

				else if   (is_tok(&l, "for",      TOK_FOR_SYM, l.cur_char)) {
				} else if (is_tok(&l, "while",    TOK_WHILE_SYM, l.cur_char)) {
				} else if (is_tok(&l, "if",       TOK_IF_SYM, l.cur_char)) {
				} else if (is_tok(&l, "else",     TOK_ELSE_SYM, l.cur_char)) {
				} else if (is_tok(&l, "struct",   TOK_STRUCT, l.cur_char)) {
				} else if (is_tok(&l, "extern",   TOK_EXTERN, l.cur_char)) {
				} else if (is_tok(&l, "true",     TOK_TRUE, l.cur_char)) {
				} else if (is_tok(&l, "false",    TOK_FALSE, l.cur_char)) {
				} else if (is_tok(&l, "break",    TOK_BREAK, l.cur_char)) {
				} else if (is_tok(&l, "continue", TOK_CONTINUE, l.cur_char)) {
				} else if (is_tok(&l, "null",     TOK_NULL, l.cur_char)) {
				} else if (is_tok(&l, "sizeof",   TOK_SIZEOF, l.cur_char)) {
				} else if (is_tok(&l, "return",   TOK_RET, l.cur_char)) {
				} else if (is_tok(&l, "import",   TOK_IMPORT, l.cur_char)) {
				} else if (is_tok(&l, "fn",       TOK_FUNC, l.cur_char)) {
				} else if (is_tok(&l, "def",      TOK_MACRO, l.cur_char)) {}

				else if (isalpha(*l.cur_char) || *l.cur_char == '_')
					add_token(&l, TOK_ID, get_id(&l));
				else lexer_error(l.cur_loc, "error: unknown token");
			} break;
		}

		l.cur_char++;
	}

	add_token(&l, TOK_EOF, "EOF");
	return l;
}

void lexer_free(Lexer *l) {
	da_free(&l->tokens);
}
