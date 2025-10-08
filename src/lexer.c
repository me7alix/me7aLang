#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdio.h>
#include <ctype.h>
#include <threads.h>
#include "../include/lexer.h"

char *get_word(Lexer *lexer) {
	while (*lexer->cur_char == ' ')
		lexer->cur_char++;

	char *start = lexer->cur_char;
	while (isalpha(*lexer->cur_char) ||
		isdigit(*lexer->cur_char) ||
		*lexer->cur_char == '_')
		lexer->cur_char++;
	lexer->cur_char--;

	size_t l = lexer->cur_char - start + 1;
	char *word = malloc(sizeof(char) * (l+1));
	memcpy(word, start, l);
	word[l] = '\0';
	return word;
}

void add_token(Lexer *lexer, TokenType type, char *data) {
	da_append(&lexer->tokens, ((Token) {
		.type = type,
		.data = data,
		.loc = lexer->cur_loc,
	}));
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

void lexer_error(Location loc, char *error) {
	size_t lines_num = loc.line_num + 1;
	size_t chars_num = loc.line_char-loc.line_start + 1;
	printf("%s:%zu:%zu %s\n", loc.file, lines_num, chars_num, error);

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
	Lexer lexer = {0};
	lexer.cur_loc.file = file;
	lexer.cur_char = code;
	lexer.cur_loc.line_start = code;

	while (*lexer.cur_char != '\0') {
		lexer.cur_loc.line_char = lexer.cur_char;
		switch (*lexer.cur_char) {
			case ' ': case '\t': break;
			case '{': add_token(&lexer, TOK_OBRA,  "{"); break;
			case '}': add_token(&lexer, TOK_CBRA,  "}"); break;
			case '(': add_token(&lexer, TOK_OPAR,  "("); break;
			case ')': add_token(&lexer, TOK_CPAR,  ")"); break;
			case ';': add_token(&lexer, TOK_SEMI,  ";"); break;
			case '.': add_token(&lexer, TOK_DOT,   "."); break;
			case ',': add_token(&lexer, TOK_COM,   ","); break;
			case '[': add_token(&lexer, TOK_OSQBRA,"["); break;
			case ']': add_token(&lexer, TOK_CSQBRA,"]"); break;

			case '+': {
				if (lexer.cur_char[1] == '=') {
					add_token(&lexer, TOK_PLUS_EQ, "+=");
					lexer.cur_char++;
				} else add_token(&lexer, TOK_PLUS, "+");
			} break;

			case '-': {
				if (lexer.cur_char[1] == '=') {
					add_token(&lexer, TOK_MINUS_EQ, "-=");
					lexer.cur_char++;
				} else add_token(&lexer, TOK_MINUS, "-");
			} break;

			case '*': {
				if (lexer.cur_char[1] == '=') {
					add_token(&lexer, TOK_STAR_EQ, "*=");
					lexer.cur_char++;
				} else add_token(&lexer, TOK_STAR, "*");
			} break;

			case '/': {
				if (lexer.cur_char[1] == '/') {
					while (*lexer.cur_char != '\n')
						lexer.cur_char += 1;
				} else if (lexer.cur_char[1] == '=') {
					add_token(&lexer, TOK_SLASH_EQ, "/=");
					lexer.cur_char++;
				} else add_token(&lexer, TOK_SLASH, "/");
			} break;

			case '!': {
				if (lexer.cur_char[1] == '=') {
					add_token(&lexer, TOK_NOT_EQ, "!=");
					lexer.cur_char++;
				} else {
					add_token(&lexer, TOK_EXC, "!"); 
				}
			} break;

			case '&': {
				if (lexer.cur_char[1] == '&') {
					add_token(&lexer, TOK_AND, "&&"); 
					lexer.cur_char++;
				} else {
					add_token(&lexer, TOK_AMP, "&"); 
				}
			} break;

			case '|': {
				if (lexer.cur_char[1] == '|') {
					add_token(&lexer, TOK_OR, "||");
					lexer.cur_char++;
				} else {
					add_token(&lexer, TOK_PIPE, "|");
				}
			} break;

			case '>': {
				if (lexer.cur_char[1] == '=') {
					add_token(&lexer, TOK_GREAT_EQ, ">=");
					lexer.cur_char++;
				} else {
					add_token(&lexer, TOK_GREAT, ">");
				}
			} break;

			case '<': {
				if (lexer.cur_char[1] == '=') {
					add_token(&lexer, TOK_LESS_EQ, "<=");
					lexer.cur_char++;
				} else {
					add_token(&lexer, TOK_LESS, "<");
				}
			} break;

			case '=': {
				if (lexer.cur_char[1] == '=') {
					add_token(&lexer, TOK_EQ_EQ, "==");
					lexer.cur_char++;
				} else {
					add_token(&lexer, TOK_EQ, "=");
				}
			} break;

			case '\n':
				if (da_last(&lexer.tokens).type != TOK_SEMI &&
					da_last(&lexer.tokens).type != TOK_CBRA &&
					da_last(&lexer.tokens).type != TOK_OBRA &&
					da_last(&lexer.tokens).type != TOK_COM)
					add_token(&lexer, TOK_SEMI, ";");
				lexer.cur_loc.line_num++;
				lexer.cur_loc.line_start = lexer.cur_char + 1;
				break;

			case ':': {
				if (lexer.cur_char[1] == '=') {
					add_token(&lexer, TOK_ASSIGN, ":=");
					lexer.cur_char++;
				} else {
					add_token(&lexer, TOK_COL, ":");
				}
			} break;

			default: {
				if (isdigit(*lexer.cur_char)) {
					char *start = lexer.cur_char;
					bool isFloat = 0;
					while (true) {
						if (*lexer.cur_char == '.')
							isFloat = 1;
						if (!(isdigit(lexer.cur_char[1]) ||
							isalpha(lexer.cur_char[1]) ||
							lexer.cur_char[1] == '.')) break;
						lexer.cur_char++;
					}

					size_t l = lexer.cur_char - start + 1;
					char *num = malloc(sizeof(char) * (l+1));
					memcpy(num, start, l); num[l] = '\0';
					if (isFloat) add_token(&lexer, TOK_FLOAT, num);
					else add_token(&lexer, TOK_INT, num);
				}

				else if (*(lexer.cur_char) == '"') {
					char *lmark = lexer.cur_char++;
					char *start = lexer.cur_char;
					while (!(lexer.cur_char[1] == '\"' && *lexer.cur_char != '\\')) {
						if (*(lexer.cur_char++) == '\0') {
							lexer.cur_char = lmark;
							lexer_error(lexer.cur_loc, "error: unclosed string");
						}
					}

					size_t l = lexer.cur_char - start + 1;
					char *str = malloc(sizeof(char) * (l+1));
					memcpy(str, start, l); str[l] = '\0';
					add_token(&lexer, TOK_STRING, str);
					lexer.cur_char++;
				}

				else if (*lexer.cur_char == '\'') {
					lexer.cur_char++;
					if (*lexer.cur_char == '\\') {
						lexer.cur_char++;
						if      (*lexer.cur_char == 'n') add_token(&lexer, TOK_CHAR, "\n");
						else if (*lexer.cur_char == '0') add_token(&lexer, TOK_CHAR, "\0");
						else lexer_error(lexer.cur_loc, "error: wrong character");
						lexer.cur_char++;
					} else {
						add_token(&lexer, TOK_CHAR, lexer.cur_char);
						lexer.cur_char++;
					}

					if (*lexer.cur_char != '\'') {
						lexer_error(lexer.cur_loc, "error: ' expected");
					}
				}

				else if   (is_tok(&lexer, "for",     TOK_FOR_SYM, lexer.cur_char)) {
				} else if (is_tok(&lexer, "while",   TOK_WHILE_SYM, lexer.cur_char)) {
				} else if (is_tok(&lexer, "if",      TOK_IF_SYM, lexer.cur_char)) {
				} else if (is_tok(&lexer, "else",    TOK_ELSE_SYM, lexer.cur_char)) {
				} else if (is_tok(&lexer, "func",    TOK_FUNC, lexer.cur_char)) {
				} else if (is_tok(&lexer, "struct",  TOK_STRUCT, lexer.cur_char)) {
				} else if (is_tok(&lexer, "extern",  TOK_EXTERN, lexer.cur_char)) {
				} else if (is_tok(&lexer, "true",    TOK_TRUE, lexer.cur_char)) {
				} else if (is_tok(&lexer, "false",   TOK_FALSE, lexer.cur_char)) {
				} else if (is_tok(&lexer, "break",   TOK_BREAK, lexer.cur_char)) {
				} else if (is_tok(&lexer, "continue",TOK_CONTINUE, lexer.cur_char)) {
				} else if (is_tok(&lexer, "null",    TOK_NULL, lexer.cur_char)) {
				} else if (is_tok(&lexer, "sizeof",  TOK_SIZEOF, lexer.cur_char)) {
				} else if (is_tok(&lexer, "return",  TOK_RET, lexer.cur_char)) {
				} else if (is_tok(&lexer, "import",  TOK_IMPORT, lexer.cur_char)) {
				} else if (is_tok(&lexer, "macro",   TOK_MACRO, lexer.cur_char)) {
				} else if (isalpha(*lexer.cur_char)) add_token(&lexer, TOK_ID, get_word(&lexer));

				else lexer_error(lexer.cur_loc, "error: unknown token");
			} break;
		}

		lexer.cur_char++;
	}

	add_token(&lexer, TOK_EOF, "EOF");
	return lexer;
}

void lexer_free(Lexer *lexer) {
	da_free(&lexer->tokens);
}
