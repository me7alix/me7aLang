#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdio.h>
#include <ctype.h>

#include "../include/lexer.h"

char *get_id(Lexer *l) {
	char *start = l->stream;
	while (
		isalpha(*l->stream) ||
		isdigit(*l->stream) ||
		*l->stream == '_'
	) l->stream++;
	l->stream--;
	size_t len = l->stream - start + 1;
	char *word = malloc(len + 1);
	memcpy(word, start, len);
	word[len] = '\0';
	return word;
}

void add_token(Lexer *l, TokenKind type, char *data) {
	da_append(&l->tokens, ((Token) {
		.kind = type,
		.data = data,
		.loc = l->loc,
	}));
}

bool is_keyword(Lexer *l, const char *tok, TokenKind type, char *str) {
	for (size_t i = 0; i < strlen(tok); i++) {
		if (tok[i] != str[i]) return false;
	}

	if (isalpha(str[strlen(tok)]) || str[strlen(tok)] == '_')
		return false;

	add_token(l, type, (char*)tok);

	for (size_t i = 0; i < strlen(tok) - 1; i++)
		l->stream++;

	return true;
}

Lexer lexer_lex(char *file, char *code) {
	Lexer l = {0};
	l.loc.file = file;
	l.stream = code;
	l.loc.line_start = code;

	while (*l.stream != '\0') {
		l.loc.line_char = l.stream;
		switch (*l.stream) {
		case ' ': case '\\': case '\t':          break;
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

		case '#': {
			if (l.stream[1] == '#') {
				add_token(&l, TOK_ID_CONCAT, "##");
				l.stream++;
			} else add_token(&l, TOK_TO_STR, "#");
		} break;

		case '.': {
			if (l.stream[1] == '.' && l.stream[2] == '.') {
				add_token(&l, TOK_ANY,   "...");
				l.stream += 2;
			} else add_token(&l, TOK_DOT,   ".");
		} break;

		case '+': {
			if (l.stream[1] == '=') {
				add_token(&l, TOK_PLUS_EQ, "+=");
				l.stream++;
			} else add_token(&l, TOK_PLUS, "+");
		} break;

		case '-': {
			if (l.stream[1] == '=') {
				add_token(&l, TOK_MINUS_EQ, "-=");
				l.stream++;
			} else add_token(&l, TOK_MINUS, "-");
		} break;

		case '*': {
			if (l.stream[1] == '=') {
				add_token(&l, TOK_STAR_EQ, "*=");
				l.stream++;
			} else add_token(&l, TOK_STAR, "*");
		} break;

		case '/': {
			if (l.stream[1] == '/') {
				while (l.stream[1] != '\n')
					l.stream++;
			} else if (l.stream[1] == '=') {
				add_token(&l, TOK_SLASH_EQ, "/=");
				l.stream++;
			} else add_token(&l, TOK_SLASH, "/");
		} break;

		case '!': {
			if (l.stream[1] == '=') {
				add_token(&l, TOK_NOT_EQ, "!=");
				l.stream++;
			} else {
				add_token(&l, TOK_EXC, "!");
			}
		} break;

		case '&': {
			if (l.stream[1] == '&') {
				add_token(&l, TOK_AND, "&&");
				l.stream++;
			} else {
				add_token(&l, TOK_AMP, "&");
			}
		} break;

		case '|': {
			if (l.stream[1] == '|') {
				add_token(&l, TOK_OR, "||");
				l.stream++;
			} else {
				add_token(&l, TOK_PIPE, "|");
			}
		} break;

		case '>': {
			if (l.stream[1] == '=') {
				add_token(&l, TOK_GREAT_EQ, ">=");
				l.stream++;
			} else if (l.stream[1] == '>') {
				add_token(&l, TOK_RIGHT_SHIFT, ">>");
				l.stream++;
			} else {
				add_token(&l, TOK_GREAT, ">");
			}
		} break;

		case '<': {
			if (l.stream[1] == '=') {
				add_token(&l, TOK_LESS_EQ, "<=");
				l.stream++;
			} else if (l.stream[1] == '<') {
				add_token(&l, TOK_LEFT_SHIFT, "<<");
				l.stream++;
			} else {
				add_token(&l, TOK_LESS, "<");
			}
		} break;

		case '=': {
			if (l.stream[1] == '=') {
				add_token(&l, TOK_EQ_EQ, "==");
				l.stream++;
			} else if (l.stream[1] == '>') {
				add_token(&l, TOK_ARROW_EQ, "=>");
				l.stream++;
			} else add_token(&l, TOK_EQ, "=");
		} break;

		case '\r':
		case '\n': {
			if (l.tokens.count > 0) {
				switch (da_last(&l.tokens).kind) {
				case TOK_OPAR:  case TOK_DOT:
				case TOK_SEMI:  case TOK_CBRA:
				case TOK_OBRA:  case TOK_COM:
				case TOK_ARROW: case TOK_ARROW_EQ: break;
				default:
					if (l.stream[-1] != '\\') {
						add_token(&l, TOK_SEMI, ";");
					}
				}
			}

			if (l.stream[0] == '\r' && l.stream[1] == '\n')
				l.stream++;

			l.loc.line_num++;
			l.loc.line_start = l.stream + 1;
		} break;

		case ':': {
			if (l.stream[1] == '=') {
				add_token(&l, TOK_ASSIGN, ":=");
				l.stream++;
			} else {
				add_token(&l, TOK_COL, ":");
			}
		} break;

		default:
			if (isdigit(*l.stream)) {
				char *start = l.stream;
				bool isFloat = 0;
				while (true) {
					if (*l.stream == '.')
						isFloat = 1;
					if (
						!(isdigit(l.stream[1]) ||
						isalpha(l.stream[1]) ||
						l.stream[1] == '.')
					) break;
					l.stream++;
				}

				size_t len = l.stream - start + 1;
				char *num = malloc(sizeof(char) * (len+1));
				memcpy(num, start, len); num[len] = '\0';
				if (isFloat) add_token(&l, TOK_FLOAT, num);
				else add_token(&l, TOK_INT, num);
				goto done;
			}

			if (*(l.stream) == '"') {
				StringBuilder sb = {0};
				l.stream++;

				while (!(l.stream[0] == '\"' && l.stream[-1] != '\\')) {
					if (l.stream[0] == '\\') {
						switch (l.stream[1]) {
						case '\\': sb_append(&sb, '\\'); break;
						case '0':  sb_append(&sb, '\0'); break;
						case 'n':  sb_append(&sb, '\n'); break;
						case 't':  sb_append(&sb, '\t'); break;
						case 'r':  sb_append(&sb, '\r'); break;
						case '\"': sb_append(&sb, '\"'); break;
						default: throw_error(l.loc, "wrong character");}
						l.stream++;
					} else if (l.stream[0] == '\0') {
						throw_error(l.loc, "unclosed string");
					} else {
						sb_append(&sb, l.stream[0]);
					}

					l.stream++;
				}

				sb_append(&sb, '\0');
				add_token(&l, TOK_STRING, sb.items);
				goto done;
			}

			if (*l.stream == '\'') {
				l.stream++;
				if (*l.stream == '\\') {
					l.stream++;
					switch (*l.stream) {
					case '0':  add_token(&l, TOK_CHAR, "\0"); break;
					case 'n':  add_token(&l, TOK_CHAR, "\n"); break;
					case 'r':  add_token(&l, TOK_CHAR, "\r"); break;
					case 't':  add_token(&l, TOK_CHAR, "\t"); break;
					case '\\': add_token(&l, TOK_CHAR, "\\"); break;
					case '\'': add_token(&l, TOK_CHAR, "'");  break;
					default: throw_error(l.loc, "wrong character");}
				} else add_token(&l, TOK_CHAR, l.stream);

				l.stream++;
				if (*l.stream != '\'')
					throw_error(l.loc, "' expected");
				goto done;
			}

			for (size_t i = 0; i < ARR_LEN(keywordPairs); i++) {
				if (is_keyword(&l, keywordPairs[i].id, keywordPairs[i].kind, l.stream)) {
					goto done;
				}
			}

			if (isalpha(*l.stream) || *l.stream == '_') {
				add_token(&l, TOK_ID, get_id(&l));
				goto done;
			}

			throw_error(l.loc, "unknown token");
		}

	done:
		l.stream++;
	}

	add_token(&l, TOK_EOF, "EOF");
	return l;
}

void lexer_free(Lexer *l) {
	da_free(&l->tokens);
}
