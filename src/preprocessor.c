#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

#include "../thirdparty/cplus.h"
#include "../include/preprocessor.h"

typedef DA(Token) Tokens;

typedef struct {
	enum {
		MACRO_OBJ,
		MACRO_FUNC,
	} kind;

	union {
		struct {
			DA(char*) args;
			Tokens body;
		} func;

		struct {
			Tokens body;
		} obj;
	} as;
} Macro;

HT(ImportedTable, char*, bool)
HT_STR(MacroTable, Macro)

static Token peek(PreprocCtx *p) {
	return da_get(&p->lexer.tokens, p->cur_tok);
}

static Token peek2(PreprocCtx *p) {
	p->cur_tok++;
	Token tok = peek(p);
	p->cur_tok--;
	return tok;
}

static Token next(PreprocCtx *p) {
	Token tok = peek(p);
	p->cur_tok++;
	return tok;
}

static void insert(PreprocCtx *p, Token tok) {
	da_insert(&p->lexer.tokens, p->cur_tok, tok);
}

void remove_tok(PreprocCtx *p) {
	da_remove_ordered(&p->lexer.tokens, p->cur_tok);
}

ImportedTable it   = {0};
MacroTable    mt   = {0};
StringBuilder path = {0};

#if defined(_WIN32)
#include <windows.h>
#endif

int pathcmp(const char *a, const char *b) {
#if defined(_WIN32)
	char path1[32768], path2[32768];

	DWORD r1 = GetFullPathNameA(a, sizeof(path1), path1, NULL);
	DWORD r2 = GetFullPathNameA(b, sizeof(path2), path2, NULL);

	if (r1 == 0 || r2 == 0)
		return 1;

	return _stricmp(path1, path2);
#else
	char ra[1024], rb[1024];
	if (!realpath(a, ra) || !realpath(b, rb))
		return 1;
	return strcmp(ra, rb);
#endif
}

u32 ImportedTable_hashf(char *str) {
	char real_path[512];
#if defined(_WIN32)
	#include <windows.h>
	DWORD len = GetFullPathNameA(str, sizeof(real_path), real_path, NULL);
	if (len == 0 || len >= sizeof(real_path))
		strncpy(real_path, str, sizeof(real_path)-1);
#else
	if (realpath(str, real_path) == NULL)
		strncpy(real_path, str, sizeof(real_path)-1);
#endif
	return hash_str(real_path);;
}

int ImportedTable_compare(char *cur_str, char *str) {
	return pathcmp(cur_str, str);
}

Lexer get_lexer(Imports *imports, char *file, bool *isImported) {
	*isImported = false;

	da_foreach(char*, imp, imports) {
		sb_reset(&path);
		sb_appendf(&path, "%s/%s", *imp, file);

		char *code = read_file(path.items);
		if (code) {
			if (ImportedTable_get(&it, path.items)) {
				*isImported = true;
				return (Lexer){0};
			}

			return lexer_lex(path.items, code);
		}
	}

	return (Lexer){0};
}


void get_folder(char *dst, const char *file) {
	const char *slash = strrchr(file, '/');
	if (slash) {
		size_t len = slash - file;
		memcpy(dst, file, len);
		dst[len] = '\0';
	} else dst[0] = '\0';
}

void resolve_pars(PreprocCtx *p, Tokens *arg) {
	da_append(arg, next(p));

	while (peek(p).kind != TOK_CPAR) {
		if (peek(p).kind == TOK_OPAR) {
			resolve_pars(p, arg);
		} else if (peek(p).kind == TOK_EOF) {
			p->cur_tok--;
			throw_error(peek(p).loc, "TOK_CPAR expected");
		}

		da_append(arg, peek(p));
		next(p);
	}
}

void resolve_bras(PreprocCtx *p, Tokens *arg) {
	da_append(arg, next(p));

	while (peek(p).kind != TOK_CBRA) {
		if (peek(p).kind == TOK_OBRA) {
			resolve_bras(p, arg);
		} else if (peek(p).kind == TOK_EOF) {
			p->cur_tok--;
			throw_error(peek(p).loc, "TOK_CBRA expected");
		}

		da_append(arg, peek(p));
		next(p);
	}
}

bool insert_macro(PreprocCtx *p) {
	Macro *macro = MacroTable_get(&mt, peek(p).data);
	if (!macro) return false;
	remove_tok(p);

	switch (macro->kind) {
	case MACRO_OBJ:
		da_foreach (Token, tok, &macro->as.obj.body) {
			insert(p, *tok);
			if (!insert_macro(p)) next(p);
		}
		break;

	case MACRO_FUNC:
		DA(Tokens) args = {0};
		size_t savedInd = p->cur_tok;

		if (peek(p).kind != TOK_OPAR)
			throw_error(peek(p).loc, "TOK_OPAR expected");
		next(p);

		while (peek(p).kind != TOK_CPAR) {
			da_append(&args, (Tokens){0});
			Tokens *arg = &da_last(&args);

			while (true) {
				if (peek(p).kind == TOK_COM) {
					next(p);
					break;
				} else if (peek(p).kind == TOK_CPAR) {
					break;
				} else if (peek(p).kind == TOK_OPAR) {
					resolve_pars(p, arg);
				} else if (peek(p).kind == TOK_OBRA) {
					resolve_bras(p, arg);
				}

				da_append(arg, peek(p));
				next(p);
			}
		}
		next(p);

		if (macro->as.func.args.count != args.count) {
			throw_error(peek(p).loc, "arguments count mismatching");
		}

		size_t toDelete = p->cur_tok - savedInd;
		for (size_t i = 0; i < toDelete; i++) {
			da_remove_ordered(&p->lexer.tokens, savedInd);
			p->cur_tok--;
		}

		savedInd = p->cur_tok;

		da_foreach (Token, tok, &macro->as.func.body) {
			if (tok->kind == TOK_ID) {
				for (size_t j = 0; j < macro->as.func.args.count; j++) {
					char *arg = macro->as.func.args.items[j];

					if (strcmp(arg, tok->data) == 0) {
						da_foreach (Token, argTok, &da_get(&args, j)) {
							argTok->loc = tok->loc;
							insert(p, *argTok);
							next(p);
						}

						goto found;
					}
				}

				insert(p, *tok);
				next(p);
				found:;
			} else {
				insert(p, *tok);
				next(p);
			}
		}

		p->cur_tok = savedInd;

		preprocessor(p, true);
		p->cur_tok--;
		da_free(&args);
	}

	return true;
}

void preprocessor(PreprocCtx *p, bool skip) {
	if (!skip) {
		char *cur_folder = malloc(256);
		get_folder(cur_folder, p->lexer.cur_loc.file);
		da_get(p->imports, 0) = cur_folder;
		ImportedTable_add(&it, p->lexer.cur_loc.file, true);
	}

	while (peek(p).kind != TOK_EOF) {
		switch (peek(p).kind) {
		case TOK_IMPORT: {
			next(p);
			if (peek(p).kind != TOK_STRING)
				throw_error(peek(p).loc, "filepath expected");

			bool isImported;
			Lexer importedLex = get_lexer(p->imports, peek(p).data, &isImported);
			if (!importedLex.tokens.items && !isImported)
				throw_error(peek(p).loc, "no such file");
			next(p);

			if (peek(p).kind != TOK_SEMI)
				throw_error(peek(p).loc, "TOK_SEMI expected");
			next(p);

			if (!isImported) {
				char *savedCurFolder = p->imports->items[0];
				PreprocCtx nctx = {
					.imports = p->imports,
					.lexer = importedLex,
				};

				preprocessor(&nctx, false);
				importedLex = nctx.lexer;
				p->imports->items[0] = savedCurFolder;

				for (size_t j = 0; j < importedLex.tokens.count - 1; j++) {
					insert(p, da_get(&importedLex.tokens, j));
					next(p);
				}
			}

			p->cur_tok--;
		} break;

		case TOK_MACRO_FUNC: {
			next(p);

			if (peek(p).kind != TOK_ID)
				throw_error(peek(p).loc, "TOK_ID expected");
			char *id = peek(p).data;
			next(p);

			if (peek(p).kind != TOK_OPAR)
				throw_error(peek(p).loc, "TOK_OPAR expected");
			next(p);

			Macro macro = {.kind = MACRO_FUNC};
			while (peek(p).kind != TOK_CPAR) {
				if (peek(p).kind != TOK_ID)
					throw_error(peek(p).loc, "TOK_ID expected");
				da_append(&macro.as.func.args, peek(p).data);
				next(p);

				if (peek(p).kind == TOK_COM)
					next(p);
			}
			next(p);

			int braCnt = 1;
			if (peek(p).kind != TOK_OBRA)
				throw_error(peek(p).loc, "TOK_OBRA expected");
			next(p);

			while (true) {
				da_append(&macro.as.func.body, peek(p));
				next(p);

				if (peek(p).kind == TOK_CBRA) {
					braCnt--;
					if (braCnt == 0) break;
				} else if (peek(p).kind == TOK_OBRA) {
					braCnt++;
				}
			}

			MacroTable_add(&mt, id, macro);
		} break;

		case TOK_MACRO_OBJ: {
			next(p);
			if (peek(p).kind != TOK_ID)
				throw_error(peek(p).loc, "TOK_ID expected");
			char *id = peek(p).data;
			next(p);

			Macro macro = {.kind = MACRO_OBJ};
			while (peek(p).kind != TOK_SEMI) {
				da_append(&macro.as.obj.body, peek(p));
				next(p);
			}

			MacroTable_add(&mt, id, macro);
		} break;

		case TOK_ID:
			insert_macro(p);
			break;

		default:;
		}

		next(p);
	}

	p->cur_tok = 0;
	while (peek(p).kind != TOK_EOF) {
		if (
			peek(p).kind == TOK_TO_STR &&
			peek2(p).kind == TOK_ID
		) {
			remove_tok(p);
			char *id = peek(p).data;
			remove_tok(p);

			insert(p, (Token){
				.kind = TOK_STRING,
				.data = id,
			});

			next(p);
		} else if (
			peek(p).kind == TOK_ID &&
			peek2(p).kind == TOK_ID_CONCAT
		) {
			char *id1 = peek(p).data;
			remove_tok(p);
			remove_tok(p);

			if (peek(p).kind != TOK_ID)
				throw_error(peek(p).loc, "TOK_ID expected");

			char *id2 = peek(p).data;
			remove_tok(p);

			char *nid = malloc(strlen(id1) + strlen(id2) + 1);
			sprintf(nid, "%s%s", id1, id2);

			insert(p, (Token){
				.kind = TOK_ID,
				.data = nid,
			});

			if (peek2(p).kind != TOK_ID_CONCAT)
				next(p);
		} else if (
			peek(p).kind  == TOK_STRING &&
			peek2(p).kind == TOK_STRING
		) {
			char *str1 = peek(p).data;
			remove_tok(p);
			char *str2 = peek(p).data;
			remove_tok(p);

			char *nstr = malloc(strlen(str1) + strlen(str2) + 1);
			sprintf(nstr, "%s%s", str1, str2);

			insert(p, (Token){
				.kind = TOK_STRING,
				.data = nstr,
			});

			if (peek2(p).kind != TOK_STRING)
				next(p);
		} else {
			next(p);
		}
	}
}
