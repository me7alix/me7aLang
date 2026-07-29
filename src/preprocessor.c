#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <cplus.h>
#include <preprocessor.h>

HT_IMPL(ImportedTable, char*, bool)
HT_IMPL_STR(MacroTable, Macro)

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
	char ra[2048], rb[2048];
	if (!realpath(a, ra) || !realpath(b, rb))
		return 1;
	return strcmp(ra, rb);
#endif
}

u32 ImportedTable_hashf(char *str) {
	char real_path[2048];
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

Tokens get_input(PreprocCtx *p, char *file, bool *is_imported) {
	static StringBuilder path = {0};
	*is_imported = false;
	da_foreach (char*, imp, p->imported_folders) {
		sb_reset(&path);
		sb_appendf(&path, "%s/%s", *imp, file);
		char *code = read_file(path.items);
		if (code) {
			if (ImportedTable_get(&p->import_registry, path.items)) {
				*is_imported = true;
				return (Tokens){0};
			}
			Lexer lex = lexer_lex(path.items, code);
			return (Tokens){
				.items = lex.tokens.items,
				.count = lex.tokens.count,
				.capacity = lex.tokens.capacity,
			};
		}
	}
	return (Tokens){0};
}

char *get_folder(const char *file) {
	char *dst = malloc(2048);
	const char *slash = strrchr(file, '/');
	if (slash) {
		size_t len = slash - file;
		memcpy(dst, file, len);
		dst[len] = '\0';
	} else dst[0] = '\0';
	return dst;
}

#define next(p) ((p)->input.items[(p)->count++])
#define peek(p) ((p)->input.items[(p)->count])
#define peek2(p) ((p)->input.items[(p)->count+1])

#define pp_append(p, tok) append(&(p)->output, tok)
static void append(Tokens *t, Token tok) { da_append(t, tok); }

static void resolve(PreprocCtx *p, Tokens *t, TokenKind lk, TokenKind rk) {
	int cnt = 1;
	append(t, next(p));
	while (cnt > 0) {
		if      (peek(p).kind == lk) cnt++;
		else if (peek(p).kind == rk) cnt--;
		append(t, next(p));
	}
}

void insert_macro(PreprocCtx *p) {
	Macro *macro = MacroTable_get(&p->macro_definitions, peek(p).data);
	Location loc = peek(p).loc;
	if (!macro) {
		pp_append(p, next(p));
		return;
	}
	switch (macro->kind) {
	case MACRO_OBJ: {
		p->inserted_macro = true;
		da_foreach (Token, tok, &macro->as.obj.body)
			pp_append(p, *tok);
		next(p);
	} break;
	case MACRO_FUNC:
		if (peek2(p).kind != TOK_OPAR) {
			pp_append(p, next(p));
			return;
		} else next(p);
		next(p);
		p->inserted_macro = true;
		DA(Tokens) args = {0};
		while (peek(p).kind != TOK_CPAR) {
			da_append(&args, (Tokens){0});
			Tokens *arg = &da_last(&args);
			while (true) {
				if (peek(p).kind == TOK_COM) {
					next(p); break;
				} else if (peek(p).kind == TOK_OPAR)
					resolve(p, arg, TOK_OPAR, TOK_CPAR);
				else if (peek(p).kind == TOK_OBRA)
					resolve(p, arg, TOK_OBRA, TOK_CBRA);
				else if (peek(p).kind == TOK_OSQBRA)
					resolve(p, arg, TOK_OSQBRA, TOK_CSQBRA);
				else if (peek(p).kind == TOK_CPAR) break;
				else append(arg, next(p));
			}
		}
		next(p);
		if (macro->as.func.args.count != args.count)
			throw_error(peek(p).loc, "arguments count mismatch");
		da_foreach (Token, tok, &macro->as.func.body) {
			Token itok = *tok;
			itok.loc = loc;
			if (tok->kind == TOK_ID) {
				bool found = false;
				for (size_t i = 0; i < macro->as.func.args.count; i++) {
					char *arg = macro->as.func.args.items[i];
					if (strcmp(arg, tok->data) == 0) {
						da_foreach (Token, arg_tok, &da_get(&args, i))
							pp_append(p, *arg_tok);
						found = true;
						break;
					}
				}
				if (!found) pp_append(p, itok);
			} else pp_append(p, itok);
		}
	}
}

#define expect(tok, knd) \
	do { \
		Token tmp = tok; \
		if ((tmp).kind != knd) { \
			throw_error((tmp).loc, #knd" expected"); \
		} \
	} while(0)

void preprocessor(PreprocCtx *p) {
	char *file = p->input.items->loc.file;
	p->imported_folders->items[0] = get_folder(file);
	ImportedTable_add(&p->import_registry, file, true);
	p->inserted_macro = true;
	while (p->inserted_macro) {
		p->inserted_macro = false;
		while (peek(p).kind != TOK_EOF) {
			switch (peek(p).kind) {
			case TOK_IMPORT: {
				next(p);
				if (peek(p).kind != TOK_STRING)
					throw_error(peek(p).loc, "filepath expected");
				bool is_imported;
				Tokens imported = get_input(p, peek(p).data, &is_imported);
				if (!imported.items && !is_imported)
					throw_error(peek(p).loc, "no such file");
				next(p);
				expect(next(p), TOK_SEMI);
				if (!is_imported) {
					PreprocCtx sp = *p;
					char *folder = p->imported_folders->items[0];
					p->input = imported;
					p->count = 0;
					preprocessor(p);
					p->output.count--; // remove last EOF
					p->imported_folders->items[0] = folder;
					p->inserted_macro = sp.inserted_macro;
					p->input = sp.input;
					p->count = sp.count;
				}
			} break;
			case TOK_MACRO_FUNC: {
				next(p);
				expect(peek(p), TOK_ID);
				char *id = next(p).data;
				expect(next(p), TOK_OPAR);
				Macro macro = {.kind = MACRO_FUNC};
				while (peek(p).kind != TOK_CPAR) {
					if (peek(p).kind != TOK_ID)
						throw_error(peek(p).loc, "TOK_ID expected");
					da_append(&macro.as.func.args, next(p).data);
					if (peek(p).kind == TOK_COM) next(p);
				}
				next(p);
				expect(next(p), TOK_OBRA);
				int cnt = 1;
				while (true) {
					if      (peek(p).kind == TOK_CBRA) cnt--;
					else if (peek(p).kind == TOK_OBRA) cnt++;
					if (cnt == 0) break;
					append(&macro.as.func.body, next(p));
				}
				MacroTable_add(&p->macro_definitions, id, macro);
				next(p);
			} break;
			case TOK_MACRO_OBJ: {
				next(p);
				expect(peek(p), TOK_ID);
				char *id = next(p).data;
				Macro macro = {.kind = MACRO_OBJ};
				while (peek(p).kind != TOK_SEMI)
					da_append(&macro.as.obj.body, next(p));
				MacroTable_add(&p->macro_definitions, id, macro);
				next(p);
			} break;
			case TOK_ID:
				insert_macro(p);
				break;
			default:
				pp_append(p, next(p));
			}
		}
		pp_append(p, ((Token){.kind = TOK_EOF}));
		da_copy(&p->input, &p->output);
		da_reset(&p->output);
		p->count = 0;
	}
	while (true) {
		bool change = false;
		while (peek(p).kind != TOK_EOF) {
			if (
				peek(p).kind == TOK_TO_STR &&
				peek2(p).kind == TOK_ID
			) {
				next(p);
				char *id = next(p).data;
				pp_append(p, ((Token){
					.kind = TOK_STRING,
					.data = id}));
				change = true;
			} else if (
				peek(p).kind == TOK_ID &&
				peek2(p).kind == TOK_ID_CONCAT
			) {
				char *id1 = next(p).data;
				expect(next(p), TOK_ID_CONCAT);
				expect(peek(p), TOK_ID);
				char *id2 = next(p).data;
				char *idn = malloc(strlen(id1) + strlen(id2) + 1);
				sprintf(idn, "%s%s", id1, id2);
				pp_append(p, ((Token){
					.kind = TOK_ID,
					.data = idn}));
				change = true;
			} else if (
				peek(p).kind == TOK_STRING &&
				peek2(p).kind == TOK_STRING
			) {
				char *lhs = next(p).data;
				char *rhs = next(p).data;
				char *res = malloc(strlen(lhs) + strlen(rhs) + 1);
				sprintf(res, "%s%s", lhs, rhs);
				pp_append(p, ((Token){
					.kind = TOK_STRING,
					.data = res}));
				change = true;
			} else pp_append(p, next(p));
		}
		pp_append(p, ((Token){.kind = TOK_EOF}));
		da_copy(&p->input, &p->output);
		da_reset(&p->output);
		p->count = 0;
		if (!change) break;
	}
	da_copy(&p->output, &p->input);
}
