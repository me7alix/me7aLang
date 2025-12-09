#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

#include "../thirdparty/cplus.h"
#include "../include/preprocessor.h"

typedef DA(Token) Macro;
HT(ImportedTable, char*, bool)
HT_STR(MacroTable, Macro)

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

u64 ImportedTable_hashf(char *str) {
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
	return strhash(real_path);;
}

int ImportedTable_compare(char *cur_str, char *str) {
	return pathcmp(cur_str, str);
}

Lexer *get_lexer(Imports *imports, char *file, bool *is_imported) {
	*is_imported = false;
	da_foreach(char*, imp, imports) {
		sb_reset(&path);
		sb_appendf(&path, "%s/%s", *imp, file);

		char *code = read_file(path.items);
		if (code) {
			if (ImportedTable_get(&it, path.items)) {
				*is_imported = true;
				return NULL;
			}

			Lexer *lexer = malloc(sizeof(Lexer));
			*lexer = lexer_lex(path.items, code);

			return lexer;
		}
	}

	return NULL;
}


void get_folder(char *dst, const char *file) {
	const char *slash = strrchr(file, '/');
	if (slash) {
		size_t len = slash - file;
		memcpy(dst, file, len);
		dst[len] = '\0';
	} else {
		dst[0] = '\0';
	}
}

bool insert_macro(Lexer *entry, size_t *cur_tok) {
	Macro *macro = MacroTable_get(&mt, da_get(&entry->tokens, *cur_tok).data);
	if (!macro) return false;
	da_remove_ordered(&entry->tokens, *cur_tok);

	for (size_t j = 0; j < macro->count; j++) {
		da_insert(&entry->tokens, *cur_tok, da_get(macro, j));
		if (!insert_macro(entry, cur_tok))
			(*cur_tok)++;
	}

	return true;
}

void preprocessor(Imports *imports, Lexer *entry) {
	char *cur_folder = malloc(256);
	get_folder(cur_folder, entry->cur_loc.file);
	da_get(imports, 0) = cur_folder;
	ImportedTable_add(&it, entry->cur_loc.file, true);

	for (size_t cur_tok = 0; cur_tok < entry->tokens.count; cur_tok++) {
		switch (da_get(&entry->tokens, cur_tok).kind) {
			case TOK_IMPORT: {
				cur_tok++;
				if (da_get(&entry->tokens, cur_tok).kind != TOK_STRING)
					lexer_error(da_get(&entry->tokens, cur_tok).loc, "error: filepath expected");

				bool is_imported;
				Lexer *imp = get_lexer(imports, da_get(&entry->tokens, cur_tok).data, &is_imported);
				if (!imp && !is_imported)
					lexer_error(da_get(&entry->tokens, cur_tok).loc, "error: no such file");

				cur_tok++;
				if (da_get(&entry->tokens, cur_tok).kind != TOK_SEMI)
					lexer_error(da_get(&entry->tokens, cur_tok).loc, "error: semicolon expected");

				cur_tok++;
				if (!is_imported) {
					preprocessor(imports, imp);

					for (size_t j = 0; j < imp->tokens.count - 1; j++) {
						da_insert(&entry->tokens, cur_tok, da_get(&imp->tokens, j));
						cur_tok++;
					}
				}
				cur_tok--;
			} break;

			case TOK_MACRO: {
				cur_tok++;
				if (da_get(&entry->tokens, cur_tok).kind != TOK_ID)
					lexer_error(da_get(&entry->tokens, cur_tok).loc, "error: identifier expected");
				char *mid = da_get(&entry->tokens, cur_tok).data;
				Macro macro = {0};

				cur_tok++;
				for (size_t j = 0; da_get(&entry->tokens, cur_tok).kind != TOK_SEMI; j++) {
					da_append(&macro, da_get(&entry->tokens, cur_tok));
					cur_tok++;
				}

				MacroTable_add(&mt, mid, macro);
			} break;

			case TOK_ID: {
				insert_macro(entry, &cur_tok);
			} break;

			default:;
		}
	}
}
