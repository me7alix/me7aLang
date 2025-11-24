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
	u64 res_hash;
#if defined(_WIN32)
	#include <windows.h>
	DWORD len = GetFullPathNameA(str, sizeof(real_path), real_path, NULL);
	if (len == 0 || len >= sizeof(real_path))
		strncpy(real_path, str, sizeof(real_path)-1);
#else
	if (realpath(str, real_path) == NULL)
		strncpy(real_path, str, sizeof(real_path)-1);
#endif
	strhash(&res_hash, real_path);
	return res_hash;
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

void preprocessor(Imports *imports, Lexer *entry) {
	char *cur_folder = malloc(256);
	get_folder(cur_folder, entry->cur_loc.file);
	da_get(imports, 0) = cur_folder;
	ImportedTable_add(&it, entry->cur_loc.file, true);

	for (size_t i = 0; i < entry->tokens.count; i++) {
		switch (da_get(&entry->tokens, i).type) {
			case TOK_IMPORT: {
				i++;
				if (da_get(&entry->tokens, i).type != TOK_STRING)
					lexer_error(da_get(&entry->tokens, i).loc, "error: filepath expected");

				bool is_imported;
				Lexer *imp = get_lexer(imports, da_get(&entry->tokens, i).data, &is_imported);
				if (!imp && !is_imported)
					lexer_error(da_get(&entry->tokens, i).loc, "error: no such file");

				i++;
				if (da_get(&entry->tokens, i).type != TOK_SEMI)
					lexer_error(da_get(&entry->tokens, i).loc, "error: semicolon expected");

				i++;
				if (!is_imported) {
					preprocessor(imports, imp);

					for (size_t j = 0; j < imp->tokens.count - 1; j++) {
						da_insert(&entry->tokens, i, da_get(&imp->tokens, j));
						i++;
					}
				}
				i--;
			} break;

			case TOK_MACRO: {
				i++;
				if (da_get(&entry->tokens, i).type != TOK_ID)
					lexer_error(da_get(&entry->tokens, i).loc, "error: identifier expected");
				char *macro_id = da_get(&entry->tokens, i).data;
				Macro macro = {0};

				i++;
				for (size_t j = 0; da_get(&entry->tokens, i).type != TOK_SEMI; j++) {
					da_append(&macro, da_get(&entry->tokens, i));
					i++;
				}

				MacroTable_add(&mt, macro_id, macro);
			} break;

			case TOK_ID: {
				Macro *macro = MacroTable_get(&mt, da_get(&entry->tokens, i).data);
				if (!macro) break;
				da_remove_ordered(&entry->tokens, i);

				for (size_t j = 0; j < macro->count; j++) {
					da_insert(&entry->tokens, i, da_get(macro, j));
					i++;
				}
			} break;

			default:;
		}
	}
}
