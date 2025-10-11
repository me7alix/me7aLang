#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

#include "../thirdparty/ht.h"
#include "../thirdparty/sb.h"
#include "../include/preprocessor.h"

typedef da(Token) Macro;
HT(ImportedTable, char*, bool)
HT_STR(MacroTable, Macro)
ImportedTable it   = {0};
MacroTable    mt   = {0};
StringBuilder path = {0};

#ifdef __linux__
int pathcmp(const char *a, const char *b) {
	char ra[512], rb[512];
	if (!realpath(a, ra) || !realpath(b, rb))
		return 0;
	return strcmp(ra, rb) == 0;
}

uint64_t ImportedTable_hashf(char *str) {
	char ra[512];
	char *path = realpath(str, ra);
	uint64_t res; strhash(&res, path);
	return res;
}

int ImportedTable_compare(char *cur_str, char *str) {
	char ra[512];
	char *p = realpath(str, ra);
	uint64_t res; strhash(&res, p);
	return res;
}
#endif

Lexer *get_lexer(Imports *imports, char *file, bool *is_imported) {
	for (size_t i = 0; i < imports->count; i++) {
		sb_reset(&path);
		sb_append_strf(&path, "%s/%s", da_get(imports, i), file);

		char *code = read_file(sb_to_str(path));
		if (code) {
			if (ImportedTable_get(&it, path.str)) {
				*is_imported = true;
				return NULL;
			}

			Lexer *lexer = malloc(sizeof(Lexer));
			*lexer = lexer_lex((char *) sb_to_str(path), code);

			*is_imported = false;
			return lexer;
		}
	}

	return NULL;
}


void get_path(char *dst, const char *file) {
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
	char cur_path[256]; get_path(cur_path, entry->cur_loc.file);
	da_get(imports, 0) = cur_path;
	ImportedTable_add(&it, strdup(cur_path), true);

	for (size_t i = 0; i < entry->tokens.count; i++) {
		switch (da_get(&entry->tokens, i).type) {
			case TOK_IMPORT: {
				i++;
				if (da_get(&entry->tokens, i).type != TOK_STRING)
					lexer_error(da_get(&entry->tokens, i).loc, "preprocessor error: filepath expected");

				bool is_imported;
				Lexer *imp = get_lexer(imports, da_get(&entry->tokens, i).data, &is_imported);
				if (!imp && !is_imported)
					lexer_error(da_get(&entry->tokens, i).loc, "preprocessor error: no such file");

				i++;
				if (da_get(&entry->tokens, i).type != TOK_SEMI)
					lexer_error(da_get(&entry->tokens, i).loc, "preprocessor error: semicolon expected");

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
					lexer_error(da_get(&entry->tokens, i).loc, "preprocessor error: identifier expected");
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
