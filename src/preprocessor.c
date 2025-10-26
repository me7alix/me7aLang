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

#ifdef __linux__
int pathcmp(const char *a, const char *b) {
	char ra[512], rb[512];
	if (!realpath(a, ra) || !realpath(b, rb))
		return 1;
	return strcmp(ra, rb);
}

uint64_t ImportedTable_hashf(char *str) {
	char ra[512];
	uint64_t res;
	realpath(str, ra);
	strhash(&res, ra);
	return res;
}

int ImportedTable_compare(char *cur_str, char *str) {
	return pathcmp(cur_str, str);
}
#endif

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
