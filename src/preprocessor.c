#include <string.h>
#include <stdio.h>
#include <stdbool.h>

#include "../thirdparty/sb.h"
#include "../include/preprocessor.h"

typedef struct {
	char *id;
	da(Token) changeto;
} Macro;

da(Macro) macros;
da(char *) imported;
StringBuilder path = {0};

#ifdef __linux__
int pathcmp(const char *a, const char *b) {
	char ra[512], rb[512];
	if (!realpath(a, ra) || !realpath(b, rb))
		return 0;
	return strcmp(ra, rb) == 0;
}
#endif

bool check_path() {
	da_foreach(char *, imp, &imported) {
		if (pathcmp(path.str, *imp)) {
			return true;
		}
	}

	return false;
}

Lexer *get_lexer(Imports *imports, char *file, bool *is_imported) {
	for (size_t i = 0; i < imports->count; i++) {
		sb_reset(&path);
		sb_append_strf(&path, "%s/%s", da_get(imports, i), file);

		char *code = read_file(sb_to_str(path));
		if (code) {
			if (check_path()) {
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
	da_append(&imported, (char *) sb_to_str(path));

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
				Macro macro = {.id = da_get(&entry->tokens, i).data};

				i++;
				for (size_t j = 0; da_get(&entry->tokens, i).type != TOK_SEMI; j++) {
					da_append(&macro.changeto, da_get(&entry->tokens, i));
					i++;
				}

				da_append(&macros, macro);
			} break;

			case TOK_ID: {
				da_foreach(Macro, macro, &macros) {
					if (strcmp(macro->id, da_get(&entry->tokens, i).data) == 0) {
						da_remove_ordered(&entry->tokens, i);

						for (size_t j = 0; j < macro->changeto.count; j++) {
							da_insert(&entry->tokens, i, da_get(&macro->changeto, j));
							i++;
						}

						break;
					}
				}
			} break;

			default:;
		}
	}
}
