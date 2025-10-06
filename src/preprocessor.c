#include <string.h>
#include <stdio.h>
#include <stdbool.h>

#include "../include/preprocessor.h"

typedef struct {
	char *id;
	da(Token) changeto;
} Macro;

bool imported[512] = {0};
int is_imported;
da(Macro) macros;

Lexer *pp_get_src(Sources *sources, char *file) {
	for (size_t i = 0; i < sources->count; i++) {
		Lexer *src = &da_get(sources, i);
		if (strcmp(src->cur_loc.file, file) == 0) {
			if (imported[i]) {
				is_imported = true;
				return NULL;
			}

			imported[i] = true;
			is_imported = false;
			return src;
		}
	}

	return NULL;
}

void preprocessor(Sources *sources, Lexer *entry) {
	Token *cur_tok = entry->tokens.items;
	for (size_t i = 0; i < entry->tokens.count; i++) {
		switch (da_get(&entry->tokens, i).type) {
			case TOK_IMPORT: {
				i++;
				if (da_get(&entry->tokens, i).type != TOK_STRING)
					lexer_error(da_get(&entry->tokens, i).loc, "preprocessor error: filepath expected");

				Lexer *imp = pp_get_src(sources, da_get(&entry->tokens, i).data);
				if (!imp && !is_imported) lexer_error(da_get(&entry->tokens, i).loc, "preprocessor error: no such file");

				i++;
				if (da_get(&entry->tokens, i).type != TOK_SEMI)
					lexer_error(da_get(&entry->tokens, i).loc, "preprocessor error: filepath expected");

				i++;
				if (!is_imported) {
					preprocessor(sources, imp);

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
