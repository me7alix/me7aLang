#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "../include/parser.h"

bool check_nested(int pn[16], int n[16]) {
	for (size_t i = 0; n[i] != 0; i++) {
		if (pn[i] != n[i]) return false;
	}

	return true;
}

Symbol *st_get(SymbolTable *st, int nested[16], const char *id) {
	for (size_t i = st->count - 1; i >= 0; i--) {
		if (strcmp(da_get(st, i).id, id) == 0 && check_nested(da_get(st, i).nested, nested)) {
			return &da_get(st, i);
		}
	}

	return NULL;
}

AST_Node *ast_alloc(AST_Node node) {
	AST_Node *new = malloc(sizeof(AST_Node)); 
	memcpy(new, &node, sizeof(node));
	return new;
}

void expect_token(Token *token, TokenType type) {
	if (token->type == type) return;

	size_t lines_num = token->location.line_num + 1;
	const char *tok_str = tok_to_str(type);

	printf("%zu parser error: %s token expected\n", lines_num, tok_str);
	exit(1);
}

void parse_block(Parser *parser, AST_Node *parent) {
	int br_cnt;

	while (true) {
		switch (parser->cur_token->type) {
			case TOK_LBRC:
				br_cnt++;
				break;

			case TOK_RBRC:
				br_cnt--;
				if (br_cnt == 0)
					return;

			case TOK_TYPE:
				expect_token(parser->cur_token-1, TOK_ID);
				expect_token(parser->cur_token+1, TOK_EQ);

				parser->cur_token++;

				da_append(&parent->payload.block.stmts, ast_alloc((AST_Node){
							.type = AST_VAR_DEF,
							.payload.variable.id = (parser->cur_token-1)->data,
							.payload.variable.type = parser->cur_token->data, 
							.payload.variable.exp = parse_expr(parser, EXPR_PARSING_VAR),
							}));

				break;

			case TOK_IF_SYM:
				expect_token(parser->cur_token+1, TOK_LBRA);

				break;

			default:
				break;
		}
	}
}

AST_Node *parse_function(Parser *parser) {
	expect_token(++parser->cur_token, TOK_ID);
	expect_token(parser->cur_token+1, TOK_LBRA);
	int br_cnt = 0;
	bool fl = true;

	AST_Node *fdn = ast_alloc((AST_Node){0});
	fdn->type = AST_FUNC_DEF;

	while (fl) {
		switch (parser->cur_token->type++) {
			case TOK_LBRA:
				br_cnt++;
				break;

			case TOK_RBRA:
				br_cnt--;
				if (br_cnt == 0)
					fl = false;
				break;

			case TOK_ID:
				expect_token(parser->cur_token+1, TOK_TYPE);
				da_append(&fdn->payload.func_def.args, ast_alloc((AST_Node){
							.type = AST_FUNC_DEF_ARG,
							.payload.func_def_arg.id = parser->cur_token->data,
							.payload.func_def_arg.type = (parser->cur_token+1)->data
							}));
				break;

			default:
				// TODO: error handling
				break;
		}
	}

	Symbol fds = {
		.type = SBL_FUNC_DEF,
		.id = fdn->payload.func_def.id,
	};

	for (size_t i = 0; i < fdn->payload.func_def.args.count; i++) {
		da_append(&fds.payload.func_def.args, da_get(&fdn->payload.func_def.args, i));
	}

	da_append(&parser->st, fds);
	return fdn;
}

void parser_parse(Parser *parser) {
	while (true) {
		switch (parser->cur_token->type) {
			case TOK_FUNC:
				parse_function(parser);
				break;

			default:
				// TODO: error handling
				break;
		}
	}
}
