#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define HASHMAP_IMPLEMENTATION
#include "../include/parser.h"
#include "../src/parser_expr.c"

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

void ast_block_add_node(AST_Node *block, AST_Node node) {
	if (block->payload.block.tail != NULL)
		block->payload.block.tail->next = ast_alloc(node);
	else {
		block->payload.block.head = ast_alloc(node);
		block->payload.block.tail = block->payload.block.head;
	}
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

				ast_block_add_node(parent, (AST_Node){
						.type = AST_VARIABLE,
						.payload.variable.name = (parser->cur_token-1)->data,
						.payload.variable.type = parser->cur_token->data, 
						.payload.variable.exp = parse_expression(parser, EXPR_PARSING_VAR),
						});

				break;

			case TOK_IF_SYM:
				expect_token(parser->cur_token+1, TOK_LBRA);

				break;

			default:
				break;
		}
	}
}

void parse_function(Parser *parser) {
	expect_token(++parser->cur_token, TOK_ID);
	expect_token(parser->cur_token+1, TOK_LBRA);
	int br_cnt = 0;

	AST_Node *func_def = ast_alloc((AST_Node){0});
	func_def->type = AST_FUNC_DEF;

	while (true) {
		switch (parser->cur_token->type++) {
			case TOK_LBRA:
				br_cnt++;
				break;

			case TOK_RBRA:
				br_cnt--;
				if (br_cnt == 0)
					return;
				break;

			case TOK_ID:
				expect_token(parser->cur_token+1, TOK_TYPE);
				func_def->payload.func_def.args = ast_alloc((AST_Node){
						.type = AST_FUNC_DEF_ARG,
						.payload.func_def_arg.name = parser->cur_token->data,
						.payload.func_def_arg.type = (parser->cur_token+1)->data,
						});
				break;

			default:
				// TODO: error handling
				break;
		}
	}

	da_append(&parser->st, ((Symbol){
			.type = SBL_FUNC_DEF,
			.id = func_def->payload.func_def.name,
			.payload.func_def.args = func_def->payload.func_def.args,
			}));
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
