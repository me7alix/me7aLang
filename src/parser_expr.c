#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <threads.h>

#include "../include/parser.h"

int64_t parse_int(char *data) {
	return atol(data);
}

double parse_float(char *data) {
	return atof(data);
}

float op_cost(char op, bool is_left) {
	switch (op) {
		case '+': case '-':
			if (is_left)
				return 1.1;
			else
				return 1.0;
		case '*': case '/':
			if (is_left)
				return 2.1;
			else
				return 2.0;
	}

	return 0.0;
}

void parse_func_call(Parser *parser, char **name, AST_Nodes *args) {
	*name = parser->cur_token->data;

	int br_cnt = 0;

	while (parser->cur_token->type != TOK_EOF) {
		switch ((parser->cur_token++)->type) {
			case TOK_RBRA: if (--br_cnt == 0) return;
			case TOK_ID: case TOK_INT: case TOK_LBRA: case TOK_FLOAT:
				if (parser->cur_token->type == TOK_LBRA) br_cnt++;
				da_append(args, parse_expr(parser, EXPR_PARSING_FUNC_CALL));
				break;
			default: /* TODO: error handling */ break;
		}
	}
}

AST_Node *expr_expand(AST_Nodes *nodes) {
	if (nodes->count == 1) {
		return da_get(nodes, 0);
	}

	for (size_t i = 0; i < nodes->count; i++) {
		if (nodes->count == 1) break;
		AST_Node *node = da_get(nodes, i);
		if (node->type == AST_INT || node->type == AST_FLOAT ||
		   (node->type == AST_BIN_EXP && node->payload.exp_binary.l && node->payload.exp_binary.r)) {
			bool is_lelf = false;
			float l_cost = 0, r_cost = 0;

			if (i != 0) l_cost = op_cost(da_get(nodes, i-1)->payload.exp_binary.op, true);
			if (i != nodes->count-1) r_cost = op_cost(da_get(nodes, i+1)->payload.exp_binary.op, false);

			if (l_cost > r_cost) is_lelf = true;

			if (l_cost == 0 && r_cost == 0) /* TODO: error handling */;

			if (is_lelf)
				da_get(nodes, i-1)->payload.exp_binary.r = node;
			else
				da_get(nodes, i+1)->payload.exp_binary.l = node;

			da_ordered_remove(nodes, i);
			i--;
		}
	}

	return expr_expand(nodes);
}

AST_Node *parse_expr(Parser *parser, ExprParsingType type) {
	AST_Nodes nodes = {0};

	while (true) {
		if (parser->cur_token->type == TOK_LBRA) {
			parser->cur_token++;
			da_append(&nodes, parse_expr(parser, EXPR_PARSING_VAR_BRA));
		}

		if (type == EXPR_PARSING_FUNC_CALL) {
			if (parser->cur_token->type == TOK_COM || parser->cur_token->type == TOK_RBRA) {
				break;
			}
		} else if (type == EXPR_PARSING_VAR) {
			if (parser->cur_token->type == TOK_SEMI) {
				break;
			}
		} else if (type == EXPR_PARSING_VAR_BRA) {
			if (parser->cur_token->type == TOK_RBRA) {
				break;
			}
		}

		switch (parser->cur_token->type) {
			case TOK_LBRA: case TOK_RBRA:
				break;
			case TOK_ID:
				if ((parser->cur_token + 1)->type != TOK_LBRA) {
					da_append(&nodes, ast_alloc((AST_Node){
								.type = AST_VAR,
								.payload.var_id = parser->cur_token->data}));
				} else {
					AST_Node *fcn = ast_alloc((AST_Node){.type = AST_FUNC_CALL});
					parse_func_call(parser, &fcn->payload.func_call.id, &fcn->payload.func_call.args);
					da_append(&nodes, fcn);
				}
				break;

			case TOK_INT:
				da_append(&nodes, ast_alloc((AST_Node){
						.type = AST_INT,
						.payload.num_int = parse_int(parser->cur_token->data)}));
				break;

			case TOK_FLOAT:
				da_append(&nodes, ast_alloc((AST_Node){
						.type = AST_FLOAT,
						.payload.num_float = parse_float(parser->cur_token->data)}));
				break;

			case TOK_PLUS: case TOK_MINUS:
			case TOK_STAR: case TOK_SLASH:
				da_append(&nodes, ast_alloc((AST_Node){
						.type = AST_BIN_EXP,
						.payload.exp_binary.op = parser->cur_token->data[0],
						.payload.exp_binary.l = NULL,
						.payload.exp_binary.r = NULL}));
				break;

			default:
				printf("expression parsing error: wrong token\n");
				break;
		}

		parser->cur_token++;
	}

	parser->cur_token++;
	AST_Node *expr = expr_expand(&nodes);
	da_free(&nodes);
	return expr;
}
