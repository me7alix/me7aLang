#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <threads.h>

#include "../include/parser.h"

int64_t parse_int(char *data) { return atol(data); }
double parse_float(char *data) { return atof(data); }

float op_cost(char *op, bool is_left) {
	switch (*op) {
		case '+': case '-':
			if (is_left) return 1.1;
			else         return 1.0;
		case '*': case '/':
			if (is_left) return 2.1;
			else         return 2.0;
	}

	if (strcmp(op, "==") == 0) {
		if (is_left) return 0.9;
		else         return 0.9;
	} else if (strcmp(op, ">=") == 0) {
		if (is_left) return 0.9;
		else         return 0.9;
	} else if (strcmp(op, "<=") == 0) {
		if (is_left) return 0.9;
		else         return 0.9;
	} else if (strcmp(op, "&&") == 0) {
		if (is_left) return 0.9;
		else         return 0.9;
	} else if (strcmp(op, "||") == 0) {
		if (is_left) return 0.9;
		else         return 0.9;
	}

	return 0.0;
}

AST_Node *parse_func_call(Parser *parser) {
	AST_Node *fcn = ast_alloc((AST_Node){.type = AST_FUNC_CALL});
	fcn->func_call.id = parser->cur_token->data;
	expect_token(parser->cur_token++, TOK_OPAR);

	while (parser->cur_token->type != TOK_EOF) {
		switch ((parser->cur_token++)->type) {
			case TOK_CPAR:
				return fcn;
			case TOK_ID: case TOK_OPAR:
			case TOK_FLOAT: case TOK_INT:
				da_append(&fcn->func_call.args, parse_expr(parser, EXPR_PARSING_FUNC_CALL));
				break;
			default: /* TODO: error handling */ break;
		}
	}

	return fcn;
}

AST_Node *expr_expand(AST_Nodes *nodes) {
	if (nodes->count == 1) {
		return da_get(nodes, 0);
	}

	for (size_t i = 0; i < nodes->count; i++) {
		if (nodes->count == 1) break;
		AST_Node *node = da_get(nodes, i);
		if (!(node->type == AST_BIN_EXP && !(node->exp_binary.l && node->exp_binary.r))) {
			bool is_lelf = false;
			float l_cost = 0, r_cost = 0;

			if (i != 0) l_cost = op_cost(da_get(nodes, i-1)->exp_binary.op, true);
			if (i != nodes->count-1) r_cost = op_cost(da_get(nodes, i+1)->exp_binary.op, false);

			if (l_cost > r_cost) is_lelf = true;

			if (l_cost == 0 && r_cost == 0) /* TODO: error handling */ (void)0;

			if (is_lelf) da_get(nodes, i-1)->exp_binary.r = node;
			else da_get(nodes, i+1)->exp_binary.l = node;

			da_ordered_remove(nodes, i);
			i--;
		}
	}

	return expr_expand(nodes);
}

Type *expr_calc_types(Parser *parser, AST_Node *expr) {
	if (expr->type == AST_INT) {
		Type *t = malloc(sizeof(Type));
		*t = (Type) {
			.kind = TYPE_INT,
			.size = 4,
		};
		return t;
	}

	if (expr->type == AST_FLOAT) {
		Type *t = malloc(sizeof(Type));
		*t = (Type) {
			.kind = TYPE_FLOAT,
			.size = 4,
		};
		return t;
	}

	if (expr->type == AST_VAR) {
		Symbol *v = parser_st_get(parser, expr->var_id);
		return v->variable.type;
	}

	Type *lt = expr_calc_types(parser, expr->exp_binary.l);
	Type *rt = expr_calc_types(parser, expr->exp_binary.r);

	if (lt->kind != rt->kind) {
		fprintf(stderr, "wrong types in exression\n");
		exit(1);
	}

	expr->exp_binary.type = lt;
	return lt;
}

AST_Node *parse_expr(Parser *parser, ExprParsingType type) {
	AST_Nodes nodes = {0};

	while (true) {
		if (parser->cur_token->type == TOK_OPAR) {
			parser->cur_token++;
			da_append(&nodes, parse_expr(parser, EXPR_PARSING_BRA));
		}

		if (type == EXPR_PARSING_FUNC_CALL) {
			if (parser->cur_token->type == TOK_COM ||
				parser->cur_token->type == TOK_CPAR) {
				break;
			}
		} else if (type == EXPR_PARSING_VAR) {
			if (parser->cur_token->type == TOK_SEMI) {
				break;
			}
		} else if (type == EXPR_PARSING_BRA) {
			if (parser->cur_token->type == TOK_CPAR) {
				parser->cur_token++;
				break;
			}
		}

		switch (parser->cur_token->type) {
			case TOK_ID:
				if ((parser->cur_token + 1)->type != TOK_OPAR) {
					da_append(&nodes, ast_alloc((AST_Node){
								.type = AST_VAR,
								.var_id = parser->cur_token->data
								}));
				} else {
					da_append(&nodes, parse_func_call(parser));
				}
				break;

			case TOK_INT:
				da_append(&nodes, ast_alloc((AST_Node){
							.type = AST_INT,
							.num_int = parse_int(parser->cur_token->data)
							}));
				break;

			case TOK_FLOAT:
				da_append(&nodes, ast_alloc((AST_Node){
							.type = AST_FLOAT,
							.num_float = parse_float(parser->cur_token->data)
							}));
				break;

			case TOK_PLUS: case TOK_MINUS:
			case TOK_STAR: case TOK_SLASH:
				da_append(&nodes, ast_alloc((AST_Node){
							.type = AST_BIN_EXP,
							.exp_binary.op = parser->cur_token->data,
							.exp_binary.l = NULL,
							.exp_binary.r = NULL
							}));
				break;

			default:
				printf("expression parsing error: wrong token %s\n", parser->cur_token->data);
				break;
		}

		parser->cur_token++;
	}

	AST_Node *expr = expr_expand(&nodes);
	expr_calc_types(parser, expr);
	da_free(&nodes);
	return expr;
}
