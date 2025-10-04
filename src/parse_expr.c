#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <threads.h>

#include "../include/parser.h"

int64_t parse_int(char *data) { return atol(data); }
double parse_float(char *data) { return atof(data); }

float op_cost(TokenType op, bool is_left) {
	switch (op) {
		case TOK_PLUS: case TOK_MINUS:
			if (is_left) return 1.1;
			else         return 1.0;
		case TOK_STAR: case TOK_SLASH:
			if (is_left) return 2.1;
			else         return 2.0;
		case TOK_EQ_EQ: case TOK_LESS:
		case TOK_LESS_EQ: case TOK_GREAT:
		case TOK_GREAT_EQ: case TOK_NOT_EQ:
			if (is_left) return 0.9;
			else         return 0.8;
		case TOK_AND: case TOK_OR:
			if (is_left) return 0.6;
			else         return 0.5;
		case TOK_COL:
			if (is_left) return 0.0;
			else         return 3.0;
		default:         break;
	}

	return 0.0;
}

AST_Node *parse_func_call(Parser *parser) {
	AST_Node *fcn = ast_new(.type = AST_FUNC_CALL);
	fcn->func_call.id = parser->cur_token->data;
	expect_token(++parser->cur_token, TOK_OPAR);
	parser->cur_token++;

	while (parser->cur_token->type != TOK_EOF) {
		switch (parser->cur_token->type) {
			case TOK_CPAR:
				parser->cur_token++;
				return fcn;
			case TOK_ID: case TOK_OPAR:
			case TOK_FLOAT: case TOK_INT: case TOK_CHAR:
				da_append(&fcn->func_call.args, parse_expr(parser, EXPR_PARSING_FUNC_CALL, NULL));
				break;
			default: unexpect_token(parser->cur_token, parser->cur_token->type); break;
		}

		parser->cur_token++;
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
		bool is_bin_op = node->type == AST_BIN_EXP && !(node->exp_binary.l && node->exp_binary.r);
		bool is_un_op = node->type == AST_UN_EXP && !node->exp_unary.v;
		if (!is_bin_op && !is_un_op) {
			bool is_lelf = false;
			float l_cost = 0, r_cost = 0;

			if (i != 0) {
				switch (da_get(nodes, i-1)->type) {
					case AST_BIN_EXP: l_cost = op_cost(da_get(nodes, i-1)->exp_binary.op, true); break;
					case AST_UN_EXP:  l_cost = op_cost(da_get(nodes, i-1)->exp_unary.op, true);  break;
						default: unreachable;
				}
			}

			if (i != nodes->count-1) {
				switch (da_get(nodes, i+1)->type) {
					case AST_BIN_EXP: r_cost = op_cost(da_get(nodes, i+1)->exp_binary.op, false); break;
					case AST_UN_EXP:  r_cost = op_cost(da_get(nodes, i+1)->exp_unary.op, false);  break;
						default: unreachable;
				}
			}

			if (l_cost > r_cost) is_lelf = true;
			if (l_cost == 0 && r_cost == 0) assert(!"wrong expression");

			if (is_lelf) {
				switch (da_get(nodes, i-1)->type) {
					case AST_BIN_EXP: da_get(nodes, i-1)->exp_binary.r = node; break;
					case AST_UN_EXP:  da_get(nodes, i-1)->exp_unary.v = node;  break;
						default: unreachable;
				}
			} else {
				switch (da_get(nodes, i+1)->type) {
					case AST_BIN_EXP: da_get(nodes, i+1)->exp_binary.l = node; break;
					case AST_UN_EXP:  da_get(nodes, i+1)->exp_unary.v = node;  break;
						default: unreachable;
				}
			}

			da_ordered_remove(nodes, i);
			i--;
		}
	}

	return expr_expand(nodes);
}

Type expr_calc_types(Parser *parser, AST_Node *expr, Type *vart) {
	switch (expr->type) {
		case AST_VAR: return parser_st_get(parser, expr->var_id)->variable.type;

		case AST_LITERAL: {
			if (vart != NULL) {
				expr->literal.type = *vart;
			} else {
				switch (expr->literal.kind) {
					case LIT_INT:   expr->literal.type = (Type) {.kind = TYPE_INT, .size = 4}; break;
					case LIT_CHAR:  expr->literal.type = (Type) {.kind = TYPE_I8, .size = 1}; break;
					case LIT_FLOAT: expr->literal.type = (Type) {.kind = TYPE_FLOAT, .size = 4}; break;
					case LIT_BOOL:  expr->literal.type = (Type) {.kind = TYPE_BOOL, .size = 1}; break;
				}
			}

			return expr->literal.type;
		} break;

		case AST_BIN_EXP: {
			Type lt = expr_calc_types(parser, expr->exp_binary.l, vart);
			Type rt = expr_calc_types(parser, expr->exp_binary.r, vart);

			if (lt.kind != rt.kind) {
				assert(!"wrong types in the expression");
			}

			expr->exp_binary.type = rt;

			if (expr->exp_binary.op == TOK_EQ_EQ ||
				expr->exp_binary.op == TOK_NOT_EQ ||
				expr->exp_binary.op == TOK_GREAT_EQ ||
				expr->exp_binary.op == TOK_LESS_EQ ||
				expr->exp_binary.op == TOK_GREAT ||
				expr->exp_binary.op == TOK_LESS) {
				expr->exp_binary.type.kind = TYPE_BOOL;
				expr->exp_binary.type.size = 1;
			}

			return expr->exp_binary.type;
		} break;

		case AST_UN_EXP: {
			Type vt = expr_calc_types(parser, expr->exp_unary.v, &expr->exp_unary.type);
			(void) vt;

			if (expr->exp_unary.op == TOK_COL) {
				return expr->exp_unary.type;
			}

			unreachable;
			return (Type) {0};
		} break;

		default: unreachable;
	}
}

AST_Node *parse_expr(Parser *parser, ExprParsingType type, Type *vart) {
	AST_Nodes nodes = {0};

	while (true) {
		if (parser->cur_token->type == TOK_OPAR) {
			parser->cur_token++;
			da_append(&nodes, parse_expr(parser, EXPR_PARSING_PAR, vart));
		}

		if (type == EXPR_PARSING_FUNC_CALL) {
			if (parser->cur_token->type == TOK_COM) break;
			else if (parser->cur_token->type == TOK_CPAR) {
				parser->cur_token--;
				break;
			}
		} else if (type == EXPR_PARSING_VAR) {
			if (parser->cur_token->type == TOK_SEMI) {
				break;
			}
		} else if (type == EXPR_PARSING_PAR) {
			if (parser->cur_token->type == TOK_CPAR) {
				parser->cur_token++;
				break;
			}
		} else if (type == EXPR_PARSING_STMT) {
			if (parser->cur_token->type == TOK_OBRA) break;
		}

		switch (parser->cur_token->type) {
			case TOK_ID:
				if ((parser->cur_token+1)->type != TOK_OPAR) {
					da_append(&nodes, ast_new(
						.type = AST_VAR,
						.var_id = parser->cur_token->data
					));
				} else {
					da_append(&nodes, parse_func_call(parser));
				}
				break;

			case TOK_INT:
				da_append(&nodes, ast_new(
					.type = AST_LITERAL,
					.literal.kind = LIT_INT,
					.literal.lint = parse_int(parser->cur_token->data),
				));
				break;

			case TOK_CHAR:
				da_append(&nodes, ast_new(
					.type = AST_LITERAL,
					.literal.kind = LIT_CHAR,
					.literal.lint = parser->cur_token->data[0],
				));
				break;

			case TOK_FLOAT:
				da_append(&nodes, ast_new(
					.type = AST_LITERAL,
					.literal.kind = LIT_FLOAT,
					.literal.lfloat = parse_float(parser->cur_token->data),
				));
				break;

			case TOK_COL: {
				Type t = parse_type(parser);
				da_append(&nodes, ast_new(
					.type = AST_UN_EXP,
					.exp_unary.type = t,
					.exp_unary.op = TOK_COL,
					.exp_unary.v = NULL,
				));
			} break;

			case TOK_NOT_EQ: case TOK_OR:
			case TOK_LESS: case TOK_GREAT:
			case TOK_LESS_EQ: case TOK_GREAT_EQ:
			case TOK_EQ_EQ: case TOK_AND:
			case TOK_PLUS: case TOK_MINUS:
			case TOK_STAR: case TOK_SLASH:
				da_append(&nodes, ast_new(
					.type = AST_BIN_EXP,
					.exp_binary.op = parser->cur_token->type,
					.exp_binary.l = NULL,
					.exp_binary.r = NULL
				));
				break;

			default:
				unexpect_token(parser->cur_token, parser->cur_token->type);
				break;
		}

		parser->cur_token++;
	}

	AST_Node *expr = expr_expand(&nodes);
	expr_calc_types(parser, expr, vart);
	da_free(&nodes);
	return expr;
}
