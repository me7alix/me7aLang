#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <threads.h>
#include <string.h>

#include "../include/parser.h"

double parse_float(char *data) { return atof(data); }
int64_t parse_int(char *data) {
	char *end;
	return strtol(data, &end, 0);
}

float op_cost(AST_ExprOp op, bool is_left) {
	switch (op) {
		case AST_OP_ADD: case AST_OP_SUB:
			if (is_left) return 1.1;
			else         return 1.0;
		case AST_OP_MUL: case AST_OP_DIV:
			if (is_left) return 2.1;
			else         return 2.0;
		case AST_OP_EQ: case AST_OP_GREAT:
		case AST_OP_LESS: case AST_OP_GREAT_EQ:
		case AST_OP_LESS_EQ: case AST_OP_NOT_EQ:
			if (is_left) return 0.9;
			else         return 0.8;
		case AST_OP_AND: case AST_OP_OR:
			if (is_left) return 0.6;
			else         return 0.5;
		case AST_OP_CAST:
			if (is_left) return 0.0;
			else         return 3.0;
		case AST_OP_NOT:
		case AST_OP_NEG: case AST_OP_SIZEOF:
		case AST_OP_REF: case AST_OP_DEREF:
			if (is_left) return 3.0;
			else         return 0.0;
		case AST_OP_VAR_EQ:
		case AST_OP_ADD_EQ:
		case AST_OP_SUB_EQ:
		case AST_OP_DIV_EQ:
		case AST_OP_MUL_EQ:
			if (is_left) return -1.0;
			else         return -1.1;
		default:         break;
	}

	return 0.0;
}

AST_Node *parse_func_call(Parser *p) {
	AST_Node *fcn = ast_new({ .kind = AST_FUNC_CALL });
	fcn->loc = parser_peek(p)->loc;
	fcn->func_call.id = parser_next(p)->data;
	expect_token(parser_peek(p), TOK_OPAR);
	parser_next(p);

	Symbol *fc = parser_symbols_get(p, fcn->func_call.id);
	if (!fc) lexer_error(fcn->loc, "error: calling an undeclared function");

	AST_Nodes fargs;
	switch (fc->type) {
		case SBL_FUNC_DEF:
			fcn->func_call.type = fc->func_def.type;
			fargs = fc->func_def.args;
			break;

		case SBL_FUNC_EXTERN:
			fcn->func_call.type = fc->func_extern.type;
			fcn->func_call.id = fc->func_extern.extern_smb;
			fargs = fc->func_extern.args;
			break;

		default: unreachable;
	}

	size_t arg_cnt = 0;
	while (parser_peek(p)->type != TOK_CPAR) {
		if (arg_cnt + 1 > fargs.count) lexer_error(fcn->loc, "error: wrong number of arguments");
		Type farg_type = fargs.items[arg_cnt++]->func_def_arg.type;
		AST_Node *expr = parse_expr(p, EXPR_PARSING_FUNC_CALL, &farg_type);
		Type expr_type = parser_get_type(p, expr);

		if (!compare_types(expr_type, farg_type))
			lexer_error(expr->loc, "error: wrong type");

		da_append(&fcn->func_call.args, expr);
		parser_next(p);
	}

	if (arg_cnt < fargs.count) lexer_error(fcn->loc, "error: wrong number of arguments");
	parser_next(p);
	return fcn;
}

AST_Node *expr_expand(AST_Nodes *nodes) {
	size_t cur_count = nodes->count;
	if (nodes->count == 1) {
		return da_get(nodes, 0);
	}

	for (size_t i = 0; i < nodes->count; i++) {
		if (nodes->count == 1) break;
		AST_Node *node = da_get(nodes, i);
		bool is_bin_op = node->kind == AST_BIN_EXP && !(node->exp_binary.l && node->exp_binary.r);
		bool is_un_op = node->kind == AST_UN_EXP && !node->exp_unary.v;
		if (!is_bin_op && !is_un_op) {
			bool is_lelf = false;
			float l_cost = -999, r_cost = -999;

			if (i != 0) {
				switch (da_get(nodes, i-1)->kind) {
					case AST_BIN_EXP: l_cost = op_cost(da_get(nodes, i-1)->exp_binary.op, true); break;
					case AST_UN_EXP:  l_cost = op_cost(da_get(nodes, i-1)->exp_unary.op, true);  break;
					default: unreachable;
				}
			}

			if (i != nodes->count-1) {
				switch (da_get(nodes, i+1)->kind) {
					case AST_BIN_EXP: r_cost = op_cost(da_get(nodes, i+1)->exp_binary.op, false); break;
					case AST_UN_EXP:  r_cost = op_cost(da_get(nodes, i+1)->exp_unary.op, false);  break;
					default: unreachable;
				}
			}

			if (l_cost > r_cost) is_lelf = true;
			if (l_cost < -500 && r_cost < -500) lexer_error(da_get(nodes, i)->loc, "error: wrong expression");

			if (is_lelf) {
				switch (da_get(nodes, i-1)->kind) {
					case AST_BIN_EXP: da_get(nodes, i-1)->exp_binary.r = node; break;
					case AST_UN_EXP:  da_get(nodes, i-1)->exp_unary.v = node;  break;
					default: unreachable;
				}
			} else {
				switch (da_get(nodes, i+1)->kind) {
					case AST_BIN_EXP: da_get(nodes, i+1)->exp_binary.l = node; break;
					case AST_UN_EXP:  da_get(nodes, i+1)->exp_unary.v = node;  break;
					default: unreachable;
				}
			}

			da_remove_ordered(nodes, i);
			i--;
		}
	}

	if (nodes->count == cur_count)
		lexer_error(da_get(nodes, 0)->loc, "error: wrong expression");
	return expr_expand(nodes);
}

Type expr_calc_types(Parser *parser, AST_Node *expr, Type *vart) {
	switch (expr->kind) {
		case AST_VAR: return parser_symbols_get(parser, expr->var_id)->variable.type;
		case AST_FUNC_CALL: return expr->func_call.type;

		case AST_LITERAL: {
			if (vart) {
				expr->literal.type = *vart;
			} else {
				switch (expr->literal.kind) {
					case LIT_INT:
						if(expr->literal.type.kind == TYPE_NULL)
							expr->literal.type = (Type) {.kind = TYPE_INT};
						break;
					case LIT_CHAR:  expr->literal.type = (Type) {.kind = TYPE_I8}; break;
					case LIT_FLOAT: expr->literal.type = (Type) {.kind = TYPE_F32}; break;
					case LIT_BOOL:  expr->literal.type = (Type) {.kind = TYPE_BOOL}; break;
				}
			}

			return expr->literal.type;
		} break;

		case AST_BIN_EXP: {
			Type lt = expr_calc_types(parser, expr->exp_binary.l, vart);
			if (expr->exp_binary.op == AST_OP_VAR_EQ) vart = &lt;
			else if (expr->exp_binary.op == AST_OP_ADD_EQ) vart = &lt;
			else if (expr->exp_binary.op == AST_OP_SUB_EQ) vart = &lt;
			else if (expr->exp_binary.op == AST_OP_MUL_EQ) vart = &lt;
			else if (expr->exp_binary.op == AST_OP_DIV_EQ) vart = &lt;
			else if (is_pointer(lt)) {
				Type iptr = (Type) {.kind = TYPE_IPTR};
				vart = &iptr;
			}

			Type rt = expr_calc_types(parser, expr->exp_binary.r, vart);
			expr->exp_binary.type = lt;

			if ((lt.kind == TYPE_IPTR && is_pointer(rt)) ||
				(is_pointer(lt) && rt.kind == TYPE_IPTR)) {
				Type ptr_type = is_pointer(lt) ? lt : rt;
				expr->exp_binary.type = (Type) {
					.kind = TYPE_POINTER,
					.pointer.base = get_pointer_base(ptr_type)
				};
			} else if (!compare_types(lt, rt)) {
				lexer_error(expr->loc, "error: operation on different types");
			}

			switch (expr->exp_binary.op) {
				case AST_OP_EQ: case AST_OP_NOT_EQ:
				case AST_OP_LESS_EQ: case AST_OP_GREAT_EQ:
				case AST_OP_GREAT: case AST_OP_LESS:
					expr->exp_binary.type.kind = TYPE_BOOL;
				default:;
			}

			return expr->exp_binary.type;
		} break;

		case AST_UN_EXP: {
			switch (expr->exp_unary.op) {
				case AST_OP_SIZEOF: break;

				case AST_OP_CAST: {
					expr_calc_types(parser, expr->exp_unary.v, &expr->exp_unary.type);
				} break;

				case AST_OP_REF: {
					Type vt = expr_calc_types(parser, expr->exp_unary.v, vart);
					Type *base = malloc(sizeof(Type)); *base = vt;
					expr->exp_unary.type = (Type) {.kind = TYPE_POINTER, .pointer.base = base};
				} break;

				case AST_OP_DEREF: {
					Type vt = expr_calc_types(parser, expr->exp_unary.v, vart);
					expr->exp_unary.type = vt;
					if (!is_pointer(vt)) lexer_error(expr->exp_unary.v->loc, "error: pointer expected");
					expr->exp_unary.type = *vt.pointer.base;
				} break;

				default: {
					expr->exp_unary.type = expr_calc_types(parser, expr->exp_unary.v, vart);
				} break;
			}

			return expr->exp_unary.type;
		} break;

		default: unreachable;
	}
}

AST_ExprOp tok_to_binary_expr_op(TokenType tok) {
	switch (tok) {
		case TOK_PLUS_EQ:   return AST_OP_ADD_EQ;
		case TOK_MINUS_EQ:  return AST_OP_SUB_EQ;
		case TOK_STAR_EQ:   return AST_OP_MUL_EQ;
		case TOK_SLASH_EQ:  return AST_OP_DIV_EQ;
		case TOK_EQ_EQ:     return AST_OP_EQ;
		case TOK_EQ:        return AST_OP_VAR_EQ;
		case TOK_GREAT:     return AST_OP_GREAT;
		case TOK_LESS:      return AST_OP_LESS;
		case TOK_GREAT_EQ:  return AST_OP_GREAT_EQ;
		case TOK_LESS_EQ:   return AST_OP_LESS_EQ;
		case TOK_AND:       return AST_OP_AND;
		case TOK_OR:        return AST_OP_OR;
		case TOK_PLUS:      return AST_OP_ADD;
		case TOK_MINUS:     return AST_OP_SUB;
		case TOK_STAR:      return AST_OP_MUL;
		case TOK_SLASH:     return AST_OP_DIV;
		default: unreachable;
	}
}

AST_ExprOp tok_to_unary_expr_op(Token *tok) {
	switch (tok->type) {
		case TOK_SIZEOF:    return AST_OP_SIZEOF;
		case TOK_COL:       return AST_OP_CAST;
		case TOK_STAR:      return AST_OP_DEREF;
		case TOK_AMP:       return AST_OP_REF;
		case TOK_EXC:       return AST_OP_NOT;
		case TOK_MINUS:     return AST_OP_NEG;
		default:
			lexer_error(tok->loc, "error: wrong operation");
			return 0;
	};
}

AST_Node *parse_expr(Parser *p, ExprParsingType type, Type *vart) {
	AST_Nodes nodes = {0};

	while (true) {
		if (parser_peek(p)->type == TOK_OPAR) {
			parser_next(p);
			da_append(&nodes, parse_expr(p, EXPR_PARSING_PAR, vart));
		}

		if (type == EXPR_PARSING_FUNC_CALL) {
			if (parser_peek(p)->type == TOK_COM) break;
			else if (parser_peek(p)->type == TOK_CPAR) {
				p->cur_token--;
				break;
			}
		} else if (type == EXPR_PARSING_VAR) {
			if (parser_peek(p)->type == TOK_SEMI) {
				break;
			}
		} else if (type == EXPR_PARSING_PAR) {
			if (parser_peek(p)->type == TOK_CPAR) {
				parser_next(p);
				break;
			}
		} else if (type == EXPR_PARSING_STMT) {
			if (parser_peek(p)->type == TOK_OBRA) {
				p->cur_token--;
				break;
			}
		}

		switch (parser_peek(p)->type) {
			case TOK_ID:
				if ((parser_peek(p)+1)->type != TOK_OPAR) {
					Symbol *var = parser_symbols_get(p, parser_peek(p)->data);
					if (!var)
						lexer_error(parser_peek(p)->loc, "error: no such variable in the scope");
					if (var->type != SBL_VAR)
						lexer_error(parser_peek(p)->loc, "error: no such variable in the scope");

					da_append(&nodes, ast_new({
						.kind = AST_VAR,
						.loc = parser_peek(p)->loc,
						.var_id = parser_peek(p)->data
					}));
				} else {
					da_append(&nodes, parse_func_call(p));
					p->cur_token--;
				}
				break;

			case TOK_INT:
				da_append(&nodes, ast_new({
					.kind = AST_LITERAL,
					.literal.kind = LIT_INT,
					.loc = parser_peek(p)->loc,
					.literal.lint = parse_int(parser_peek(p)->data),
				}));
				break;

			case TOK_TRUE:
				da_append(&nodes, ast_new({
					.kind = AST_LITERAL,
					.loc = parser_peek(p)->loc,
					.literal.kind = LIT_BOOL,
					.literal.lint = 1,
				}));
				break;

			case TOK_FALSE:
				da_append(&nodes, ast_new({
					.kind = AST_LITERAL,
					.literal.kind = LIT_BOOL,
					.literal.lint = 0,
				}));
				break;

			case TOK_NULL: {
				Type *u0 = malloc(sizeof(Type));
				*u0 = (Type) {.kind = TYPE_NULL};
				da_append(&nodes, ast_new({
					.kind = AST_LITERAL,
					.literal.type = (Type) { .kind = TYPE_POINTER, .pointer.base = u0},
					.literal.kind = LIT_INT,
					.literal.lint = 0,
				}));
			} break;

			case TOK_CHAR:
				da_append(&nodes, ast_new({
					.kind = AST_LITERAL,
					.loc = parser_peek(p)->loc,
					.literal.kind = LIT_CHAR,
					.literal.lint = parser_peek(p)->data[0],
				}));
				break;

			case TOK_FLOAT:
				da_append(&nodes, ast_new({
					.kind = AST_LITERAL,
					.literal.kind = LIT_FLOAT,
					.literal.lfloat = parse_float(parser_peek(p)->data),
				}));
				break;

			case TOK_COL: {
				Type t = parse_type(p);
				da_append(&nodes, ast_new({
					.kind = AST_UN_EXP,
					.loc = parser_peek(p)->loc,
					.exp_unary.type = t,
					.exp_unary.op = AST_OP_CAST,
					.exp_unary.v = NULL,
				}));
			} break;

			case TOK_SIZEOF:
				if (parser_looknext(p)->type == TOK_COL) {
					da_append(&nodes, ast_new({
						.kind = AST_UN_EXP,
						.loc = parser_peek(p)->loc,
						.exp_unary.op = tok_to_unary_expr_op(parser_next(p)),
						.exp_unary.type = (Type) {.kind = TYPE_IPTR},
						.exp_unary.v = ast_new({
							.kind = AST_LITERAL,
							.literal.type = parse_type(p),
						}),
					}));
				} else {
					da_append(&nodes, ast_new({
						.kind = AST_UN_EXP,
						.loc = parser_peek(p)->loc,
						.exp_unary.op = tok_to_unary_expr_op(parser_peek(p)),
						.exp_unary.type = (Type) {.kind = TYPE_IPTR},
						.exp_unary.v = NULL,
					}));
				}
				break;

			case TOK_EXC: case TOK_AMP:
				da_append(&nodes, ast_new({
					.kind = AST_UN_EXP,
					.loc = parser_peek(p)->loc,
					.exp_unary.op = tok_to_unary_expr_op(parser_peek(p)),
					.exp_unary.v = NULL,
				}));
				break;

			case TOK_PLUS_EQ: case TOK_MINUS_EQ:
			case TOK_STAR_EQ: case TOK_SLASH_EQ:
			case TOK_NOT_EQ: case TOK_OR:
			case TOK_LESS: case TOK_GREAT:
			case TOK_LESS_EQ: case TOK_GREAT_EQ:
			case TOK_EQ_EQ: case TOK_AND:
			case TOK_PLUS: case TOK_SLASH:
			case TOK_EQ: {
				da_append(&nodes, ast_new({
					.kind = AST_BIN_EXP,
					.loc = parser_peek(p)->loc,
					.exp_binary.op = tok_to_binary_expr_op(parser_peek(p)->type),
					.exp_binary.l = NULL,
					.exp_binary.r = NULL
				}));
			} break;

			case TOK_STAR: case TOK_MINUS: {
				bool is_unary_op = false;
				if (nodes.count == 0) {
					is_unary_op = true;
				} else {
					if (da_last(&nodes)->kind == AST_UN_EXP)
						lexer_error(parser_peek(p)->loc,
				  "error: invalid operator combination\n"
				  "hint: try to use parenthesis");

					bool is_bin_op = da_last(&nodes)->kind == AST_BIN_EXP;
					if (is_bin_op && da_last(&nodes)->exp_binary.l && da_last(&nodes)->exp_binary.r) is_bin_op = false;
					if (is_bin_op) {
						is_unary_op = true;
					}
				}

				if (!is_unary_op) {
					da_append(&nodes, ast_new({
						.kind = AST_BIN_EXP,
						.loc = parser_peek(p)->loc,
						.exp_binary.op = tok_to_binary_expr_op(parser_peek(p)->type),
						.exp_binary.l = NULL,
						.exp_binary.r = NULL
					}));
				} else {
					da_append(&nodes, ast_new({
						.kind = AST_UN_EXP,
						.loc = parser_peek(p)->loc,
						.exp_unary.op = tok_to_unary_expr_op(parser_peek(p)),
						.exp_unary.v = NULL,
					}));
				}
			} break;

			default: lexer_error(parser_peek(p)->loc, "error: unexpected token");
		}

		parser_next(p);
	}

	AST_Node *expr = expr_expand(&nodes);
	expr_calc_types(p, expr, vart);
	da_free(&nodes);
	return expr;
}
