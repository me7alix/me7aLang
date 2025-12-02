#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "../include/parser.h"

Type TUPTR = {.kind = TYPE_UPTR};

double parse_float(char *data) {
	return atof(data);
}

long long parse_int(char *data) {
	char *end;
	return strtoll(data, &end, 0);
}

float expr_op_precedence(AST_ExprOp op, bool l) {
	switch (op) {
		case AST_OP_ADD:
		case AST_OP_SUB:
			if (l) return 1.1;
			else   return 1.0;
		case AST_OP_MOD:
		case AST_OP_MUL:
		case AST_OP_DIV:
			if (l) return 2.1;
			else   return 2.0;
		case AST_OP_EQ:      case AST_OP_GREAT:
		case AST_OP_LESS:    case AST_OP_GREAT_EQ:
		case AST_OP_LESS_EQ: case AST_OP_NOT_EQ:
			if (l) return 0.9;
			else   return 0.8;
		case AST_OP_AND:
		case AST_OP_OR:
			if (l) return 0.6;
			else   return 0.5;
		case AST_OP_CAST:
			if (l) return 0.0;
			else   return 5.0;
		case AST_OP_FIELD:
		case AST_OP_ARR:
			if (l) return 4.0;
			else   return 3.0;
		case AST_OP_SIZEOF:
			if (l) return 6.0;
			else   return 0.0;
		case AST_OP_NOT: case AST_OP_NEG: 
		case AST_OP_REF: case AST_OP_DEREF:
			if (l) return 3.0;
			else   return 0.0;
		case AST_OP_VAR_EQ:
		case AST_OP_ADD_EQ: case AST_OP_SUB_EQ:
		case AST_OP_DIV_EQ: case AST_OP_MUL_EQ:
			if (l) return -1.0;
			else   return -1.1;
		default:   break;
	}

	return 0.0;
}

AST_Node *expr_expand(AST_Nodes *nodes) {
	size_t cur_count = nodes->count;
	if (nodes->count == 1) {
		bool error = false;
		if (da_get(nodes, 0)->kind == AST_BIN_EXP) {
			if (!da_get(nodes, 0)->expr_binary.l ||
				!da_get(nodes, 0)->expr_binary.r) error = true;
		} else if (da_get(nodes, 0)->kind == AST_UN_EXP) {
			if (!da_get(nodes, 0)->expr_unary.v) error = true;
		}

		if (error) lexer_error(da_get(nodes, 0)->loc, "error: wrong expression");
		return da_get(nodes, 0);
	}

	for (size_t i = 0; i < nodes->count; i++) {
		if (nodes->count == 1) break;
		AST_Node *node = da_get(nodes, i);
		bool is_bin_op = node->kind == AST_BIN_EXP && !(node->expr_binary.l && node->expr_binary.r);
		bool is_un_op = node->kind == AST_UN_EXP && !node->expr_unary.v;
		if (!is_bin_op && !is_un_op) {
			bool is_lelf = false;
			float lp = -999, rp = -999;

			if (i != 0) {
				switch (da_get(nodes, i-1)->kind) {
					case AST_BIN_EXP:
						lp = expr_op_precedence(da_get(nodes, i-1)->expr_binary.op, true);
						break;
					case AST_UN_EXP:
						lp = expr_op_precedence(da_get(nodes, i-1)->expr_unary.op, true);
						break;
					default: UNREACHABLE;
				}
			}

			if (i != nodes->count-1) {
				switch (da_get(nodes, i+1)->kind) {
					case AST_BIN_EXP:
						rp = expr_op_precedence(da_get(nodes, i+1)->expr_binary.op, false);
						break;
					case AST_UN_EXP:
						rp = expr_op_precedence(da_get(nodes, i+1)->expr_unary.op, false);
						break;
					default: UNREACHABLE;
				}
			}

			if (lp > rp) is_lelf = true;
			if (lp < -500 && rp < -500)
				lexer_error(da_get(nodes, i)->loc, "error: wrong expression");

			if (is_lelf) {
				switch (da_get(nodes, i-1)->kind) {
					case AST_BIN_EXP: da_get(nodes, i-1)->expr_binary.r = node; break;
					case AST_UN_EXP:  da_get(nodes, i-1)->expr_unary.v = node;  break;
					default: UNREACHABLE;
				}
			} else {
				switch (da_get(nodes, i+1)->kind) {
					case AST_BIN_EXP: da_get(nodes, i+1)->expr_binary.l = node; break;
					case AST_UN_EXP:  da_get(nodes, i+1)->expr_unary.v = node;  break;
					default: UNREACHABLE;
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

bool type_is_int(Type t) {
	switch (t.kind) {
		case TYPE_INT:  case TYPE_UINT:
		case TYPE_I8:   case TYPE_U8:
		case TYPE_I16:  case TYPE_U16:
		case TYPE_I32:  case TYPE_U32:
		case TYPE_I64:  case TYPE_U64:
		case TYPE_IPTR: case TYPE_UPTR:
			return true;
		default:
			return false;
	}
}

Type expr_calc_types(Parser *p, AST_Node *expr, Type *vart) {
	switch (expr->kind) {
		case AST_FUNC_CALL: return expr->func_call.type;

		case AST_VID: {
			Symbol *var = parser_symbol_table_get(p, SBL_VAR, expr->vid);
			if (!var) lexer_error(expr->loc, "error: no such variable in the scope");
			return var->variable.type;
		} break;

		case AST_LITERAL: {
			if (vart && expr->literal.kind == LIT_INT) {
				expr->literal.type = *vart;
			} else {
				switch (expr->literal.kind) {
					case LIT_INT: {
						if(expr->literal.type.kind == TYPE_NULL)
							expr->literal.type = (Type) {.kind = TYPE_INT};
					} break;
					case LIT_CHAR:  expr->literal.type = (Type) {.kind = TYPE_I8};   break;
					case LIT_FLOAT: expr->literal.type = (Type) {.kind = TYPE_F32};  break;
					case LIT_BOOL:  expr->literal.type = (Type) {.kind = TYPE_BOOL}; break;
					case LIT_STR: {
						static Type ti8 = {.kind = TYPE_I8};
						expr->literal.type = (Type) {.kind = TYPE_POINTER, .pointer.base = &ti8};
					} break;
				}
			}

			return expr->literal.type;
		} break;

		case AST_BIN_EXP: {
			Type lt = expr_calc_types(p, expr->expr_binary.l, vart);
			switch (expr->expr_binary.op) {
				case AST_OP_VAR_EQ:
				case AST_OP_ADD_EQ:
				case AST_OP_SUB_EQ:
				case AST_OP_MUL_EQ:
				case AST_OP_DIV_EQ:
					vart = &lt;
					break;
				default: if(is_pointer(lt)) {
					vart = &TUPTR;
				} break;
			}

			if (expr->expr_binary.op == AST_OP_FIELD) {
				da_foreach (Field, field, &lt.user->ustruct.fields) {
					if (strcmp(expr->expr_binary.r->vid, field->id) == 0) {
						expr->expr_binary.type = field->type;
						return field->type;
					}
				}

				lexer_error(expr->expr_binary.l->loc, "error: the struct has no such field");
			}

			Type rt = expr_calc_types(p, expr->expr_binary.r, vart);
			expr->expr_binary.type = lt;

			if ((lt.kind == TYPE_IPTR && is_pointer(rt)) ||
				(is_pointer(lt) && rt.kind == TYPE_IPTR) ||
				(lt.kind == TYPE_UPTR && is_pointer(rt)) ||
				(is_pointer(lt) && rt.kind == TYPE_UPTR)) {
				Type ptr_type = is_pointer(lt) ? lt : rt;
				expr->expr_binary.type = (Type) {
					.kind = TYPE_POINTER,
					.pointer.base = get_pointer_base(ptr_type)
				};
			} else if (!compare_types(lt, rt)) {
				AST_Node *le = expr->expr_binary.l, *re = expr->expr_binary.r;
				if ((le->kind == AST_LITERAL || re->kind == AST_LITERAL) &&
					(type_is_int(lt) && type_is_int(rt))) {
					AST_Node *lit     = le->kind == AST_LITERAL ? le : re;
					AST_Node *not_lit = le->kind != AST_LITERAL ? le : re;
					lit->literal.type = parser_get_type(p, not_lit);
				} else lexer_error(expr->loc, "error: operation on different types");
			}

			switch (expr->expr_binary.op) {
				case AST_OP_EQ: case AST_OP_NOT_EQ:
				case AST_OP_LESS_EQ: case AST_OP_GREAT_EQ:
				case AST_OP_GREAT: case AST_OP_LESS:
					expr->expr_binary.type.kind = TYPE_BOOL;
				default:;
			}

			if (expr->expr_binary.op == AST_OP_ARR)
				expr->expr_binary.type = *expr->expr_binary.type.pointer.base;

			return expr->expr_binary.type;
		} break;

		case AST_UN_EXP: {
			switch (expr->expr_unary.op) {
				case AST_OP_SIZEOF: {
					expr_calc_types(p, expr->expr_unary.v, NULL);
				} break;

				case AST_OP_CAST: {
					expr_calc_types(p, expr->expr_unary.v, &expr->expr_unary.type);
				} break;

				case AST_OP_REF: {
					Type vt = expr_calc_types(p, expr->expr_unary.v, vart);
					Type *base = malloc(sizeof(Type)); *base = vt;
					expr->expr_unary.type = (Type) {.kind = TYPE_POINTER, .pointer.base = base};
				} break;

				case AST_OP_DEREF: {
					Type vt = expr_calc_types(p, expr->expr_unary.v, vart);
					expr->expr_unary.type = vt;
					if (!is_pointer(vt))
						lexer_error(expr->expr_unary.v->loc, "error: pointer expected");
					expr->expr_unary.type = *vt.pointer.base;
				} break;

				default:
					expr->expr_unary.type = expr_calc_types(p, expr->expr_unary.v, vart);
					break;
			}

			return expr->expr_unary.type;
		} break;

		default: UNREACHABLE;
	}
}

AST_ExprOp tok_to_binary_expr_op(TokenKind tok) {
	switch (tok) {
		case TOK_PLUS_EQ:  return AST_OP_ADD_EQ;
		case TOK_MINUS_EQ: return AST_OP_SUB_EQ;
		case TOK_STAR_EQ:  return AST_OP_MUL_EQ;
		case TOK_SLASH_EQ: return AST_OP_DIV_EQ;
		case TOK_EQ_EQ:    return AST_OP_EQ;
		case TOK_EQ:       return AST_OP_VAR_EQ;
		case TOK_GREAT:    return AST_OP_GREAT;
		case TOK_LESS:     return AST_OP_LESS;
		case TOK_GREAT_EQ: return AST_OP_GREAT_EQ;
		case TOK_LESS_EQ:  return AST_OP_LESS_EQ;
		case TOK_AND:      return AST_OP_AND;
		case TOK_OR:       return AST_OP_OR;
		case TOK_PLUS:     return AST_OP_ADD;
		case TOK_MINUS:    return AST_OP_SUB;
		case TOK_STAR:     return AST_OP_MUL;
		case TOK_SLASH:    return AST_OP_DIV;
		case TOK_PS:       return AST_OP_MOD;
		case TOK_OSQBRA:   return AST_OP_ARR;
		case TOK_DOT:      return AST_OP_FIELD;
		default: UNREACHABLE;
	}
}

AST_ExprOp tok_to_unary_expr_op(Token *tok) {
	switch (tok->kind) {
		case TOK_SIZEOF: return AST_OP_SIZEOF;
		case TOK_COL:    return AST_OP_CAST;
		case TOK_STAR:   return AST_OP_DEREF;
		case TOK_AMP:    return AST_OP_REF;
		case TOK_EXC:    return AST_OP_NOT;
		case TOK_MINUS:  return AST_OP_NEG;
		default:
			lexer_error(tok->loc, "error: wrong operation");
			return 0;
	};
}

AST_Node *parse_expr(Parser *p, ExprParsingType type, Type *vart) {
	AST_Nodes nodes = {0};

	while (true) {
		if (parser_peek(p)->kind == TOK_OPAR) {
			parser_next(p);
			da_append(&nodes, parse_expr(p, EXPR_PARSING_PAR, vart));
		}

		if (type == EXPR_PARSING_FUNC_CALL) {
			if (parser_peek(p)->kind == TOK_COM) break;
			else if (parser_peek(p)->kind == TOK_CPAR) {
				p->cur_token--;
				break;
			}
		} else if (type == EXPR_PARSING_VAR) {
			if (parser_peek(p)->kind == TOK_SEMI) {
				break;
			}
		} else if (type == EXPR_PARSING_SQBRA) {
			if (parser_peek(p)->kind == TOK_CSQBRA) {
				parser_next(p);
				break;
			}
		} else if (type == EXPR_PARSING_PAR) {
			if (parser_peek(p)->kind == TOK_CPAR) {
				parser_next(p);
				break;
			}
		} else if (type == EXPR_PARSING_STMT) {
			if (parser_peek(p)->kind == TOK_OBRA) {
				p->cur_token--;
				break;
			}
		}

		switch (parser_peek(p)->kind) {
			case TOK_ID:
				if (parser_looknext(p)->kind == TOK_OPAR) {
					da_append(&nodes, parse_func_call(p));
					p->cur_token--;
				} else {
					da_append(&nodes, ast_new({
						.kind = AST_VID,
						.loc = parser_peek(p)->loc,
						.vid = parser_peek(p)->data
					}));
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

			case TOK_STRING:
				da_append(&nodes, ast_new({
					.kind = AST_LITERAL,
					.loc = parser_peek(p)->loc,
					.literal.kind = LIT_STR,
					.literal.str = parser_peek(p)->data,
				}));
				break;

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
					.expr_unary.type = t,
					.expr_unary.op = AST_OP_CAST,
					.expr_unary.v = NULL,
				}));
			} break;

			case TOK_SIZEOF:
				if (parser_looknext(p)->kind == TOK_COL) {
					da_append(&nodes, ast_new({
						.kind = AST_UN_EXP,
						.loc = parser_peek(p)->loc,
						.expr_unary.op = tok_to_unary_expr_op(parser_next(p)),
						.expr_unary.type = TUPTR,
						.expr_unary.v = ast_new({
							.kind = AST_LITERAL,
							.literal.type = parse_type(p),
						}),
					}));
				} else {
					da_append(&nodes, ast_new({
						.kind = AST_UN_EXP,
						.loc = parser_peek(p)->loc,
						.expr_unary.op = tok_to_unary_expr_op(parser_peek(p)),
						.expr_unary.type = TUPTR,
						.expr_unary.v = NULL,
					}));
				}
				break;

			case TOK_EXC: case TOK_AMP:
				da_append(&nodes, ast_new({
					.kind = AST_UN_EXP,
					.loc = parser_peek(p)->loc,
					.expr_unary.op = tok_to_unary_expr_op(parser_peek(p)),
					.expr_unary.v = NULL,
				}));
				break;

			case TOK_OSQBRA:
				da_append(&nodes, ast_new({
					.kind = AST_BIN_EXP,
					.loc = parser_peek(p)->loc,
					.expr_binary.op = tok_to_binary_expr_op(parser_peek(p)->kind),
					.expr_binary.l = NULL,
					.expr_binary.r = NULL
				}));
				parser_next(p);
				da_append(&nodes, parse_expr(p, EXPR_PARSING_SQBRA, &TUPTR));
				p->cur_token--;
				break;

			case TOK_PLUS_EQ: case TOK_MINUS_EQ:
			case TOK_STAR_EQ: case TOK_SLASH_EQ:
			case TOK_NOT_EQ:  case TOK_OR:
			case TOK_LESS:    case TOK_GREAT:
			case TOK_LESS_EQ: case TOK_GREAT_EQ:
			case TOK_EQ_EQ:   case TOK_AND:
			case TOK_PLUS:    case TOK_SLASH:
			case TOK_EQ:      case TOK_PS:
			case TOK_DOT: {
				da_append(&nodes, ast_new({
					.kind = AST_BIN_EXP,
					.loc = parser_peek(p)->loc,
					.expr_binary.op = tok_to_binary_expr_op(parser_peek(p)->kind),
					.expr_binary.l = NULL,
					.expr_binary.r = NULL
				}));
			} break;

			case TOK_STAR: case TOK_MINUS: {
				bool is_unary_op = false;
				if (nodes.count == 0) {
					is_unary_op = true;
				} else {
					if (da_last(&nodes)->kind == AST_UN_EXP) {
						is_unary_op = false;
					} else {
						bool is_bin_op = da_last(&nodes)->kind == AST_BIN_EXP;
						if (is_bin_op && da_last(&nodes)->expr_binary.l &&
							da_last(&nodes)->expr_binary.r) is_bin_op = false;
						if (is_bin_op) is_unary_op = true;
					}
				}

				if (!is_unary_op) {
					da_append(&nodes, ast_new({
						.kind = AST_BIN_EXP,
						.loc = parser_peek(p)->loc,
						.expr_binary.op = tok_to_binary_expr_op(parser_peek(p)->kind),
						.expr_binary.l = NULL,
						.expr_binary.r = NULL
					}));
				} else {
					da_append(&nodes, ast_new({
						.kind = AST_UN_EXP,
						.loc = parser_peek(p)->loc,
						.expr_unary.op = tok_to_unary_expr_op(parser_peek(p)),
						.expr_unary.v = NULL,
					}));
				}
			} break;

			default:
				lexer_error(parser_peek(p)->loc, "error: unexpected token");
		}

		parser_next(p);
	}

	AST_Node *expr = expr_expand(&nodes);
	expr_calc_types(p, expr, vart);
	da_free(&nodes);
	return expr;
}
