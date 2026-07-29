#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <cplus.h>
#include <parser.h>

#define peek(p) (*(p)->tokens)
#define peek2(p) (*((p)->tokens+1))
#define next(p) (*((p)->tokens++))

int op_prec(AST_ExprOp op, bool l) {
	switch (op) {
	case AST_OP_ADD:
	case AST_OP_SUB:
		return 10;
	case AST_OP_MOD:
	case AST_OP_MUL:
	case AST_OP_DIV:
		return 20;
	case AST_OP_BW_NOT:
		return l ? 0 : 30;
	case AST_OP_BW_XOR:
	case AST_OP_BW_AND:
	case AST_OP_BW_OR:
	case AST_OP_BW_LS:
	case AST_OP_BW_RS:
		return 25;
	case AST_OP_EQ:
	case AST_OP_GREAT:
	case AST_OP_LESS:
	case AST_OP_GREAT_EQ:
	case AST_OP_LESS_EQ:
	case AST_OP_NOT_EQ:
		return 8;
	case AST_OP_OR:
		return 6;
	case AST_OP_AND:
		return 7;
	case AST_OP_CAST:
		return l ? 50 : 0;
	case AST_OP_ARR:
		return 30;
	case AST_OP_FIELD:
		return 40;
	case AST_OP_SIZEOF:
		return l ? 0 : 60;
	case AST_OP_NOT:
	case AST_OP_NEG:
	case AST_OP_REF:
	case AST_OP_DEREF:
		return l ? 0 : 30;
	case AST_OP_VAR_EQ:
	case AST_OP_ADD_EQ:
	case AST_OP_SUB_EQ:
	case AST_OP_DIV_EQ:
	case AST_OP_MUL_EQ:
		return 1;
	default:
		return 0;
	}
}

Type get_func_type(SymbolKind kind, Symbol *func) {
	Type *type = new(Type, .kind = TYPE_FUNCTION);
	AST_Nodes *args;
	switch (kind) {
	case SBL_FUNC_DEF:
		type->as.func.ret = &func->func_def.type;
		args = &func->func_def.args;
		break;
	case SBL_FUNC_EXTERN:
		type->as.func.ret = &func->func_extern.type;
		args = &func->func_extern.args;
	}
	da_foreach (AST_Node*, arg, args) {
		da_append(&type->as.func.args, (*arg)->as.func_def_arg.type);
	}
	return *type;
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

Symbol *sbltbl_get(Parser *p, SymbolKind st, char *id);

// Calculates and checks types, sometimes changes AST
Type expr_analysis(Parser *p, AST_Node *expr, Type *src_type) {
	switch (expr->kind) {
	case AST_ARRAY:
		bool err = false;
		if      (!src_type)              err = true;
		else if (!is_pointer(*src_type)) err = true;
		if (err) throw_error(expr->loc, "types mismatch");

		Type baseType = *get_pointer_base(*src_type);
		da_foreach (AST_Node*, n, &expr->as.array) {
			Type nt = expr_analysis(p, *n, &baseType);
			if (!compare_types(baseType, nt)) {
				throw_types_mismatch((*n)->loc, baseType, nt);
			}
		}
		return *src_type;

	case AST_FUNC_CALL:
		return expr->as.func_call.type;

	case AST_VID: {
		Symbol *var = sbltbl_get(p, SBL_VAR, expr->as.vid.id);
		if (!var) throw_error(expr->loc, "no such variable in the scope");
		return var->variable.type;
	} break;

	case AST_LITERAL: {
		if (src_type && expr->as.literal.kind == LIT_INT) {
			expr->as.literal.type = *src_type;
		} else {
			switch (expr->as.literal.kind) {
			case LIT_CHAR:  expr->as.literal.type = (Type) {.kind = TYPE_U8};   break;
			case LIT_FLOAT: expr->as.literal.type = (Type) {.kind = TYPE_F32};  break;
			case LIT_BOOL:  expr->as.literal.type = (Type) {.kind = TYPE_BOOL}; break;
			case LIT_INT:
				if(expr->as.literal.type.kind == TYPE_NULL) {
					expr->as.literal.type = (Type){.kind = TYPE_INT};
				} break;
			case LIT_STR:;
				expr->as.literal.type = (Type){
					.kind = TYPE_POINTER,
					.as.pointer.base = &TU8
				};
			}
		}

		return expr->as.literal.type;
	} break;

	case AST_BIN_EXP: {
		Type lt = expr_analysis(p, expr->as.ebin.l, src_type);
		switch (expr->as.ebin.op) {
		case AST_OP_VAR_EQ:
		case AST_OP_ADD_EQ:
		case AST_OP_SUB_EQ:
		case AST_OP_MUL_EQ:
		case AST_OP_DIV_EQ:
			src_type = &lt;
			break;
		default:
			if(is_pointer(lt)) {
				src_type = &TUPTR;
			}
		}

		if (expr->as.ebin.op == AST_OP_FIELD) {
			if (expr->as.ebin.r->kind == AST_METHOD_CALL) {
				/* Auto-referencing */
				if (lt.kind == TYPE_STRUCT) {
					Type *nt = malloc(sizeof(*nt));
					*nt = lt;
					Type ct = {
						.kind = TYPE_POINTER,
						.as.pointer.base = nt,
					};
					expr->as.ebin.l = new(AST_Node,
						.kind = AST_UN_EXP,
						.as.eun.op = AST_OP_REF,
						.as.eun.v = expr->as.ebin.l,
						.as.eun.type = ct,
					);
					lt = ct;
				} else {
					if (is_pointer(lt)) {
						if (get_pointer_base(lt)->kind == TYPE_STRUCT) {
							goto no_err;
						}
					}
					throw_error(expr->as.ebin.l->loc, "struct expected");
					no_err:;
				}

				da_foreach (Member, member, &lt.as.pointer.base->as.user->as.ustruct.members) {
					if (member->kind == MBR_METHOD) {
						if (strcmp(expr->as.ebin.r->as.method_call.id,
								member->as.method.func->as.func_def.id) == 0) {
							AST_Node *func    = member->as.method.func;
							AST_Node *metCall = expr->as.ebin.r;
							/* Method call types checking */
							if (func->as.func_def.args.count != metCall->as.method_call.args.count) {
								throw_error(metCall->loc, "arguments count mismatch");
							}
							for (size_t i = 1; i < func->as.func_def.args.count; i++) {
								Type req_type = func->as.func_def.args.items[i]->as.func_def_arg.type;
								AST_Node *arg = metCall->as.method_call.args.items[i];
								expr_analysis(p, arg, &req_type);
								if (!compare_types(req_type, parser_get_type(p, arg))) {
									throw_types_mismatch(
										metCall->as.method_call.args.items[i]->loc,
										req_type, parser_get_type(p, arg)
									);
								}
							}
							/* Passing the struct as pointer to the method */
							expr->as.ebin.type = func->as.func_def.type;
							metCall->as.method_call.struct_name = lt.as.pointer.base->as.user->id;
							metCall->as.method_call.args.items[0] = expr->as.ebin.l;
							metCall->as.method_call.type = expr->as.ebin.type;
							return expr->as.ebin.type;
						}
					}
				}
				throw_error(expr->as.ebin.l->loc, "no such method");
			} else {
				/* Auto-dereferencing */
				if (lt.kind == TYPE_POINTER) {
					expr->as.ebin.l = new(AST_Node,
						.kind = AST_UN_EXP,
						.as.eun.op = AST_OP_DEREF,
						.as.eun.v = expr->as.ebin.l,
						.as.eun.type = *lt.as.pointer.base,
					);
					lt = *lt.as.pointer.base;
				}
				if (lt.kind != TYPE_STRUCT) {
					throw_error(expr->loc, "struct expected");
				}
				da_foreach (Member, member, &lt.as.user->as.ustruct.members) {
					if (member->kind == MBR_FIELD) {
						if (strcmp(expr->as.ebin.r->as.vid.id,
								member->as.field.id) == 0) {
							expr->as.ebin.type = member->as.field.type;
							return member->as.field.type;
						}
					}
				}
				throw_error(expr->loc, "no such field");
			}
		}

		Type rt = expr_analysis(p, expr->as.ebin.r, src_type);
		expr->as.ebin.type = lt;

		if (is_pointer(lt) && is_pointer(rt) && expr->as.ebin.op == AST_OP_SUB) {
			expr->as.ebin.type = (Type){.kind = TYPE_IPTR};
		} else if ((lt.kind == TYPE_IPTR && is_pointer(rt)) ||
			(is_pointer(lt) && rt.kind == TYPE_IPTR) ||
			(lt.kind == TYPE_UPTR && is_pointer(rt)) ||
			(is_pointer(lt) && rt.kind == TYPE_UPTR)) {
			Type ptr_type = is_pointer(lt) ? lt : rt;
			expr->as.ebin.type = (Type) {
				.kind = TYPE_POINTER,
				.as.pointer.base = get_pointer_base(ptr_type)
			};
		} else if (!compare_types(lt, rt)) {
			AST_Node *le = expr->as.ebin.l, *re = expr->as.ebin.r;
			if ((le->kind == AST_LITERAL || re->kind == AST_LITERAL) &&
				(type_is_int(lt) && type_is_int(rt))) {
				AST_Node *lit     = le->kind == AST_LITERAL ? le : re;
				AST_Node *not_lit = le->kind != AST_LITERAL ? le : re;
				lit->as.literal.type = parser_get_type(p, not_lit);
			} else throw_types_mismatch(expr->loc, lt, rt);
		}

		switch (expr->as.ebin.op) {
		case AST_OP_EQ: case AST_OP_NOT_EQ:
		case AST_OP_LESS_EQ: case AST_OP_GREAT_EQ:
		case AST_OP_GREAT: case AST_OP_LESS:
			expr->as.ebin.type.kind = TYPE_BOOL;
		}
		if (expr->as.ebin.op == AST_OP_ARR)
			expr->as.ebin.type = *expr->as.ebin.type.as.pointer.base;
		return expr->as.ebin.type;
	} break;

	case AST_UN_EXP: {
		switch (expr->as.eun.op) {
		case AST_OP_SIZEOF:
			expr_analysis(p, expr->as.eun.v, NULL);
			break;

		case AST_OP_CAST:
			expr_analysis(p, expr->as.eun.v, &expr->as.eun.type);
			break;

		case AST_OP_REF: {
			Type vt = expr_analysis(p, expr->as.eun.v, src_type);
			Type *base = malloc(sizeof(Type)); *base = vt;
			expr->as.eun.type = (Type){.kind = TYPE_POINTER, .as.pointer.base = base};
		} break;

		case AST_OP_DEREF: {
			Type vt = expr_analysis(p, expr->as.eun.v, src_type);
			expr->as.eun.type = vt;
			if (!is_pointer(vt))
				throw_error(expr->as.eun.v->loc, "as.pointer expected");
			expr->as.eun.type = *vt.as.pointer.base;
		} break;

		default:
			expr->as.eun.type = expr_analysis(p, expr->as.eun.v, src_type);
		}

		return expr->as.eun.type;
	} break;

	default:
		UNREACHABLE;
	}
}

AST_ExprOp get_bin_op(Token tok) {
	switch (tok.kind) {
	case TOK_PLUS_EQ:     return AST_OP_ADD_EQ;
	case TOK_MINUS_EQ:    return AST_OP_SUB_EQ;
	case TOK_STAR_EQ:     return AST_OP_MUL_EQ;
	case TOK_SLASH_EQ:    return AST_OP_DIV_EQ;
	case TOK_NOT_EQ:      return AST_OP_NOT_EQ;
	case TOK_EQ_EQ:       return AST_OP_EQ;
	case TOK_EQ:          return AST_OP_VAR_EQ;
	case TOK_GREAT:       return AST_OP_GREAT;
	case TOK_LESS:        return AST_OP_LESS;
	case TOK_GREAT_EQ:    return AST_OP_GREAT_EQ;
	case TOK_LESS_EQ:     return AST_OP_LESS_EQ;
	case TOK_AND:         return AST_OP_AND;
	case TOK_OR:          return AST_OP_OR;
	case TOK_PLUS:        return AST_OP_ADD;
	case TOK_MINUS:       return AST_OP_SUB;
	case TOK_STAR:        return AST_OP_MUL;
	case TOK_SLASH:       return AST_OP_DIV;
	case TOK_PS:          return AST_OP_MOD;
	case TOK_OSQBRA:      return AST_OP_ARR;
	case TOK_DOT:         return AST_OP_FIELD;
	case TOK_AMP:         return AST_OP_BW_AND;
	case TOK_PIPE:        return AST_OP_BW_OR;
	case TOK_XOR:         return AST_OP_BW_XOR;
	case TOK_LEFT_SHIFT:  return AST_OP_BW_LS;
	case TOK_RIGHT_SHIFT: return AST_OP_BW_RS;
	default:
		throw_error(tok.loc, "wrong operation");
		return 0;
	}
}

AST_ExprOp get_un_op(Token tok) {
	switch (tok.kind) {
	case TOK_SIZEOF: return AST_OP_SIZEOF;
	case TOK_COL:    return AST_OP_CAST;
	case TOK_STAR:   return AST_OP_DEREF;
	case TOK_AMP:    return AST_OP_REF;
	case TOK_EXC:    return AST_OP_NOT;
	case TOK_MINUS:  return AST_OP_NEG;
	case TOK_TILDA:  return AST_OP_BW_NOT;
	default:
		throw_error(tok.loc, "wrong operation");
		return 0;
	}
}

bool check_expr_ended(Parser *p, TokenKind *until) {
	while (*until) {
		if (peek(p).kind == *until)
			return true;
		until++;
	}
	return false;
}

AST_Node *parse_expr_item(Parser *p, TokenKind *until);

// Pratt parser for expressions
AST_Node *parse_expr_bp(Parser *p, float min_bp, TokenKind *until) {
	if (check_expr_ended(p, until)) return NULL;
	AST_Node *lhs = parse_expr_item(p, until);
	while (true) {
		if (lhs->kind == AST_OPERATOR) {
			Type *op_type = lhs->as.operator.type;
			AST_ExprOp op = get_un_op(lhs->as.operator.tok);
			int rbp = op_prec(op, false);
			lhs = new(AST_Node,
				.kind = AST_UN_EXP,
				.loc = lhs->loc,
				.as.eun.op = op);
			if (op == AST_OP_SIZEOF) {
				lhs->as.eun.type = TUPTR;
				if (op_type) {
					lhs->as.eun.v = new(AST_Node,
						.kind = AST_LITERAL,
						.loc = lhs->loc,
						.as.literal.type = *op_type);
					continue;
				}
			}
			lhs->as.eun.v = parse_expr_bp(p, rbp, until);
		} else {
			if (check_expr_ended(p, until)) break;
			Token *saved = p->tokens;
			AST_Node *op_expr = parse_expr_item(p, until);
			if (op_expr->kind != AST_OPERATOR)
				throw_error(op_expr->loc, "operator expected");
			if (op_expr->as.operator.tok.kind == TOK_COL) {
				AST_ExprOp op = get_un_op(op_expr->as.operator.tok);
				int lbp = op_prec(op, true);
				int rbp = op_prec(op, false);
				if (lbp <= min_bp) {
					p->tokens = saved;
					break;
				}
				lhs = new(AST_Node,
					.kind = AST_UN_EXP,
					.loc = op_expr->loc,
					.as.eun.type = *op_expr->as.operator.type,
					.as.eun.op = op,
					.as.eun.v = lhs
				);
			} else {
				AST_ExprOp op = get_bin_op(op_expr->as.operator.tok);
				int rbp = op_prec(op, true);
				int lbp = op_prec(op, false);
				if (lbp <= min_bp) {
					p->tokens = saved;
					break;
				}
				AST_Node *rhs;
				if (op == AST_OP_ARR) {
					rhs = parse_expr_bp(p, 0, until(TOK_CSQBRA));
					next(p);
				} else if (op == AST_OP_FIELD && peek(p).kind == TOK_ID && peek2(p).kind == TOK_OPAR) {
					rhs = parse_method_call(p);
				} else {
					rhs = parse_expr_bp(p, rbp, until);
				}
				lhs = new(AST_Node,
					.kind = AST_BIN_EXP,
					.loc = op_expr->loc,
					.as.ebin.op = op,
					.as.ebin.l = lhs,
					.as.ebin.r = rhs
				);
			}
		}
	}
	return lhs;
}

AST_Node *parse_array(Parser *p) {
	AST_Node *al = new(AST_Node,
		.kind = AST_ARRAY,
		.loc = next(p).loc);
	while (peek(p).kind != TOK_CBRA) {
		AST_Node *expr = parse_expr(p, until(TOK_COM, TOK_CBRA), NULL);
		da_append(&al->as.array, expr);
		if (peek(p).kind == TOK_COM) next(p);
	}
	next(p);
	return al;
}

double parse_float(char *data) {
	return atof(data);
}

long long parse_int(char *data) {
	char *end;
	return strtoll(data, &end, 0);
}

AST_Node *parse_expr_item(Parser *p, TokenKind *until) {
	switch (peek(p).kind) {
	case TOK_OPAR: {
		next(p);
		AST_Node *expr = parse_expr_bp(p, 0, until(TOK_CPAR));
		next(p);
		return expr;
	}

	case TOK_ID:
		if (peek2(p).kind == TOK_OPAR) {
			return parse_func_call(p);
		} else {
			Symbol *var = sbltbl_get(p, SBL_VAR, peek(p).data);
			AST_Node *expr = new(AST_Node,
				.kind = AST_VID,
				.loc = peek(p).loc,
				.as.vid.id = peek(p).data,
				.as.vid.uid = var ? var->variable.uid : 0);
			next(p);
			return expr;
		} break;

	case TOK_INT: {
		AST_Node *expr = new(AST_Node,
			.kind = AST_LITERAL,
			.loc = peek(p).loc,
			.as.literal.kind = LIT_INT);
		expr->as.literal.as.lint = parse_int(next(p).data);
		return expr;
	}

	case TOK_TRUE:
		return new(AST_Node,
			.kind = AST_LITERAL,
			.loc = next(p).loc,
			.as.literal.kind = LIT_BOOL,
			.as.literal.as.lbool = true
		);

	case TOK_OBRA:
		return parse_array(p);

	case TOK_FALSE:
		return new(AST_Node,
			.kind = AST_LITERAL,
			.loc = next(p).loc,
			.as.literal.kind = LIT_BOOL,
			.as.literal.as.lbool = false
		);

	case TOK_NULL:
		return new(AST_Node,
			.kind = AST_LITERAL,
			.loc = next(p).loc,
			.as.literal.kind = LIT_INT,
			.as.literal.as.lint = 0,
			.as.literal.type = (Type) {
				.kind = TYPE_POINTER,
				.as.pointer.base = &TU0
			}
		);

	case TOK_STRING: {
		AST_Node *expr = new(AST_Node,
			.kind = AST_LITERAL,
			.loc = peek(p).loc,
			.as.literal.kind = LIT_STR,
			.as.literal.as.str = peek(p).data);
		next(p);
		return expr;
	}

	case TOK_CHAR: {
		AST_Node *expr = new(AST_Node,
			.kind = AST_LITERAL,
			.loc = peek(p).loc,
			.as.literal.kind = LIT_CHAR,
			.as.literal.as.lint = peek(p).data[0]);
		next(p);
		return expr;
	}

	case TOK_FLOAT: {
		AST_Node *expr = new(AST_Node,
			.kind = AST_LITERAL,
			.loc = peek(p).loc,
			.as.literal.kind = LIT_FLOAT);
		expr->as.literal.as.lfloat = parse_float(next(p).data);
		return expr;
	}

	case TOK_COL: {
		Token ct = peek(p);
		Type *type = parse_type(p);
		AST_Node *expr = new(AST_Node,
			.kind = AST_OPERATOR,
			.loc = ct.loc,
			.as.operator.tok = ct,
			.as.operator.type = type);
		return expr;
	}

	case TOK_SIZEOF: {
		AST_Node *expr = new(AST_Node,
			.kind = AST_OPERATOR,
			.loc = peek(p).loc,
			.as.operator.tok = peek(p),
			.as.operator.type = NULL);
		next(p);
		if (peek(p).kind == TOK_COL)
			expr->as.operator.type = parse_type(p);
		return expr;
	}

	case TOK_EXC:
	case TOK_TILDA:{
		AST_Node *expr = new(AST_Node,
			.kind = AST_OPERATOR,
			.loc = peek(p).loc,
			.as.operator.tok = peek(p),
			.as.operator.type = NULL);
		next(p);
		return expr;
	}

	case TOK_OSQBRA: {
		AST_Node *expr = new(AST_Node,
			.kind = AST_OPERATOR,
			.loc = peek(p).loc,
			.as.operator.tok = peek(p),
			.as.operator.type = NULL);
		next(p);
		return expr;
	}

	case TOK_AMP:
	case TOK_STAR:
	case TOK_MINUS:
	case TOK_LEFT_SHIFT:
	case TOK_RIGHT_SHIFT:
	case TOK_PLUS_EQ:
	case TOK_MINUS_EQ:
	case TOK_STAR_EQ:
	case TOK_SLASH_EQ:
	case TOK_NOT_EQ:
	case TOK_OR:
	case TOK_LESS:
	case TOK_GREAT:
	case TOK_LESS_EQ:
	case TOK_GREAT_EQ:
	case TOK_EQ_EQ:
	case TOK_AND:
	case TOK_PLUS:
	case TOK_SLASH:
	case TOK_EQ:
	case TOK_PS:
	case TOK_DOT:
	case TOK_PIPE:
	case TOK_XOR: {
		AST_Node *expr = new(AST_Node,
			.kind = AST_OPERATOR,
			.loc = peek(p).loc,
			.as.operator.tok = peek(p),
			.as.operator.type = NULL);
		next(p);
		return expr;
	}

	default:
		throw_error(peek(p).loc, "unexpected token");
	}
}

AST_Node *parse_expr(Parser *p, TokenKind *until, Type *src_type) {
	AST_Node *expr = parse_expr_bp(p, 0, until);
	if (expr) expr_analysis(p, expr, src_type);
	return expr;
}
