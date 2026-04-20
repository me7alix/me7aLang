#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "../include/parser.h"

Type TUPTR = {.kind = TYPE_UPTR};

static Token peek(Parser *p)  { return *p->cur_token;       }
static Token peek2(Parser *p) { return *(p->cur_token + 1); }
static Token next(Parser *p)  { return *p->cur_token++;     }
Symbol *smbt_get(Parser *p, SymbolKind st, char *id);

double parse_float(char *data) {
	return atof(data);
}

long long parse_int(char *data) {
	char *end;
	return strtoll(data, &end, 0);
}

float op_prec(AST_ExprOp op, bool l) {
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
	case AST_OP_BW_NOT:
		if (l) return 2.8;
		else   return 0;
	case AST_OP_BW_AND:
	case AST_OP_BW_OR:
	case AST_OP_BW_LS:
	case AST_OP_BW_RS:
		if (l) return 2.6;
		else   return 2.5;
	case AST_OP_EQ:
	case AST_OP_GREAT:
	case AST_OP_LESS:
	case AST_OP_GREAT_EQ:
	case AST_OP_LESS_EQ:
	case AST_OP_NOT_EQ:
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
	case AST_OP_NOT:
	case AST_OP_NEG:
	case AST_OP_REF:
	case AST_OP_DEREF:
		if (l) return 3.0;
		else   return 0.0;
	case AST_OP_VAR_EQ:
	case AST_OP_ADD_EQ:
	case AST_OP_SUB_EQ:
	case AST_OP_DIV_EQ:
	case AST_OP_MUL_EQ:
		if (l) return -1.0;
		else   return -1.1;
	default:   break;
	}

	return 0.0;
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

// Generates AST using precedence climbing algorithm
AST_Node *expr_expand(AST_Nodes *nodes) {
	if (nodes->count == 0) return NULL;
	Location loc = da_get(nodes, 0)->loc;

	size_t countBefore = nodes->count;
	if (nodes->count == 1) {
		bool error = false;
		if (da_get(nodes, 0)->kind == AST_BIN_EXP) {
			if (!da_get(nodes, 0)->as.ebin.l ||
				!da_get(nodes, 0)->as.ebin.r) error = true;
		} else if (da_get(nodes, 0)->kind == AST_UN_EXP) {
			if (!da_get(nodes, 0)->as.eun.v) error = true;
		}

		if (error) throw_error(da_get(nodes, 0)->loc, "wrong expression");
		return da_get(nodes, 0);
	}

	for (size_t i = 0; i < nodes->count; i++) {
		if (nodes->count == 1) break;
		AST_Node *node = da_get(nodes, i);

		bool isBinOp =
			node->kind == AST_BIN_EXP &&
			!(node->as.ebin.l && node->as.ebin.r);

		bool isUnOp =
			node->kind == AST_UN_EXP &&
			!node->as.eun.v;

		if (!isBinOp && !isUnOp) {
			bool isLeft = false;
			float lp = -999, rp = -999;

			if (i != 0) {
				switch (da_get(nodes, i-1)->kind) {
				case AST_BIN_EXP: lp = op_prec(da_get(nodes, i-1)->as.ebin.op, true); break;
				case AST_UN_EXP:  lp = op_prec(da_get(nodes, i-1)->as.eun.op,  true); break;
				default: throw_error(da_get(nodes, i-1)->loc, "wrong expression"); }
			}

			if (i != nodes->count-1) {
				switch (da_get(nodes, i+1)->kind) {
				case AST_BIN_EXP: rp = op_prec(da_get(nodes, i+1)->as.ebin.op, false); break;
				case AST_UN_EXP:  rp = op_prec(da_get(nodes, i+1)->as.eun.op,  false); break;
				default: throw_error(da_get(nodes, i+1)->loc, "wrong expression");  }
			}

			if (lp > rp) isLeft = true;
			if (lp < -500 && rp < -500)
				throw_error(da_get(nodes, i)->loc, "wrong expression");

			if (isLeft) {
				switch (da_get(nodes, i-1)->kind) {
				case AST_BIN_EXP: da_get(nodes, i-1)->as.ebin.r = node; break;
				case AST_UN_EXP:  da_get(nodes, i-1)->as.eun.v = node;  break;
				default: UNREACHABLE; }
			} else {
				switch (da_get(nodes, i+1)->kind) {
				case AST_BIN_EXP: da_get(nodes, i+1)->as.ebin.l = node; break;
				case AST_UN_EXP:  da_get(nodes, i+1)->as.eun.v = node;  break;
				default: UNREACHABLE; }
			}

			da_remove_ordered(nodes, i);
			i--;
		}
	}

	if (nodes->count == countBefore)
		throw_error(loc, "wrong expression");
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

// Calculates and checks types, sometimes changes AST
Type expr_analysis(Parser *p, AST_Node *expr, Type *vart) {
	switch (expr->kind) {
	case AST_ARRAY:
		bool err = false;
		if      (!vart)              err = true;
		else if (!is_pointer(*vart)) err = true;
		if (err) throw_error(expr->loc, "types mismatching");

		Type baseType = *get_pointer_base(*vart);
		da_foreach (AST_Node*, n, &expr->as.array) {
			Type nt = expr_analysis(p, *n, &baseType);
			if (!compare_types(baseType, nt)) {
				throw_error((*n)->loc, "types mismatching");
			}
		}
		return *vart;

	case AST_FUNC_CALL:
		return expr->as.func_call.type;

	case AST_VID: {
		Symbol *var = smbt_get(p, SBL_VAR, expr->as.vid.id);
		if (!var) throw_error(expr->loc, "no such variable in the scope");
		return var->variable.type;
	} break;

	case AST_LITERAL: {
		if (vart && expr->as.literal.kind == LIT_INT) {
			expr->as.literal.type = *vart;
		} else {
			switch (expr->as.literal.kind) {
			case LIT_CHAR:  expr->as.literal.type = (Type) {.kind = TYPE_U8};   break;
			case LIT_FLOAT: expr->as.literal.type = (Type) {.kind = TYPE_F32};  break;
			case LIT_BOOL:  expr->as.literal.type = (Type) {.kind = TYPE_BOOL}; break;
			case LIT_INT: {
				if(expr->as.literal.type.kind == TYPE_NULL) {
					expr->as.literal.type = (Type) {.kind = TYPE_INT};
				}
			} break;
			case LIT_STR: {
				static Type TU8 = {.kind = TYPE_U8};
				expr->as.literal.type = (Type){
					.kind = TYPE_POINTER,
					.as.pointer.base = &TU8
				};
			} break;
			}
		}

		return expr->as.literal.type;
	} break;

	case AST_BIN_EXP: {
		Type lt = expr_analysis(p, expr->as.ebin.l, vart);
		switch (expr->as.ebin.op) {
		case AST_OP_VAR_EQ:
		case AST_OP_ADD_EQ:
		case AST_OP_SUB_EQ:
		case AST_OP_MUL_EQ:
		case AST_OP_DIV_EQ:
			vart = &lt;
			break;
		default:
			if(is_pointer(lt)) {
				vart = &TUPTR;
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
						if (get_pointer_base(lt)->kind == TYPE_STRUCT)
							goto no_err;
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
								throw_error(metCall->loc, "arguments count mismatching");
							}

							for (size_t i = 1; i < func->as.func_def.args.count; i++) {
								Type req_type = func->as.func_def.args.items[i]->as.func_def_arg.type;
								AST_Node *arg = metCall->as.method_call.args.items[i];
								expr_analysis(p, arg, &req_type);

								if (!compare_types(req_type, parser_get_type(p, arg))) {
									throw_error(
										metCall->as.method_call.args.items[i]->loc,
										"types mismatching");
								}
							}

							/* Passing the struct as.pointer to the method */

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

				if (lt.kind != TYPE_STRUCT)
					throw_error(expr->loc, "struct expected");

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

		Type rt = expr_analysis(p, expr->as.ebin.r, vart);
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
			} else throw_error(expr->loc, "operation on different types");
		}

		switch (expr->as.ebin.op) {
		case AST_OP_EQ: case AST_OP_NOT_EQ:
		case AST_OP_LESS_EQ: case AST_OP_GREAT_EQ:
		case AST_OP_GREAT: case AST_OP_LESS:
			expr->as.ebin.type.kind = TYPE_BOOL;
		default:;
		}

		if (expr->as.ebin.op == AST_OP_ARR)
			expr->as.ebin.type = *expr->as.ebin.type.as.pointer.base;

		return expr->as.ebin.type;
	} break;

	case AST_UN_EXP: {
		switch (expr->as.eun.op) {
		case AST_OP_SIZEOF: {
			expr_analysis(p, expr->as.eun.v, NULL);
		} break;

		case AST_OP_CAST: {
			expr_analysis(p, expr->as.eun.v, &expr->as.eun.type);
		} break;

		case AST_OP_REF: {
			Type vt = expr_analysis(p, expr->as.eun.v, vart);
			Type *base = malloc(sizeof(Type)); *base = vt;
			expr->as.eun.type = (Type) {.kind = TYPE_POINTER, .as.pointer.base = base};
		} break;

		case AST_OP_DEREF: {
			Type vt = expr_analysis(p, expr->as.eun.v, vart);
			expr->as.eun.type = vt;
			if (!is_pointer(vt))
				throw_error(expr->as.eun.v->loc, "as.pointer expected");
			expr->as.eun.type = *vt.as.pointer.base;
		} break;

		default:
			expr->as.eun.type = expr_analysis(p, expr->as.eun.v, vart);
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
	};
}

AST_Node *parse_array(Parser *p) {
	AST_Node *al = new(AST_Node,
		.kind = AST_ARRAY,
		.loc = next(p).loc,
	);

	while (peek(p).kind != TOK_CBRA) {
		AST_Node *expr = parse_expr(p, EXPAR_ARRAY, NULL);
		da_append(&al->as.array, expr);
		next(p);

		if (peek(p).kind == TOK_COM) next(p);
		else if (peek(p).kind != TOK_CBRA)
			throw_error(al->loc, "unexpected token");
	}

	return al;
}

AST_Node *parse_expr(Parser *p, ExprParsingType type, Type *vart) {
	AST_Nodes nodes = {0};

	while (true) {
		if (peek(p).kind == TOK_OPAR) {
			next(p);
			da_append(&nodes, parse_expr(p, EXPAR_PAR, vart));
		}

		switch (type) {
		case EXPAR_FUNCALL:
			if (peek(p).kind == TOK_COM) goto done;
			else if (peek(p).kind == TOK_CPAR) {
				p->cur_token--;
				goto done;
			} break;
		case EXPAR_VAR:
			if (peek(p).kind == TOK_SEMI)
				goto done;
			break;
		case EXPAR_SQBRA:
			if (peek(p).kind == TOK_CSQBRA) {
				next(p);
				goto done;
			} break;
		case EXPAR_PAR:
			if (peek(p).kind == TOK_CPAR) {
				next(p);
				goto done;
			} break;
		case EXPAR_STMT:
			if (
				peek(p).kind == TOK_OBRA  ||
				peek(p).kind == TOK_ARROW ||
				peek(p).kind == TOK_ARROW_EQ
			) {
				p->cur_token--;
				goto done;
			} break;
		case EXPAR_ARRAY:
			if (
				peek(p).kind == TOK_COM ||
				peek(p).kind == TOK_CBRA
			) {
				p->cur_token--;
				goto done;
			} break;
		}

		switch (peek(p).kind) {
		case TOK_ID:
			if (peek2(p).kind == TOK_OPAR) {
				if (nodes.count > 0) {
					if (
						da_last(&nodes)->kind    == AST_BIN_EXP  &&
						da_last(&nodes)->as.ebin.op == AST_OP_FIELD
					) {
						da_append(&nodes, parse_method_call(p));
						p->cur_token--;
						break;
					}
				}

				da_append(&nodes, parse_func_call(p));
				p->cur_token--;
			} else {
				Symbol *var = smbt_get(p, SBL_VAR, peek(p).data);
				da_append(&nodes, new(AST_Node,
					.kind = AST_VID,
					.loc = peek(p).loc,
					.as.vid.id = peek(p).data,
					.as.vid.uid = var ? var->variable.uid : 0,
				));
				
			}
			break;

		case TOK_INT:
			da_append(&nodes, new(AST_Node,
				.kind = AST_LITERAL,
				.as.literal.kind = LIT_INT,
				.loc = peek(p).loc,
				.as.literal.as.lint = parse_int(peek(p).data),
			));
			break;

		case TOK_TRUE:
			da_append(&nodes, new(AST_Node,
				.kind = AST_LITERAL,
				.loc = peek(p).loc,
				.as.literal.kind = LIT_BOOL,
				.as.literal.as.lint = 1,
			));
			break;

		case TOK_OBRA:
			da_append(&nodes, parse_array(p));
			break;

		case TOK_FALSE:
			da_append(&nodes, new(AST_Node,
				.kind = AST_LITERAL,
				.as.literal.kind = LIT_BOOL,
				.as.literal.as.lint = 0,
			));
			break;

		case TOK_NULL: {
			Type *u0 = malloc(sizeof(Type));
			*u0 = (Type) {.kind = TYPE_NULL};
			da_append(&nodes, new(AST_Node,
				.kind = AST_LITERAL,
				.as.literal.kind = LIT_INT,
				.as.literal.as.lint = 0,
				.as.literal.type = (Type) {
					.kind = TYPE_POINTER,
					.as.pointer.base = u0
				},
			));
		} break;

		case TOK_STRING:
			da_append(&nodes, new(AST_Node,
				.kind = AST_LITERAL,
				.loc = peek(p).loc,
				.as.literal.kind = LIT_STR,
				.as.literal.as.str = peek(p).data,
			));
			break;

		case TOK_CHAR:
			da_append(&nodes, new(AST_Node,
				.kind = AST_LITERAL,
				.loc = peek(p).loc,
				.as.literal.kind = LIT_CHAR,
				.as.literal.as.lint = peek(p).data[0],
			));
			break;

		case TOK_FLOAT:
			da_append(&nodes, new(AST_Node,
				.kind = AST_LITERAL,
				.as.literal.kind = LIT_FLOAT,
				.as.literal.as.lfloat = parse_float(peek(p).data),
			));
			break;

		case TOK_COL: {
			Type t = *parse_type(p);
			da_append(&nodes, new(AST_Node,
				.kind = AST_UN_EXP,
				.loc = peek(p).loc,
				.as.eun.type = t,
				.as.eun.op = AST_OP_CAST,
				.as.eun.v = NULL,
			));
		} break;

		case TOK_SIZEOF:
			if (peek2(p).kind == TOK_COL) {
				da_append(&nodes, new(AST_Node,
					.kind = AST_UN_EXP,
					.loc = peek(p).loc,
					.as.eun.op = get_un_op(next(p)),
					.as.eun.type = TUPTR,
					.as.eun.v = new(AST_Node,
						.kind = AST_LITERAL,
						.as.literal.type = *parse_type(p),
					),
				));
			} else {
				da_append(&nodes, new(AST_Node,
					.kind = AST_UN_EXP,
					.loc = peek(p).loc,
					.as.eun.op = get_un_op(peek(p)),
					.as.eun.type = TUPTR,
					.as.eun.v = NULL,
				));
			}
			break;

		case TOK_EXC: case TOK_TILDA:
			da_append(&nodes, new(AST_Node,
				.kind = AST_UN_EXP,
				.loc = peek(p).loc,
				.as.eun.op = get_un_op(peek(p)),
				.as.eun.v = NULL,
			));
			break;

		case TOK_OSQBRA:
			da_append(&nodes, new(AST_Node,
				.kind = AST_BIN_EXP,
				.loc = peek(p).loc,
				.as.ebin.op = get_bin_op(peek(p)),
				.as.ebin.l = NULL,
				.as.ebin.r = NULL
			));
			next(p);
			da_append(&nodes, parse_expr(p, EXPAR_SQBRA, &TUPTR));
			p->cur_token--;
			break;

		case TOK_LEFT_SHIFT:
		case TOK_RIGHT_SHIFT:
		case TOK_PLUS_EQ: case TOK_MINUS_EQ:
		case TOK_STAR_EQ: case TOK_SLASH_EQ:
		case TOK_NOT_EQ:  case TOK_OR:
		case TOK_LESS:    case TOK_GREAT:
		case TOK_LESS_EQ: case TOK_GREAT_EQ:
		case TOK_EQ_EQ:   case TOK_AND:
		case TOK_PLUS:    case TOK_SLASH:
		case TOK_EQ:      case TOK_PS:
		case TOK_DOT:     case TOK_PIPE:
		case TOK_XOR: {
			da_append(&nodes, new(AST_Node,
				.kind = AST_BIN_EXP,
				.loc = peek(p).loc,
				.as.ebin.op = get_bin_op(peek(p)),
				.as.ebin.l = NULL,
				.as.ebin.r = NULL
			));
		} break;

		case TOK_AMP:
		case TOK_STAR: case TOK_MINUS: {
			bool is_unary_op = false;
			if (nodes.count == 0) {
				is_unary_op = true;
			} else {
				if (da_last(&nodes)->kind == AST_UN_EXP) {
					is_unary_op = false;
				} else {
					bool isBinOp = da_last(&nodes)->kind == AST_BIN_EXP;
					if (isBinOp && da_last(&nodes)->as.ebin.l &&
						da_last(&nodes)->as.ebin.r) isBinOp = false;
					if (isBinOp) is_unary_op = true;
				}
			}

			if (!is_unary_op) {
				da_append(&nodes, new(AST_Node,
					.kind = AST_BIN_EXP,
					.loc = peek(p).loc,
					.as.ebin.op = get_bin_op(peek(p)),
					.as.ebin.l = NULL,
					.as.ebin.r = NULL
				));
			} else {
				da_append(&nodes, new(AST_Node,
					.kind = AST_UN_EXP,
					.loc = peek(p).loc,
					.as.eun.op = get_un_op(peek(p)),
					.as.eun.v = NULL,
				));
			}
		} break;

		default:
			throw_error(peek(p).loc, "unexpected token");
		}

		next(p);
	}

done:;
	AST_Node *expr = expr_expand(&nodes);
	if (expr) expr_analysis(p, expr, vart);
	da_free(&nodes);
	return expr;
}
