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
Symbol *smbt_get(Parser *p, SymbolType st, char *id);

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

Type get_func_type(SymbolType kind, Symbol *func) {
	Type *type = type_new(.kind = TYPE_FUNCTION);
	AST_Nodes *args;
	switch (kind) {
	case SBL_FUNC_DEF:
		type->func.ret = &func->func_def.type;
		args = &func->func_def.args;
		break;
	case SBL_FUNC_EXTERN:
		type->func.ret = &func->func_extern.type;
		args = &func->func_extern.args;
	}

	da_foreach (AST_Node*, arg, args) {
		da_append(&type->func.args, (*arg)->func_def_arg.type);
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
			if (!da_get(nodes, 0)->ebin.l ||
				!da_get(nodes, 0)->ebin.r) error = true;
		} else if (da_get(nodes, 0)->kind == AST_UN_EXP) {
			if (!da_get(nodes, 0)->eun.v) error = true;
		}

		if (error) throw_error(da_get(nodes, 0)->loc, "wrong expression");
		return da_get(nodes, 0);
	}

	for (size_t i = 0; i < nodes->count; i++) {
		if (nodes->count == 1) break;
		AST_Node *node = da_get(nodes, i);

		bool isBinOp =
			node->kind == AST_BIN_EXP &&
			!(node->ebin.l && node->ebin.r);

		bool isUnOp =
			node->kind == AST_UN_EXP &&
			!node->eun.v;

		if (!isBinOp && !isUnOp) {
			bool isLeft = false;
			float lp = -999, rp = -999;

			if (i != 0) {
				switch (da_get(nodes, i-1)->kind) {
				case AST_BIN_EXP: lp = op_prec(da_get(nodes, i-1)->ebin.op, true); break;
				case AST_UN_EXP:  lp = op_prec(da_get(nodes, i-1)->eun.op,  true); break;
				default: throw_error(da_get(nodes, i-1)->loc, "wrong expression"); }
			}

			if (i != nodes->count-1) {
				switch (da_get(nodes, i+1)->kind) {
				case AST_BIN_EXP: rp = op_prec(da_get(nodes, i+1)->ebin.op, false); break;
				case AST_UN_EXP:  rp = op_prec(da_get(nodes, i+1)->eun.op,  false); break;
				default: throw_error(da_get(nodes, i+1)->loc, "wrong expression");  }
			}

			if (lp > rp) isLeft = true;
			if (lp < -500 && rp < -500)
				throw_error(da_get(nodes, i)->loc, "wrong expression");

			if (isLeft) {
				switch (da_get(nodes, i-1)->kind) {
				case AST_BIN_EXP: da_get(nodes, i-1)->ebin.r = node; break;
				case AST_UN_EXP:  da_get(nodes, i-1)->eun.v = node;  break;
				default: UNREACHABLE; }
			} else {
				switch (da_get(nodes, i+1)->kind) {
				case AST_BIN_EXP: da_get(nodes, i+1)->ebin.l = node; break;
				case AST_UN_EXP:  da_get(nodes, i+1)->eun.v = node;  break;
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
		da_foreach (AST_Node*, n, &expr->array) {
			Type nt = expr_analysis(p, *n, get_pointer_base(*vart));
			if (!compare_types(*get_pointer_base(*vart), nt)) {
				throw_error((*n)->loc, "types mismatching");
			}
		}
		return *vart;

	case AST_FUNC_CALL:
		return expr->func_call.type;

	case AST_VID: {
		Symbol *var = smbt_get(p, SBL_VAR, expr->vid.id);
		if (!var) throw_error(expr->loc, "no such variable in the scope");
		return var->variable.type;
	} break;

	case AST_LITERAL: {
		if (vart && expr->literal.kind == LIT_INT) {
			expr->literal.type = *vart;
		} else {
			switch (expr->literal.kind) {
			case LIT_CHAR:  expr->literal.type = (Type) {.kind = TYPE_I8};   break;
			case LIT_FLOAT: expr->literal.type = (Type) {.kind = TYPE_F32};  break;
			case LIT_BOOL:  expr->literal.type = (Type) {.kind = TYPE_BOOL}; break;
			case LIT_INT: {
				if(expr->literal.type.kind == TYPE_NULL) {
					expr->literal.type = (Type) {.kind = TYPE_INT};
				}
			} break;
			case LIT_STR: {
				static Type TU8 = {.kind = TYPE_U8};
				expr->literal.type = (Type){
					.kind = TYPE_POINTER,
					.pointer.base = &TU8
				};
			} break;
			}
		}

		return expr->literal.type;
	} break;

	case AST_BIN_EXP: {
		Type lt = expr_analysis(p, expr->ebin.l, vart);
		switch (expr->ebin.op) {
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

		if (expr->ebin.op == AST_OP_FIELD) {
			if (expr->ebin.r->kind == AST_METHOD_CALL) {
				/* Auto-referencing */

				if (lt.kind == TYPE_STRUCT) {
					Type *nt = malloc(sizeof(*nt));
					*nt = lt;

					Type ct = {
						.kind = TYPE_POINTER,
						.pointer.base = nt,
					};

					expr->ebin.l = ast_new(
						.kind = AST_UN_EXP,
						.eun.op = AST_OP_REF,
						.eun.v = expr->ebin.l,
						.eun.type = ct,
					);

					lt = ct;
				}

				da_foreach (StructMember, member, &lt.pointer.base->user->ustruct.members) {
					if (member->kind == STMEM_METHOD) {
						if (strcmp(expr->ebin.r->method_call.id,
								member->as.method.func->func_def.id) == 0) {
							AST_Node *func    = member->as.method.func;
							AST_Node *metCall = expr->ebin.r;

							/* Method call types checking */

							if (func->func_def.args.count != metCall->method_call.args.count) {
								throw_error(metCall->loc, "arguments count mismatching");
							}

							for (size_t i = 1; i < func->func_def.args.count; i++) {
								Type req_type = func->func_def.args.items[i]->func_def_arg.type;
								AST_Node *arg = metCall->method_call.args.items[i];
								expr_analysis(p, arg, &req_type);

								if (!compare_types(req_type, parser_get_type(p, arg))) {
									throw_error(
										metCall->method_call.args.items[i]->loc,
										"types mismatching");
								}
							}

							/* Passing the struct pointer to the method */

							expr->ebin.type = func->func_def.type;

							metCall->method_call.struct_name = lt.pointer.base->user->id;
							metCall->method_call.args.items[0] = expr->ebin.l;
							metCall->method_call.type = expr->ebin.type;

							return expr->ebin.type;
						}
					}
				}

				throw_error(expr->ebin.l->loc, "no such method");
			} else {
				/* Auto-dereferencing */

				if (lt.kind == TYPE_POINTER) {
					expr->ebin.l = ast_new(
						.kind = AST_UN_EXP,
						.eun.op = AST_OP_DEREF,
						.eun.v = expr->ebin.l,
						.eun.type = *lt.pointer.base,
					);

					lt = *lt.pointer.base;
				}

				da_foreach (StructMember, member, &lt.user->ustruct.members) {
					if (member->kind == STMEM_FIELD) {
						if (strcmp(expr->ebin.r->vid.id,
								member->as.field.id) == 0) {
							expr->ebin.type = member->as.field.type;
							return member->as.field.type;
						}
					}
				}

				throw_error(expr->loc, "no such field");
			}
		}

		Type rt = expr_analysis(p, expr->ebin.r, vart);
		expr->ebin.type = lt;

		if (is_pointer(lt) && is_pointer(rt) && expr->ebin.op == AST_OP_SUB) {
			expr->ebin.type = (Type){.kind = TYPE_IPTR};
		} else if ((lt.kind == TYPE_IPTR && is_pointer(rt)) ||
			(is_pointer(lt) && rt.kind == TYPE_IPTR) ||
			(lt.kind == TYPE_UPTR && is_pointer(rt)) ||
			(is_pointer(lt) && rt.kind == TYPE_UPTR)) {
			Type ptr_type = is_pointer(lt) ? lt : rt;
			expr->ebin.type = (Type) {
				.kind = TYPE_POINTER,
				.pointer.base = get_pointer_base(ptr_type)
			};
		} else if (!compare_types(lt, rt)) {
			AST_Node *le = expr->ebin.l, *re = expr->ebin.r;
			if ((le->kind == AST_LITERAL || re->kind == AST_LITERAL) &&
				(type_is_int(lt) && type_is_int(rt))) {
				AST_Node *lit     = le->kind == AST_LITERAL ? le : re;
				AST_Node *not_lit = le->kind != AST_LITERAL ? le : re;
				lit->literal.type = parser_get_type(p, not_lit);
			} else throw_error(expr->loc, "operation on different types");
		}

		switch (expr->ebin.op) {
		case AST_OP_EQ: case AST_OP_NOT_EQ:
		case AST_OP_LESS_EQ: case AST_OP_GREAT_EQ:
		case AST_OP_GREAT: case AST_OP_LESS:
			expr->ebin.type.kind = TYPE_BOOL;
		default:;
		}

		if (expr->ebin.op == AST_OP_ARR)
			expr->ebin.type = *expr->ebin.type.pointer.base;

		return expr->ebin.type;
	} break;

	case AST_UN_EXP: {
		switch (expr->eun.op) {
		case AST_OP_SIZEOF: {
			expr_analysis(p, expr->eun.v, NULL);
		} break;

		case AST_OP_CAST: {
			expr_analysis(p, expr->eun.v, &expr->eun.type);
		} break;

		case AST_OP_REF: {
			Type vt = expr_analysis(p, expr->eun.v, vart);
			Type *base = malloc(sizeof(Type)); *base = vt;
			expr->eun.type = (Type) {.kind = TYPE_POINTER, .pointer.base = base};
		} break;

		case AST_OP_DEREF: {
			Type vt = expr_analysis(p, expr->eun.v, vart);
			expr->eun.type = vt;
			if (!is_pointer(vt))
				throw_error(expr->eun.v->loc, "pointer expected");
			expr->eun.type = *vt.pointer.base;
		} break;

		default:
			expr->eun.type = expr_analysis(p, expr->eun.v, vart);
		}

		return expr->eun.type;
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

AST_Node *parse_array(Parser *p, Type *vart) {
	Location alc = next(p).loc;
	AST_Nodes array = {0};
	AST_Node *al = ast_new(
		.kind = AST_ARRAY,
		.loc = alc,
	);

	if (!is_pointer(*vart))
		throw_error(alc, "types mismatching");

	while (peek(p).kind != TOK_CBRA) {
		Type base_type = *get_pointer_base(*vart);
		AST_Node *expr = parse_expr(p,
			EXPR_PARSING_ARRAY,
			&base_type
		);

		da_append(&array, expr);
		next(p);

		if (peek(p).kind == TOK_COM)
			next(p);
		else if (peek(p).kind != TOK_CBRA)
			throw_error(alc, "unexpected token");
	}

	//printf("%zu\n%zu\n%p\n", array.count, array.capacity, array.items);

	al->array = array;
	return al;
}

AST_Node *parse_expr(Parser *p, ExprParsingType type, Type *vart) {
	AST_Nodes nodes = {0};

	while (true) {
		if (peek(p).kind == TOK_OPAR) {
			next(p);
			da_append(&nodes, parse_expr(p, EXPR_PARSING_PAR, vart));
		}

		if (type == EXPR_PARSING_FUNC_CALL) {
			if (peek(p).kind == TOK_COM) break;
			else if (peek(p).kind == TOK_CPAR) {
				p->cur_token--;
				break;
			}
		} else if (type == EXPR_PARSING_VAR) {
			if (peek(p).kind == TOK_SEMI) {
				break;
			}
		} else if (type == EXPR_PARSING_SQBRA) {
			if (peek(p).kind == TOK_CSQBRA) {
				next(p);
				break;
			}
		} else if (type == EXPR_PARSING_PAR) {
			if (peek(p).kind == TOK_CPAR) {
				next(p);
				break;
			}
		} else if (type == EXPR_PARSING_STMT) {
			if (
				peek(p).kind == TOK_OBRA  ||
				peek(p).kind == TOK_ARROW ||
				peek(p).kind == TOK_ARROW_EQ
			) {
				p->cur_token--;
				break;
			}
		} else if (type == EXPR_PARSING_ARRAY) {
			if (
				peek(p).kind == TOK_COM ||
				peek(p).kind == TOK_CBRA
			) {
				p->cur_token--;
				break;
			}
		}

		switch (peek(p).kind) {
		case TOK_ID:
			if (peek2(p).kind == TOK_OPAR) {
				if (nodes.count > 0) {
					if (
						da_last(&nodes)->kind    == AST_BIN_EXP  &&
						da_last(&nodes)->ebin.op == AST_OP_FIELD
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
				da_append(&nodes, ast_new(
					.kind = AST_VID,
					.loc = peek(p).loc,
					.vid.id = peek(p).data,
					.vid.uid = var ? var->variable.uid : 0,
				));
				
			}
			break;

		case TOK_INT:
			da_append(&nodes, ast_new(
				.kind = AST_LITERAL,
				.literal.kind = LIT_INT,
				.loc = peek(p).loc,
				.literal.lint = parse_int(peek(p).data),
			));
			break;

		case TOK_TRUE:
			da_append(&nodes, ast_new(
				.kind = AST_LITERAL,
				.loc = peek(p).loc,
				.literal.kind = LIT_BOOL,
				.literal.lint = 1,
			));
			break;

		case TOK_OBRA:
			da_append(&nodes, parse_array(p, vart));
			break;

		case TOK_FALSE:
			da_append(&nodes, ast_new(
				.kind = AST_LITERAL,
				.literal.kind = LIT_BOOL,
				.literal.lint = 0,
			));
			break;

		case TOK_NULL: {
			Type *u0 = malloc(sizeof(Type));
			*u0 = (Type) {.kind = TYPE_NULL};
			da_append(&nodes, ast_new(
				.kind = AST_LITERAL,
				.literal.kind = LIT_INT,
				.literal.lint = 0,
				.literal.type = (Type) {
					.kind = TYPE_POINTER,
					.pointer.base = u0
				},
			));
		} break;

		case TOK_STRING:
			da_append(&nodes, ast_new(
				.kind = AST_LITERAL,
				.loc = peek(p).loc,
				.literal.kind = LIT_STR,
				.literal.str = peek(p).data,
			));
			break;

		case TOK_CHAR:
			da_append(&nodes, ast_new(
				.kind = AST_LITERAL,
				.loc = peek(p).loc,
				.literal.kind = LIT_CHAR,
				.literal.lint = peek(p).data[0],
			));
			break;

		case TOK_FLOAT:
			da_append(&nodes, ast_new(
				.kind = AST_LITERAL,
				.literal.kind = LIT_FLOAT,
				.literal.lfloat = parse_float(peek(p).data),
			));
			break;

		case TOK_COL: {
			Type t = *parse_type(p);
			da_append(&nodes, ast_new(
				.kind = AST_UN_EXP,
				.loc = peek(p).loc,
				.eun.type = t,
				.eun.op = AST_OP_CAST,
				.eun.v = NULL,
			));
		} break;

		case TOK_SIZEOF:
			if (peek2(p).kind == TOK_COL) {
				da_append(&nodes, ast_new(
					.kind = AST_UN_EXP,
					.loc = peek(p).loc,
					.eun.op = get_un_op(next(p)),
					.eun.type = TUPTR,
					.eun.v = ast_new(
						.kind = AST_LITERAL,
						.literal.type = *parse_type(p),
					),
				));
			} else {
				da_append(&nodes, ast_new(
					.kind = AST_UN_EXP,
					.loc = peek(p).loc,
					.eun.op = get_un_op(peek(p)),
					.eun.type = TUPTR,
					.eun.v = NULL,
				));
			}
			break;

		case TOK_EXC: case TOK_TILDA:
			da_append(&nodes, ast_new(
				.kind = AST_UN_EXP,
				.loc = peek(p).loc,
				.eun.op = get_un_op(peek(p)),
				.eun.v = NULL,
			));
			break;

		case TOK_OSQBRA:
			da_append(&nodes, ast_new(
				.kind = AST_BIN_EXP,
				.loc = peek(p).loc,
				.ebin.op = get_bin_op(peek(p)),
				.ebin.l = NULL,
				.ebin.r = NULL
			));
			next(p);
			da_append(&nodes, parse_expr(p, EXPR_PARSING_SQBRA, &TUPTR));
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
			da_append(&nodes, ast_new(
				.kind = AST_BIN_EXP,
				.loc = peek(p).loc,
				.ebin.op = get_bin_op(peek(p)),
				.ebin.l = NULL,
				.ebin.r = NULL
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
					if (isBinOp && da_last(&nodes)->ebin.l &&
						da_last(&nodes)->ebin.r) isBinOp = false;
					if (isBinOp) is_unary_op = true;
				}
			}

			if (!is_unary_op) {
				da_append(&nodes, ast_new(
					.kind = AST_BIN_EXP,
					.loc = peek(p).loc,
					.ebin.op = get_bin_op(peek(p)),
					.ebin.l = NULL,
					.ebin.r = NULL
				));
			} else {
				da_append(&nodes, ast_new(
					.kind = AST_UN_EXP,
					.loc = peek(p).loc,
					.eun.op = get_un_op(peek(p)),
					.eun.v = NULL,
				));
			}
		} break;

		default:
			throw_error(peek(p).loc, "unexpected token");
		}

		next(p);
	}

	AST_Node *expr = expr_expand(&nodes);
	if (expr) expr_analysis(p, expr, vart);
	da_free(&nodes);
	return expr;
}
