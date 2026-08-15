#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <parser.h>

HT_IMPL_STR(UserTypes, UserType)
HT_IMPL(SymbolTable, SymbolKey, Symbol)

u32 SymbolTable_hashf(SymbolKey key) {
	return hash_combine(hash_str(key.id), hash_num(key.kind));
}

int SymbolTable_compare(SymbolKey cur_key, SymbolKey key) {
	return !(strcmp(key.id, cur_key.id) == 0 && key.kind == cur_key.kind);
}

void push_scope(Parser *p) {
	da_append(&p->sss, (SymbolTable){0});
}

void pop_scope(Parser *p) {
	SymbolTable_free(&da_last(&p->sss));
	p->sss.count--;
}

bool sbltbl_glob_add(Parser *p, SymbolKind st, char *id, Symbol smbl) {
	if (SymbolTable_get(&da_first(&p->sss), (SymbolKey){st, id})) return true;
	SymbolTable_add(&da_first(&p->sss), (SymbolKey){st, id}, smbl);
	return false;
}

bool sbltbl_add(Parser *p, SymbolKind st, char *id, Symbol smbl) {
	if (SymbolTable_get(&da_last(&p->sss), (SymbolKey){st, id})) return true;
	SymbolTable_add(&da_last(&p->sss), (SymbolKey){st, id}, smbl);
	return false;
}

Symbol *sbltbl_get(Parser *p, SymbolKind st, char *id) {
	SymbolKey key = {st, id};
	for (int i = p->sss.count - 1; i >= 0; i--) {
		Symbol *smbl = SymbolTable_get(&da_get(&p->sss, i), key);
		if (smbl) return smbl;
	}
	return NULL;
}

void ast_body_append(AST_Node *body, AST_Node *n) {
	da_append(&body->as.body.stmts, n);
}

Type parser_get_type(Parser *p, AST_Node *n) {
	switch (n->kind) {
		case AST_BIN_EXP:     return n->as.ebin.type;
		case AST_UN_EXP:      return n->as.eun.type;
		case AST_LITERAL:     return n->as.literal.type;
		case AST_FUNC_CALL:   return n->as.func_call.type;
		case AST_METHOD_CALL: return n->as.method_call.type;
		case AST_VID: return sbltbl_get(p, SBL_VAR, n->as.vid.id)->variable.type;
		default: UNREACHABLE;
	}
}

void throw_types_mismatch(Location loc, Type t1, Type t2) {
	StringBuilder st1 = {0};
	render_type(&st1, t1);
	StringBuilder st2 = {0};
	render_type(&st2, t2);
	StringBuilder res = {0};
	sb_appendf(&res, "types mismatch (%s and %s)", st1.items, st2.items);
	sb_free(&st1);
	sb_free(&st2);
	throw_error(loc, res.items);
}

long long calc_arr_len(AST_Node *e) {
	switch (e->kind) {
	case AST_LITERAL:
		if (e->as.literal.kind == LIT_INT) return e->as.literal.as.lint;
		else throw_error(e->loc, "expected integer literal");
	case AST_UN_EXP:
		if (e->as.eun.op == AST_OP_NEG) return -calc_arr_len(e->as.eun.v);
		else throw_error(e->loc, "invalid unary operator in array size");
	case AST_BIN_EXP: {
		long long le = calc_arr_len(e->as.ebin.l);
		long long re = calc_arr_len(e->as.ebin.r);
		switch (e->as.ebin.op) {
			case AST_OP_ADD: return le + re;
			case AST_OP_SUB: return le - re;
			case AST_OP_MUL: return le * re;
			case AST_OP_DIV: return le / re;
			default: throw_error(e->loc, "invalid binary operator in array size");
		}
	}
	default:
		throw_error(e->loc, "wrong expression");
	}
	return 0;
}

#define peek(p) (p)->tokens[0]
#define peek2(p) (p)->tokens[1]
#define next(p) (*((p)->tokens++))

void parse_func_args(Parser *p, AST_Nodes *fargs);
Type *parse_type(Parser *p);

Type *parse_type_r(Parser *p) {
	Location loc = peek(p).loc;
	Type *type = NULL;
	if (peek(p).kind == TOK_STAR) {
		next(p);
		type = new(Type,
			.kind = TYPE_POINTER,
			.as.pointer.base = parse_type_r(p));
		return type;
	} else if (peek(p).kind == TOK_OSQBRA) {
		type = new(Type, .kind = TYPE_ARRAY);
		next(p);
		AST_Node *arrLenExpr = parse_expr(p, until(TOK_CSQBRA), &TUPTR); next(p);
		if (arrLenExpr) {
			long long arr_len = calc_arr_len(arrLenExpr);
			if (arr_len <= 0) throw_error(arrLenExpr->loc, "array size must be greater than zero");
			type->as.array.length = arr_len;
		} else type->as.array.length = 0;
		type->as.array.elem = parse_type_r(p);
		return type;
	}
	type = new(Type, 0);
	if (peek(p).kind == TOK_FUNC) {
		type->kind = TYPE_FUNCTION;
		next(p);
		expect(next(p), TOK_OPAR);
		while (true) {
			Type *arg = parse_type_r(p); next(p);
			da_append(&type->as.func.args, *arg);
			if (peek(p).kind == TOK_COM) {
				next(p);
			} else if (peek(p).kind == TOK_CPAR) {
				next(p);
				break;
			}
		}
		if (peek(p).kind == TOK_COL) {
			type->as.func.ret = parse_type(p);
		} else type->as.func.ret = &TU0;
		return type;
	}
	static struct {
		const char *str;
		TypeKind kind;
	} types[] = {
		{ "int",   TYPE_INT   },
		{ "uint",  TYPE_UINT  },
		{ "float", TYPE_FLOAT },
		{ "bool",  TYPE_BOOL  },
		{ "i16",   TYPE_I16   },
		{ "i8",    TYPE_I8    },
		{ "i64",   TYPE_I64   },
		{ "u16",   TYPE_U16   },
		{ "u8",    TYPE_U8    },
		{ "u64",   TYPE_U64   },
		{ "u32",   TYPE_U32   },
		{ "i32",   TYPE_I32   },
		{ "iptr",  TYPE_IPTR  },
		{ "uptr",  TYPE_UPTR  },
		{ "u0",    TYPE_NULL  },
	};
	for (size_t i = 0; i < ARR_LEN(types); i++) {
		if (strcmp(peek(p).data, types[i].str) == 0) {
			type->kind = types[i].kind;
			next(p);
			return type;
		}
	}
	UserType *user_type = UserTypes_get(&p->ut, peek(p).data);
	if (user_type) {
		type->kind = user_type->kind;
		type->as.user = user_type;
		next(p);
		return type;
	}
	throw_error(loc, "incorrect type");
	return type;
}

Type *parse_type(Parser *p) {
	expect(next(p), TOK_COL);
	return parse_type_r(p);
}

void expect_f(Token token, TokenKind type, char *ts) {
	if (token.kind == type) return;
	char err[256]; sprintf(err, "%s expected", ts);
	throw_error(token.loc, err);
}

// Type checking occurs at the analysis stage
AST_Node *parse_method_call(Parser *p) {
	AST_Node *metCall = new(AST_Node,
		.kind = AST_METHOD_CALL,
		.loc = peek(p).loc,
		.as.method_call.id = next(p).data);
	// The first argument of any method is reserved for "self"
	da_append(&metCall->as.method_call.args, NULL);
	expect(next(p), TOK_OPAR);
	while (peek(p).kind != TOK_CPAR) {
		AST_Node *expr = parse_expr(p, until(TOK_COM, TOK_CPAR), NULL);
		if (peek(p).kind == TOK_COM) next(p);
		da_append(&metCall->as.method_call.args, expr);
	}
	next(p);
	return metCall;
}

AST_Node *parse_func_call(Parser *p) {
	AST_Node *fcn = new(AST_Node, .kind = AST_FUNC_CALL);
	fcn->loc = peek(p).loc;
	fcn->as.func_call.id = next(p).data;
	expect(next(p), TOK_OPAR);
	Symbol *fcf = sbltbl_get(p, SBL_FUNC_DEF,    fcn->as.func_call.id);
	Symbol *fce = sbltbl_get(p, SBL_FUNC_EXTERN, fcn->as.func_call.id);
	if (!fcf && !fce) throw_error(fcn->loc, "calling an undeclared function");
	AST_Nodes fargs;
	if (fcf) {
		fcn->as.func_call.type = fcf->func_def.type;
		fargs = fcf->func_def.args;
	} else if (fce) {
		fcn->as.func_call.type = fce->func_extern.type;
		fcn->as.func_call.id = fce->func_extern.extern_smb;
		fargs = fce->func_extern.args;
	}
	size_t arg_cnt = 0;
	bool met_any = false;
	bool is_next_any = false;
	AST_Node *expr;
	while (peek(p).kind != TOK_CPAR) {
		if (!met_any) {
			if (arg_cnt >= fargs.count) {
				throw_error(fcn->loc, "too many arguments");
			}
		}
		if (fargs.items[arg_cnt]->kind == AST_FUNC_DEF_ARG_ANY) {
			met_any = true;
		}
		if (arg_cnt < fargs.count - 1) {
			if (fargs.items[arg_cnt + 1]->kind == AST_FUNC_DEF_ARG_ANY) {
				is_next_any = true;
			}
		}
		if (!met_any) {
			Type farg_type = fargs.items[arg_cnt++]->as.func_def_arg.type;
			expr = parse_expr(p, until(TOK_CPAR, TOK_COM), &farg_type);
			Type expr_type = parser_get_type(p, expr);
			if (!compare_types(expr_type, farg_type)) {
				throw_types_mismatch(expr->loc, expr_type, farg_type);
			}
		} else {
			expr = parse_expr(p, until(TOK_CPAR, TOK_COM), NULL);
		}
		da_append(&fcn->as.func_call.args, expr);
		if (peek(p).kind == TOK_COM) next(p);
	}
	if (!met_any && !is_next_any && arg_cnt < fargs.count)
		throw_error(fcn->loc, "not enough arguments");
	next(p);
	return fcn;
}

static uint var_id_ctr = 1;

AST_Node *parse_var_def(Parser *p) {
	char *id = peek(p).data;
	Location loc = next(p).loc;
	Type type = *parse_type(p);
	AST_Node *vdn = new(AST_Node,
		.kind = AST_VAR_DEF,
		.loc = loc,
		.as.var_def.id = id,
		.as.var_def.uid = var_id_ctr++,
		.as.var_def.type = type,
		.as.var_def.expr = NULL);
	if (peek(p).kind == TOK_EQ) {
		next(p);
		AST_Node *expr = parse_expr(p, until(TOK_SEMI), &type); next(p);
		vdn->as.var_def.expr = expr;
		if (expr->kind == AST_ARRAY &&
			vdn->as.var_def.type.kind == TYPE_ARRAY &&
			vdn->as.var_def.type.as.array.length == 0
		) vdn->as.var_def.type.as.array.length = expr->as.array.count;
	}
	if (sbltbl_add(p, SBL_VAR, vdn->as.var_def.id, (Symbol) {
		.variable.type = vdn->as.var_def.type,
		.variable.uid = vdn->as.var_def.uid,
	})) throw_error(vdn->loc, "redifinition of the variable");
	return vdn;
}

AST_Node *parse_var_assign(Parser *p) {
	char *id = peek(p).data;
	Location loc = next(p).loc;
	next(p);
	AST_Node *expr = parse_expr(p, until(TOK_SEMI), NULL); next(p);
	AST_Node *vdn = new(AST_Node,
		.kind = AST_VAR_DEF,
		.loc = loc,
		.as.var_def.id = id,
		.as.var_def.uid = var_id_ctr++,
		.as.var_def.type = parser_get_type(p, expr),
		.as.var_def.expr = expr);
	if (sbltbl_add(p, SBL_VAR, vdn->as.var_def.id, (Symbol) {
		.variable.type = vdn->as.var_def.type,
		.variable.uid  = vdn->as.var_def.uid,
	})) throw_error(vdn->loc, "redifinition of the variable");
	return vdn;
}

AST_Node *parse_var_mut(Parser *p, TokenKind *until) {
	AST_Node *exp = parse_expr(p, until, NULL); next(p);
	return new(AST_Node,
		.kind = AST_VAR_MUT,
		.loc = exp->loc,
		.as.var_mut.type = exp->as.ebin.type,
		.as.var_mut.expr = exp
	);
}

AST_Node *parse_func_return(Parser *p, AST_Node *func, AST_Node *body) {
	AST_Node *ret = new(AST_Node,
		.kind = AST_FUNC_RET,
		.loc = peek(p).loc,
		.as.func_ret.type = func->as.func_def.type);
	next(p);
	da_foreach (AST_Nodes, defers, &func->as.func_def.defers_stack) {
		da_foreach (AST_Node*, dbody, defers) {
			ast_body_append(body, *dbody);
		}
	}
	if (peek(p).kind == TOK_SEMI) {
		if (ret->as.func_ret.type.kind != TYPE_NULL)
			throw_error(ret->loc, "you must return something");
		ret->as.func_ret.type = TU0;
	} else {
		ret->as.func_ret.expr = parse_expr(p, until(TOK_SEMI), NULL); next(p);
		if (!compare_types(parser_get_type(p, ret->as.func_ret.expr), ret->as.func_ret.type)) {
			throw_types_mismatch(
				ret->as.func_ret.expr->loc,
				parser_get_type(p, ret->as.func_ret.expr),
				ret->as.func_ret.type
			);
		}
	}
	return ret;
}

AST_Node *parse_body(Parser *p, AST_Node *func, bool skip);

AST_Node *parse_if_stmt(Parser *p, AST_Node *func) {
	AST_Node *r = new(AST_Node,
		.kind = AST_IF_STMT,
		.loc = next(p).loc);
	r->as.stmt_if.expr = parse_expr(p, until(TOK_OBRA, TOK_ARROW, TOK_ARROW_EQ), NULL);
	if (parser_get_type(p, r->as.stmt_if.expr).kind != TYPE_BOOL)
		throw_error(r->as.stmt_if.expr->loc, "bool expected");
	r->as.stmt_if.body = parse_body(p, func, false);
	if (peek(p).kind == TOK_ELSE_SYM) {
		next(p);
		if (peek(p).kind == TOK_IF_SYM) {
			r->as.stmt_if.next = parse_if_stmt(p, func);
		} else {
			r->as.stmt_if.next = new(AST_Node,
				.kind = AST_ELSE_STMT,
				.loc = peek(p).loc);
			r->as.stmt_if.next->as.stmt_else.body = parse_body(p, func, false);
		}
	} else r->as.stmt_if.next = NULL;
	return r;
}

AST_Node *parse_while_stmt(Parser *p, AST_Node *func) {
	AST_Node *r = new(AST_Node,
		.kind = AST_WHILE_STMT,
		.loc = next(p).loc);
	r->as.stmt_while.expr = parse_expr(p, until(TOK_OBRA, TOK_ARROW, TOK_ARROW_EQ), NULL);
	if (parser_get_type(p, r->as.stmt_while.expr).kind != TYPE_BOOL)
		throw_error(r->as.stmt_while.expr->loc, "bool expected");
	r->as.stmt_while.body = parse_body(p, func, false);
	return r;
}

AST_Node *parse_for_stmt(Parser *p, AST_Node *func) {
	AST_Node *r = new(AST_Node,
		.kind = AST_FOR_STMT,
		.loc = next(p).loc);
	push_scope(p);
	if (peek2(p).kind == TOK_COL) {
		r->as.stmt_for.var = parse_var_def(p);
	} else if (peek2(p).kind == TOK_EQ) {
		r->as.stmt_for.var = parse_var_mut(p, until(TOK_SEMI));
	} else if (peek2(p).kind == TOK_ASSIGN) {
		r->as.stmt_for.var = parse_var_assign(p);
	}
	r->as.stmt_for.expr = parse_expr(p, until(TOK_SEMI), &r->as.stmt_for.var->as.var_def.type);
	if (parser_get_type(p, r->as.stmt_for.expr).kind != TYPE_BOOL)
		throw_error(r->as.stmt_for.expr->loc, "bool expected");
	next(p);
	r->as.stmt_for.mut = parse_var_mut(p, until(TOK_OBRA, TOK_ARROW, TOK_ARROW_EQ));
	p->tokens--;
	r->as.stmt_for.body = parse_body(p, func, true);
	pop_scope(p);
	return r;
}

AST_Node *parse_body(Parser *p, AST_Node *func, bool skip) {
	bool is_arrow    = peek(p).kind == TOK_ARROW;
	bool is_arrow_eq = peek(p).kind == TOK_ARROW_EQ;
	AST_Node *body = new(AST_Node,
		.kind = AST_BODY,
		.loc = peek(p).loc);
	if (!is_arrow && !is_arrow_eq)
		expect(peek(p), TOK_OBRA);
	next(p);
	da_append(&func->as.func_def.defers_stack, (AST_Nodes){0});
	if (!skip) push_scope(p);
	if (is_arrow_eq) {
		Type ft = func->as.func_def.type;
		AST_Node *en = parse_expr(p, until(TOK_SEMI), &ft);
		Type et = parser_get_type(p, en);
		if (!compare_types(ft, et))
			throw_types_mismatch(en->loc, ft, et);
		da_append(&body->as.body.stmts,
			new(AST_Node,
				.kind = AST_FUNC_RET,
				.as.func_ret.expr = en,
				.as.func_ret.type = et));
		goto done;
	}
	while (true) {
		switch (peek(p).kind) {
		case TOK_CBRA: goto done;
		case TOK_SEMI: next(p); break;
		case TOK_OBRA:
			ast_body_append(body, parse_body(p, func, false));
			break;
		case TOK_ID: {
			switch (peek2(p).kind) {
			case TOK_COL:
				ast_body_append(body, parse_var_def(p));
				break;
			case TOK_ASSIGN:
				ast_body_append(body, parse_var_assign(p));
				break;
			case TOK_OPAR:
				ast_body_append(body, parse_func_call(p));
				expect(peek(p), TOK_SEMI);
				next(p);
				break;
			default:
				ast_body_append(body, parse_var_mut(p, until(TOK_SEMI)));
			}
		} break;
		case TOK_BREAK:
			ast_body_append(body, new(AST_Node,
				.kind = AST_LOOP_BREAK,
				.loc = next(p).loc,
			));
			expect(next(p), TOK_SEMI);
			break;
		case TOK_CONTINUE:
			ast_body_append(body, new(AST_Node,
				.kind = AST_LOOP_CONTINUE,
				.loc = next(p).loc,
			));
			expect(next(p), TOK_SEMI);
			break;
		case TOK_BLOCK:
			next(p);
			ast_body_append(body, parse_body(p, func, false));
			break;
		case TOK_IF_SYM:
			ast_body_append(body, parse_if_stmt(p, func));
			break;
		case TOK_WHILE_SYM:
			ast_body_append(body, parse_while_stmt(p, func));
			break;
		case TOK_FOR_SYM:
			ast_body_append(body, parse_for_stmt(p, func));
			break;
		case TOK_RET:
			ast_body_append(body, parse_func_return(p, func, body));
			break;
		case TOK_DEFER:
			if (peek2(p).kind == TOK_OBRA) next(p);
			else peek(p).kind = TOK_ARROW;
			AST_Node *dbody = parse_body(p, func, false);
			da_append(&da_last(&func->as.func_def.defers_stack), dbody);
			break;
		default:
			ast_body_append(body, parse_var_mut(p, until(TOK_SEMI)));
		}
		if (is_arrow) {
			p->tokens--;
			goto done;
		}
	}

done:
	next(p);
	da_foreach (AST_Node*, dbody, &da_last(&func->as.func_def.defers_stack))
		ast_body_append(body, *dbody);
	func->as.func_def.defers_stack.count--;
	if (!skip) pop_scope(p);
	return body;
}

void parse_func_args(Parser *p, AST_Nodes *fargs) {
	while (peek(p).kind != TOK_CPAR) {
		switch (peek(p).kind) {
		case TOK_COM:
			next(p);
			break;
		case TOK_ID: {
			expect(peek2(p), TOK_COL);
			AST_Node *arg = new(AST_Node,
				.loc = peek(p).loc,
				.kind = AST_FUNC_DEF_ARG,
				.as.func_def_arg.id = peek(p).data,
				.as.func_def_arg.uid = var_id_ctr++,
			);
			next(p);
			arg->as.func_def_arg.type = *parse_type(p);
			da_append(fargs, arg);
			if (sbltbl_add(p, SBL_VAR, arg->as.func_def_arg.id, (Symbol) {
				.variable.type = arg->as.func_def_arg.type,
				.variable.uid = arg->as.func_def_arg.uid,
			})) throw_error(arg->loc, "redifinition of the variable");
		} break;
		case TOK_ANY:;
			AST_Node *arg = new(AST_Node, .kind = AST_FUNC_DEF_ARG_ANY);
			next(p);
			da_append(fargs, arg);
			break;
		default:
			throw_error(peek(p).loc, "unexpected token");
		}
	}
	next(p);
}

AST_Node *parse_function(Parser *p, AST_Node *self, bool is_static) {
	if (next(p).kind == TOK_STATIC) {
		is_static = true;
		expect(next(p), TOK_FUNC);
	}
	AST_Node *fdn = new(AST_Node,
		.kind = AST_FUNC_DEF,
		.loc = peek(p).loc,
		.as.func_def.id = peek(p).data,
		.as.func_def.is_static = is_static);
	char *pref = "method";
	if (strncmp(pref, fdn->as.func_def.id, strlen(pref)) == 0)
		throw_error(fdn->loc, "prefix `method` is reserved, you cannot use it");
	next(p);
	expect(next(p), TOK_OPAR);
	push_scope(p);
	if (self) {
		da_append(&fdn->as.func_def.args, self);
		sbltbl_add(p, SBL_VAR, self->as.func_def_arg.id, (Symbol) {
			.variable.type = self->as.func_def_arg.type,
			.variable.uid = self->as.func_def_arg.uid,
		});
	}
	parse_func_args(p, &fdn->as.func_def.args);
	if (peek(p).kind == TOK_COL) {
		fdn->as.func_def.type = *parse_type(p);
	} else {
		fdn->as.func_def.type = TU0;
	}
	if (self) {
		if (peek(p).kind != TOK_SEMI)
			fdn->as.func_def.body = parse_body(p, fdn, true);
		pop_scope(p);
		return fdn;
	}
	Symbol fds = {
		.func_def.type = fdn->as.func_def.type,
		.func_def.is_def = true,
	};
	for (size_t i = 0; i < fdn->as.func_def.args.count; i++)
		da_append(&fds.func_def.args, da_get(&fdn->as.func_def.args, i));
	Symbol *su = sbltbl_get(p, SBL_FUNC_EX_USED, fdn->as.func_def.id);
	Symbol *se = sbltbl_get(p, SBL_FUNC_EXTERN,  fdn->as.func_def.id);
	Symbol *sf = sbltbl_get(p, SBL_FUNC_DEF,     fdn->as.func_def.id);
	if (se || su) throw_error(fdn->loc, "the symbol is already in use");
	else if (sf) {
		if (sf->func_def.is_def)
			throw_error(fdn->loc, "function redefinition");
		if (!compare_types(sf->func_def.type, fds.func_def.type))
			throw_error(fdn->loc, "wrong function return type in the function declaration");
		if (sf->func_def.args.count != fds.func_def.args.count)
			throw_error(fdn->loc, "wrong number of arguments in the function declaration");
		for (size_t i = 0; i < fds.func_def.args.count; i++) {
			Type l = fds.func_def.args.items[i]->as.func_def_arg.type;
			Type r = sf->func_def.args.items[i]->as.func_def_arg.type;
			if (!compare_types(l, r)) throw_error(fdn->loc, "wrong type in the function declaration");
		}
		sf->func_def.is_def = true;
		fdn->as.func_def.body = parse_body(p, fdn, true);
		pop_scope(p);
		return fdn;
	} else {
		if (peek(p).kind == TOK_SEMI) {
			pop_scope(p);
			fds.func_def.is_def = false;
			sbltbl_add(p, SBL_FUNC_DEF, fdn->as.func_def.id, fds);
			return NULL;
		} else {
			sbltbl_glob_add(p, SBL_FUNC_DEF, fdn->as.func_def.id, fds);
			fdn->as.func_def.body = parse_body(p, fdn, true);
			pop_scope(p);
			return fdn;
		}
	}
}

void parse_extern(Parser *p) {
	next(p);
	expect(peek(p), TOK_ID);
	char *extern_smb = peek(p).data;
	if (peek2(p).kind == TOK_ID)
		next(p);
	expect(peek(p), TOK_ID);
	char *id = peek(p).data;
	Symbol fes = { .func_extern.extern_smb = extern_smb };
	Location loc = next(p).loc;
	expect(next(p), TOK_OPAR);
	push_scope(p);
	parse_func_args(p, &fes.func_extern.args);
	pop_scope(p);
	if (peek(p).kind == TOK_COL) {
		fes.func_extern.type = *parse_type(p);
		next(p);
	} else fes.func_extern.type = TU0;
	Symbol *sf = sbltbl_get(p, SBL_FUNC_DEF,     id);
	Symbol *se = sbltbl_get(p, SBL_FUNC_EXTERN,  id);
	Symbol *su = sbltbl_get(p, SBL_FUNC_EX_USED, id);
	if (sf || se || su) throw_error(loc, "the symbol is already in use");
	sbltbl_add(p, SBL_FUNC_EXTERN, id, fes);
	sbltbl_add(p, SBL_FUNC_EX_USED, extern_smb, (Symbol){0});
}

void parse_method(Parser *p, UserType *st, bool is_static) {
	Type *ut = new(Type,
		.kind = TYPE_STRUCT,
		.as.user = st);
	AST_Node *self = new(AST_Node,
		.kind = AST_FUNC_DEF_ARG,
		.as.func_def_arg.id = "self",
		.as.func_def_arg.uid = var_id_ctr++,
		.as.func_def_arg.type = (Type){
			.kind = TYPE_POINTER,
			.as.pointer.base = ut});
	AST_Node *func = parse_function(p, self, is_static);
	da_foreach (Member, member, &st->as.ustruct.members) {
		if (member->kind == MBR_METHOD) {
			AST_Node *memb = member->as.method.func;
			if (strcmp(memb->as.func_def.id, func->as.func_def.id) != 0)
				continue;
			if (memb->as.func_def.body)
				throw_error(func->loc, "redefinition of method");
			if (!compare_types(memb->as.func_def.type, func->as.func_def.type))
				throw_error(func->loc,
					"return type mismatch between declaration and definition");
			for (size_t i = 0; i < memb->as.func_def.args.count; i++) {
				Type a = memb->as.func_def.args.items[i]->as.func_def_arg.type;
				Type b = func->as.func_def.args.items[i]->as.func_def_arg.type;
				if (!compare_types(a, b)) {
					throw_error(func->loc, "argument type mismatch between declaration and definition");
				}
			}
			da_remove_ordered(
				&st->as.ustruct.members,
				(size_t)(member - st->as.ustruct.members.items));
			member--;
		}
	}
	da_append(&st->as.ustruct.members, ((Member){
		.kind = MBR_METHOD,
		.as.method.func = func,
	}));
}

void parse_struct(Parser *p) {
	next(p);
	if (UserTypes_get(&p->ut, peek(p).data))
		throw_error(peek(p).loc, "redefinition of the struct");
	UserTypes_add(&p->ut, peek(p).data, (UserType){
		.kind = TYPE_STRUCT,
		.id = peek(p).data});
	UserType *st = UserTypes_get(&p->ut, next(p).data);
	expect(next(p), TOK_OBRA);
	while (peek(p).kind != TOK_CBRA) {
		switch (peek(p).kind) {
		case TOK_SEMI:
			next(p);
			break;
		case TOK_STATIC:
		case TOK_FUNC:
			parse_method(p, st, false);
			break;
		case TOK_ID:;
			char *id = next(p).data;
			Type type = *parse_type(p);
			da_append(&st->as.ustruct.members, ((Member){
				.kind = MBR_FIELD,
				.as.field.type = type,
				.as.field.id = id,
			}));
			break;
		default:
			throw_error(peek(p).loc, "unexpected token");
		}
	}
	next(p);
}

void parse_impl(Parser *p) {
	bool is_static = false;
	if (next(p).kind == TOK_STATIC) {
		is_static = true;
		expect(next(p), TOK_IMPL);
	}
	Location snl = peek(p).loc;
	UserType *st = UserTypes_get(&p->ut, next(p).data);
	if (!st) throw_error(snl, "no such struct or union");
	expect(next(p), TOK_OBRA);
	while (peek(p).kind != TOK_CBRA) {
		switch (peek(p).kind) {
		case TOK_SEMI:
			next(p);
			break;
		case TOK_STATIC:
		case TOK_FUNC:
			parse_method(p, st, is_static);
			break;
		default:
			throw_error(peek(p).loc, "unexpected token");
		}
	}
	next(p);
}

Parser parser_parse(Token *tokens) {
	Parser p = {0};
	AST_Node *prog = new(AST_Node, .kind = AST_PROG);
	p.program = prog;
	p.tokens = tokens;
	push_scope(&p);
	while (peek(&p).kind != TOK_EOF) {
		switch (peek(&p).kind) {
		case TOK_SEMI:   next(&p);         break;
		case TOK_STRUCT: parse_struct(&p); break;
		case TOK_IMPL:   parse_impl(&p);   break;
		case TOK_EXTERN: parse_extern(&p); break;
		case TOK_STATIC: {
			if (peek2(&p).kind == TOK_FUNC) {
				AST_Node *func = parse_function(&p, NULL, true);
				if (func) da_append(&prog->as.program.stmts, func);
			} else if (peek2(&p).kind == TOK_IMPL) {
				parse_impl(&p);
			}
		} break;
		case TOK_FUNC: {
			AST_Node *func = parse_function(&p, NULL, false);
			if (func) da_append(&prog->as.program.stmts, func);
		} break;
		case TOK_ID:
			if (peek2(&p).kind == TOK_COL) {
				da_append(&prog->as.program.stmts, parse_var_def(&p));
			} else if (peek2(&p).kind == TOK_ASSIGN) {
				da_append(&prog->as.program.stmts, parse_var_assign(&p));
			} else throw_error(peek(&p).loc, "unexpected top level declaration");
			break;
		default:
			throw_error(peek(&p).loc, "unexpected top level declaration");
		}
	}
	return p;
}
