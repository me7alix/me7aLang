#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "../include/parser.h"

static Token peek(Parser *p)  { return *p->cur_token;       }
static Token peek2(Parser *p) { return *(p->cur_token + 1); }
static Token next(Parser *p)  { return *(p->cur_token++);   }

HT_IMPL_STR(UserTypes, UserType*)
HT_IMPL(SymbolTable, SymbolKey, Symbol)

u32 SymbolTable_hashf(SymbolKey key) {
	return hash_combine(hash_str(key.id), hash_num(key.type));
}

int SymbolTable_compare(SymbolKey cur_key, SymbolKey key) {
	return !(strcmp(key.id, cur_key.id) == 0 && key.type == cur_key.type);
}

void parser_push_scope(Parser *p) {
	da_append(&p->sss, (SymbolTable){0});
}

void parser_pop_scope(Parser *p) {
	SymbolTable_free(&da_last(&p->sss));
	p->sss.count--;
}

static uint VUID = 1;
bool parser_symbol_table_add(Parser *p, SymbolType st, char *id, Symbol smbl) {
	if (SymbolTable_get(&da_last(&p->sss), (SymbolKey) { st, id })) return true;
	SymbolTable_add(&da_last(&p->sss), (SymbolKey) { st, id }, smbl);
	return false;
}

Symbol *parser_symbol_table_get(Parser *p, SymbolType st, char *id) {
	SymbolKey key = {st, id};

	for (int i = p->sss.count - 1; i >= 0; i--) {
		Symbol *smbl = SymbolTable_get(&da_get(&p->sss, i), key);
		if (smbl) return smbl;
	}

	return NULL;
}

Type parser_get_type(Parser *p, AST_Node *n) {
	switch (n->kind) {
	case AST_BIN_EXP:     return n->expr_binary.type;
	case AST_UN_EXP:      return n->expr_unary.type;
	case AST_LITERAL:     return n->literal.type;
	case AST_FUNC_CALL:   return n->func_call.type;
	case AST_METHOD_CALL: return n->method_call.type;
	case AST_VID:         return parser_symbol_table_get(p, SBL_VAR, n->vid.id)->variable.type;
	default: UNREACHABLE;
	}
}

bool compare_types(Type a, Type b) {
	if (is_pointer(a) && is_pointer(b)) {
		if (
			get_pointer_base(a)->kind != get_pointer_base(b)->kind &&
			!(
				get_pointer_base(a)->kind == TYPE_NULL ||
				get_pointer_base(b)->kind == TYPE_NULL
			)
		) return false;
	} else if (a.kind != b.kind) {
		return false;
	}

	return true;
}

long long calc_arr_len(AST_Node *e) {
	switch (e->kind) {
	case AST_LITERAL: {
		if (e->literal.kind != LIT_INT)
			throw_error(e->loc, "expected integer literal");
		return e->literal.lint;
	} break;

	case AST_UN_EXP: {
		switch (e->expr_unary.op) {
			case AST_OP_NEG: return -calc_arr_len(e->expr_unary.v);
			default: throw_error(e->loc, "invalid unary operator in array size");
		}
	} break;

	case AST_BIN_EXP: {
		size_t le = calc_arr_len(e->expr_binary.l);
		size_t re = calc_arr_len(e->expr_binary.r);
		switch (e->expr_binary.op) {
			case AST_OP_ADD: return le + re;
			case AST_OP_SUB: return le - re;
			case AST_OP_MUL: return le * re;
			case AST_OP_DIV: return le / re;
			default: throw_error(e->loc, "invalid binary operator in array size");
		}
	} break;

	default:
		throw_error(e->loc, "wrong expression");
	}

	return 0;
}

Type *parse_type_r(Parser *p) {
	Location loc = peek(p).loc;
	Type *type = NULL;

	if (peek(p).kind == TOK_STAR) {
		next(p);

		type = type_new(
			.kind = TYPE_POINTER,
			.pointer.base = parse_type_r(p),
		);

		return type;
	} else if (peek(p).kind == TOK_OSQBRA) {
		type = type_new(.kind = TYPE_ARRAY);
		next(p);

		static Type tuptr = (Type) {.kind = TYPE_UPTR};
		AST_Node *arrLenExpr = parse_expr(p, EXPR_PARSING_SQBRA, &tuptr);
		long long calculatedArrLen = calc_arr_len(arrLenExpr);

		if (calculatedArrLen <= 0)
			throw_error(arrLenExpr->loc, "array size must be greater than zero");

		type->array.length = calculatedArrLen;

		type->array.elem = parse_type_r(p);
		return type;
	}


	type = type_new();
	char *tn = peek(p).data;
	if      (!strcmp(tn, "int"))   type->kind = TYPE_INT;
	else if (!strcmp(tn, "uint"))  type->kind = TYPE_UINT;
	else if (!strcmp(tn, "float")) type->kind = TYPE_FLOAT;
	else if (!strcmp(tn, "bool"))  type->kind = TYPE_BOOL;
	else if (!strcmp(tn, "i16"))   type->kind = TYPE_I16;
	else if (!strcmp(tn, "i8"))    type->kind = TYPE_I8;
	else if (!strcmp(tn, "i64"))   type->kind = TYPE_I64;
	else if (!strcmp(tn, "u16"))   type->kind = TYPE_U16;
	else if (!strcmp(tn, "u8"))    type->kind = TYPE_U8;
	else if (!strcmp(tn, "u64"))   type->kind = TYPE_U64;
	else if (!strcmp(tn, "u32"))   type->kind = TYPE_U32;
	else if (!strcmp(tn, "i32"))   type->kind = TYPE_I32;
	else if (!strcmp(tn, "iptr"))  type->kind = TYPE_IPTR;
	else if (!strcmp(tn, "uptr"))  type->kind = TYPE_UPTR;
	else if (!strcmp(tn, "u0"))    type->kind = TYPE_NULL;
	else {
		UserType **user_type = UserTypes_get(&p->ut, tn);
		if (user_type) {
			type->kind = (*user_type)->kind;
			type->user = *user_type;
		} else throw_error(loc, "incorrect type");
	}

	return type;
}

Type *parse_type(Parser *p) {
	expect_token(next(p), TOK_COL);
	return parse_type_r(p);
}

AST_Node *ast_alloc(AST_Node node) {
	AST_Node *new = malloc(sizeof(AST_Node));
	memcpy(new, &node, sizeof(node));
	return new;
}

void expect_token_f(Token token, TokenKind type, char *ts) {
	if (token.kind == type) return;
	char err[256]; sprintf(err, "%s expected", ts);
	throw_error(token.loc, err);
}

// Type checking occurs at the analysis stage
AST_Node *parse_method_call(Parser *p) {
	AST_Node *metCall = ast_new(
		.kind = AST_METHOD_CALL,
		.loc = peek(p).loc,
		.method_call.id = next(p).data,
	);
	
	// the first argument of any method is reserved for "self"
	da_append(&metCall->method_call.args, NULL);

	expect_token(next(p), TOK_OPAR);
	while (peek(p).kind != TOK_CPAR) {
		AST_Node *expr = parse_expr(p, EXPR_PARSING_FUNC_CALL, NULL);
		da_append(&metCall->method_call.args, expr);
		next(p);
	}

	next(p);
	return metCall;
}

AST_Node *parse_func_call(Parser *p) {
	AST_Node *fcn = ast_new(.kind = AST_FUNC_CALL);
	fcn->loc = peek(p).loc;
	fcn->func_call.id = next(p).data;
	expect_token(peek(p), TOK_OPAR);
	next(p);

	Symbol *fcf = parser_symbol_table_get(p, SBL_FUNC_DEF,    fcn->func_call.id);
	Symbol *fce = parser_symbol_table_get(p, SBL_FUNC_EXTERN, fcn->func_call.id);
	if (!fcf && !fce) throw_error(fcn->loc, "calling an undeclared function");

	AST_Nodes fargs;
	if (fcf) {
		fcn->func_call.type = fcf->func_def.type;
		fargs = fcf->func_def.args;
	} else if (fce) {
		fcn->func_call.type = fce->func_extern.type;
		fcn->func_call.id = fce->func_extern.extern_smb;
		fargs = fce->func_extern.args;
	}

	size_t arg_cnt = 0;
	bool met_any = false;
	bool is_next_any = false;
	AST_Node *expr;

	while (peek(p).kind != TOK_CPAR) {
		if (!met_any)
			if (arg_cnt >= fargs.count)
				throw_error(fcn->loc, "too many arguments");

		if (fargs.items[arg_cnt]->kind == AST_FUNC_DEF_ARG_ANY)
			met_any = true;

		if (arg_cnt < fargs.count - 1)
			if (fargs.items[arg_cnt + 1]->kind == AST_FUNC_DEF_ARG_ANY)
				is_next_any = true;

		if (!met_any) {
			Type farg_type = fargs.items[arg_cnt++]->func_def_arg.type;
			expr = parse_expr(p, EXPR_PARSING_FUNC_CALL, &farg_type);
			Type expr_type = parser_get_type(p, expr);

			if (!compare_types(expr_type, farg_type))
				throw_error(expr->loc, "types mismatching");
		} else {
			expr = parse_expr(p, EXPR_PARSING_FUNC_CALL, NULL);
		}

		da_append(&fcn->func_call.args, expr);
		next(p);
	}

	if (!met_any && !is_next_any && arg_cnt < fargs.count)
		throw_error(fcn->loc, "not enough arguments");

	next(p);
	return fcn;
}

AST_Node *parse_var_def(Parser *p) {
	char *id = peek(p).data;
	Location loc = next(p).loc;
	Type type = *parse_type(p);
	next(p);

	AST_Node *vdn = ast_new(
		.kind = AST_VAR_DEF,
		.loc = loc,
		.var_def.id = id,
		.var_def.uid = VUID++,
		.var_def.type = type,
		.var_def.expr = NULL,
	);

	if (peek(p).kind == TOK_EQ) {
		next(p);
		vdn->var_def.expr = parse_expr(p, EXPR_PARSING_VAR, &type);
	}

	if (parser_symbol_table_add(p, SBL_VAR, vdn->var_def.id, (Symbol) {
		.variable.type = type,
		.variable.uid = vdn->var_def.uid,
	})) throw_error(vdn->loc, "redifinition of the variable");

	return vdn;
}

AST_Node *parse_var_assign(Parser *p) {
	char *id = peek(p).data;
	Location loc = peek(p).loc;
	next(p);
	next(p);

	AST_Node *expr = parse_expr(p, EXPR_PARSING_VAR, NULL);

	AST_Node *vdn = ast_new(
		.kind = AST_VAR_DEF,
		.loc = loc,
		.var_def.id = id,
		.var_def.uid = VUID++,
		.var_def.expr = expr,
	);

	switch (expr->kind) {
	case AST_BIN_EXP:   vdn->var_def.type = expr->expr_binary.type; break;
	case AST_UN_EXP:    vdn->var_def.type = expr->expr_unary.type;  break;
	case AST_LITERAL:   vdn->var_def.type = expr->literal.type;     break;
	case AST_FUNC_CALL: vdn->var_def.type = expr->func_call.type;   break;
	case AST_VID:
		Symbol *s = parser_symbol_table_get(
			p, SBL_VAR, expr->vid.id);
		vdn->var_def.type = s->variable.type;
		break;
	default: UNREACHABLE;
	}

	if (parser_symbol_table_add(p, SBL_VAR, vdn->var_def.id, (Symbol) {
		.variable.type = vdn->var_def.type,
		.variable.uid = vdn->var_def.uid,
	})) throw_error(vdn->loc, "redifinition of the variable");

	return vdn;
}

AST_Node *parse_var_mut(Parser *p, ExprParsingType pt) {
	AST_Node *exp = parse_expr(p, pt, NULL);
	AST_Node *vmn = ast_new(
		.kind = AST_VAR_MUT,
		.loc = exp->loc,
		.var_mut.type = exp->expr_binary.type,
		.var_mut.expr = exp,
	);

	return vmn;
}

AST_Node *parse_func_return(Parser *p, AST_Node *func) {
	AST_Node *ret = ast_new(
		.kind = AST_FUNC_RET,
		.loc = peek(p).loc,
		.func_ret.type = func->func_def.type,
	);

	next(p);
	if (peek(p).kind == TOK_SEMI) {
		if (ret->func_ret.type.kind != TYPE_NULL)
			throw_error(ret->loc, "you must return something");
		ret->func_ret.type = (Type) {.kind = TYPE_NULL};
	} else {
		ret->func_ret.expr = parse_expr(p, EXPR_PARSING_VAR, NULL);
		if (!compare_types(parser_get_type(p, ret->func_ret.expr), ret->func_ret.type))
			throw_error(ret->func_ret.expr->loc, "wrong type");
	}

	return ret;
}

AST_Node *parse_body(Parser *p, AST_Node *func, bool skip);

AST_Node *parse_if_stmt(Parser *p, AST_Node *func) {
	AST_Node *r = ast_new(
		.kind = AST_IF_STMT,
		.loc = peek(p).loc,
	);

	next(p);

	r->stmt_if.expr = parse_expr(p, EXPR_PARSING_STMT, NULL);
	if (parser_get_type(p, r->stmt_if.expr).kind != TYPE_BOOL)
		throw_error(r->stmt_while.expr->loc, "bool expected");
	next(p);

	r->stmt_if.body = parse_body(p, func, false);

	if (peek2(p).kind == TOK_ELSE_SYM) {
		next(p);
		if (peek2(p).kind == TOK_IF_SYM) {
			next(p);
			r->stmt_if.next = parse_if_stmt(p, func);
		} else {
			r->stmt_if.next = ast_new(
				.kind = AST_ELSE_STMT,
				.loc = next(p).loc,
			);
			r->stmt_if.next->stmt_else.body = parse_body(p, func, false);
		}
	} else r->stmt_if.next = NULL;

	return r;
}

AST_Node *parse_while_stmt(Parser *p, AST_Node *func) {
	next(p);
	AST_Node *r = ast_new(.kind = AST_WHILE_STMT);

	r->stmt_while.expr = parse_expr(p, EXPR_PARSING_STMT, NULL);
	if (parser_get_type(p, r->stmt_while.expr).kind != TYPE_BOOL)
		throw_error(r->stmt_while.expr->loc, "bool expected");

	next(p);
	r->stmt_while.body = parse_body(p, func, false);

	return r;
}

AST_Node *parse_for_stmt(Parser *p, AST_Node *func) {
	AST_Node *r = ast_new(
		.kind = AST_FOR_STMT,
		.loc = peek(p).loc,
	);
	next(p);

	parser_push_scope(p);

	if (peek2(p).kind == TOK_COL) {
		r->stmt_for.var = parse_var_def(p);
	} else if (peek2(p).kind == TOK_EQ) {
		r->stmt_for.var = parse_var_mut(p, EXPR_PARSING_VAR);
	} else if (peek2(p).kind == TOK_ASSIGN) {
		r->stmt_for.var = parse_var_assign(p);
	}
	next(p);

	r->stmt_for.expr = parse_expr(p, EXPR_PARSING_VAR, &r->stmt_for.var->var_def.type);
	if (parser_get_type(p, r->stmt_for.expr).kind != TYPE_BOOL)
		throw_error(r->stmt_while.expr->loc, "bool expected");
	next(p);

	r->stmt_for.mut = parse_var_mut(p, EXPR_PARSING_STMT);
	next(p);

	r->stmt_for.body = parse_body(p, func, true);

	parser_pop_scope(p);
	return r;
}

AST_Node *parse_body(Parser *p, AST_Node *func, bool skip) {
	if (!skip)
		parser_push_scope(p);

	AST_Node *body = ast_new(.kind = AST_BODY);
	expect_token(peek(p), TOK_OBRA);
	next(p);

	while (true) {
		switch (peek(p).kind) {
		case TOK_SEMI: break;
		case TOK_CBRA: goto ex;

		case TOK_OBRA:
			da_append(&body->body.stmts, parse_body(p, func, false));
			break;

		case TOK_ID: {
			if (peek2(p).kind == TOK_COL)
				da_append(&body->body.stmts, parse_var_def(p));
			else if (peek2(p).kind == TOK_ASSIGN)
				da_append(&body->body.stmts, parse_var_assign(p));
			else if (peek2(p).kind == TOK_OPAR)
				da_append(&body->body.stmts, parse_func_call(p));
			else da_append(&body->body.stmts, parse_var_mut(p, EXPR_PARSING_VAR));
		} break;

		case TOK_BREAK:
			da_append(&body->body.stmts, ast_new(
				.kind = AST_LOOP_BREAK,
				.loc = next(p).loc,
			));
			expect_token(peek(p), TOK_SEMI);
			break;

		case TOK_CONTINUE:
			da_append(&body->body.stmts, ast_new(
				.kind = AST_LOOP_CONTINUE,
				.loc = next(p).loc,
			));
			expect_token(peek(p), TOK_SEMI);
			break;

		case TOK_BLOCK:
			next(p);
			da_append(&body->body.stmts, parse_body(p, func, false));
			break;

		case TOK_IF_SYM:    da_append(&body->body.stmts, parse_if_stmt(p, func));     break;
		case TOK_WHILE_SYM: da_append(&body->body.stmts, parse_while_stmt(p, func));  break;
		case TOK_FOR_SYM:   da_append(&body->body.stmts, parse_for_stmt(p, func));    break;
		case TOK_RET:       da_append(&body->body.stmts, parse_func_return(p, func)); break;
		default: da_append(&body->body.stmts, parse_var_mut(p, EXPR_PARSING_VAR));    break;
		}

		next(p);
	}

ex:
	if (!skip)
		parser_pop_scope(p);
	return body;
}

void parse_func_args(Parser *p, AST_Nodes *fargs) {
	while (peek(p).kind != TOK_CPAR) {
		switch (peek(p).kind) {
		case TOK_COM: break;
		case TOK_ID: {
			expect_token(peek2(p), TOK_COL);
			AST_Node *arg = ast_new(
				.loc = peek(p).loc,
				.kind = AST_FUNC_DEF_ARG,
				.func_def_arg.id = peek(p).data,
				.func_def_arg.uid = VUID++,
			);

			next(p);
			arg->func_def_arg.type = *parse_type(p);
			da_append(fargs, arg);

			if (parser_symbol_table_add(p, SBL_VAR, arg->func_def_arg.id, (Symbol) {
				.variable.type = arg->func_def_arg.type,
				.variable.uid = arg->func_def_arg.uid,
			})) throw_error(arg->loc, "redifinition of the variable");
		} break;

		case TOK_ANY:
			AST_Node *arg = ast_new(.kind = AST_FUNC_DEF_ARG_ANY);
			da_append(fargs, arg);
			break;

		default:
			expect_token(peek(p), p->cur_token->kind);
		}

		next(p);
	}

	next(p);
}

AST_Node *parse_function(Parser *p, AST_Node *self) {
	bool is_static = false;
	if (next(p).kind == TOK_STATIC) {
		is_static = true;
		expect_token(next(p), TOK_FUNC);
	}

	AST_Node *fdn = ast_new(
		.kind = AST_FUNC_DEF,
		.loc = peek(p).loc,
		.func_def.id = peek(p).data,
		.func_def.is_static = is_static,
	);

	char *pref = "method";
	if (strncmp(pref, fdn->func_def.id, strlen(pref)) == 0)
		throw_error(fdn->loc, "`method` prefix is reserved, you cannot use it");

	next(p);
	expect_token(next(p), TOK_OPAR);
	
	parser_push_scope(p);

	if (self) {
		da_append(&fdn->func_def.args, self);
		parser_symbol_table_add(p, SBL_VAR, self->func_def_arg.id, (Symbol) {
			.variable.type = self->func_def_arg.type,
			.variable.uid = self->func_def_arg.uid,
		});
	}

	parse_func_args(p, &fdn->func_def.args);

	if (peek(p).kind == TOK_COL) {
		fdn->func_def.type = *parse_type(p);
		next(p);
	} else {
		fdn->func_def.type = (Type) {.kind = TYPE_NULL};
	}

	if (self) {
		if (peek(p).kind != TOK_SEMI)
			fdn->func_def.body = parse_body(p, fdn, true);
		parser_pop_scope(p);
		return fdn;
	}

	Symbol fds = {
		.func_def.type = fdn->func_def.type,
		.func_def.is_def = true,
	};

	for (size_t i = 0; i < fdn->func_def.args.count; i++)
		da_append(&fds.func_def.args, da_get(&fdn->func_def.args, i));

	Symbol *seu = parser_symbol_table_get(p, SBL_FUNC_EX_USED, fdn->func_def.id);
	Symbol *se  = parser_symbol_table_get(p, SBL_FUNC_EXTERN,  fdn->func_def.id);
	Symbol *sf  = parser_symbol_table_get(p, SBL_FUNC_DEF,     fdn->func_def.id);

	if (se || seu) throw_error(fdn->loc, "the symbol is already in use");
	else if (sf) {
		if (sf->func_def.is_def)
			throw_error(fdn->loc, "function redefinition");
		if (!compare_types(sf->func_def.type, fds.func_def.type))
			throw_error(fdn->loc, "wrong function return type in the function declaration");
		if (sf->func_def.args.count != fds.func_def.args.count)
			throw_error(fdn->loc, "wrong number of arguments in the function declaration");
		for (size_t i = 0; i < fds.func_def.args.count; i++) {
			Type l = fds.func_def.args.items[i]->func_def_arg.type;
			Type r = sf->func_def.args.items[i]->func_def_arg.type;
			if (!compare_types(l, r))
				throw_error(fdn->loc, "wrong type in the function declaration");
		}

		sf->func_def.is_def = true;
	} else {
		if (peek(p).kind == TOK_SEMI) {
			parser_pop_scope(p);
			fds.func_def.is_def = false;
			parser_symbol_table_add(p, SBL_FUNC_DEF, fdn->func_def.id, fds);
			return NULL;
		} else {
			fdn->func_def.body = parse_body(p, fdn, true);	
			parser_pop_scope(p);
			parser_symbol_table_add(p, SBL_FUNC_DEF, fdn->func_def.id, fds);
			return fdn;
		}
	}

	fdn->func_def.body = parse_body(p, fdn, true);	
	parser_pop_scope(p);

	return fdn;
}

void parse_extern(Parser *p) {
	next(p);

	expect_token(peek(p), TOK_ID);
	char *extern_smb = peek(p).data;

	if (peek2(p).kind == TOK_ID)
		next(p);

	expect_token(peek(p), TOK_ID);

	char *id = peek(p).data;
	Symbol fes = { .func_extern.extern_smb = extern_smb };
	Location loc = peek(p).loc;

	next(p);
	expect_token(next(p), TOK_OPAR);

	parser_push_scope(p);
	parse_func_args(p, &fes.func_extern.args);
	parser_pop_scope(p);

	if (peek(p).kind == TOK_COL) {
		fes.func_extern.type = *parse_type(p);
		next(p);
	} else {
		fes.func_extern.type = (Type) {.kind = TYPE_NULL};
	}

	Symbol *sf  = parser_symbol_table_get(p, SBL_FUNC_DEF,     id);
	Symbol *se  = parser_symbol_table_get(p, SBL_FUNC_EXTERN,  id);
	Symbol *seu = parser_symbol_table_get(p, SBL_FUNC_EX_USED, id);

	if (sf || se || seu)
		throw_error(loc, "the symbol is already in use used");

	parser_symbol_table_add(p, SBL_FUNC_EXTERN, id, fes);
	parser_symbol_table_add(p, SBL_FUNC_EX_USED, extern_smb, (Symbol){0});
	expect_token(peek(p), TOK_SEMI);
}

void parse_method(Parser *p, UserType *st) {
	Type *ut = type_new(
		.kind = TYPE_STRUCT,
		.user = st
	);

	AST_Node *self = ast_new(
		.kind = AST_FUNC_DEF_ARG,
		.func_def_arg.id = "self",
		.func_def_arg.uid = VUID++,
		.func_def_arg.type = (Type){
			.kind = TYPE_POINTER,
			.pointer.base = ut,
		},
	);

	AST_Node *func = parse_function(p, self);
	da_append(&st->ustruct.members, ((StructMember){
		.kind = STMEM_METHOD,
		.as.method.func = func,
	}));
}

void parse_struct(Parser *p) {
	next(p);

	if (UserTypes_get(&p->ut, peek(p).data))
		throw_error(peek(p).loc, "redefinition of the struct");

	UserType *st = malloc(sizeof(*st));
	*st = (UserType){
		.kind = TYPE_STRUCT,
		.id = peek(p).data,
	};

	UserTypes_add(&p->ut, next(p).data, st);

	expect_token(next(p), TOK_OBRA);
	while (peek(p).kind != TOK_CBRA) {
		switch (peek(p).kind) {
		case TOK_STATIC:
		case TOK_FUNC:
			parse_method(p, st);
			break;

		case TOK_ID: {
			char *id = next(p).data;
			Type type = *parse_type(p);

			da_append(&st->ustruct.members, ((StructMember){
				.kind = STMEM_FIELD,
				.as.field.type = type,
				.as.field.id = id,
			}));
		} break;

		default:
			throw_error(peek(p).loc, "unexpected token");
		}

		next(p);
		if (peek(p).kind == TOK_SEMI)
			next(p);
	}
}

void parse_impl(Parser *p) {
	next(p);

	Location snl = peek(p).loc;
	UserType **stc = UserTypes_get(&p->ut, next(p).data);
	if (!stc) throw_error(snl, "no such struct or union");
	UserType *st = *stc;

	expect_token(next(p), TOK_OBRA);
	while (peek(p).kind != TOK_CBRA) {
		switch (peek(p).kind) {
		case TOK_STATIC:
		case TOK_FUNC:
			parse_method(p, st);
			break;

		default:
			throw_error(peek(p).loc, "unexpected token");
		}

		next(p);
		if (peek(p).kind == TOK_SEMI)
			next(p);
	}
}

Parser parser_parse(Token *tokens) {
	Parser p = {0};
	AST_Node *prog = ast_new(.kind = AST_PROG);
	p.program = prog;
	p.cur_token = tokens;

	parser_push_scope(&p);

	while (peek(&p).kind != TOK_EOF) {
		switch (peek(&p).kind) {
		case TOK_SEMI:                       break;
		case TOK_STRUCT: parse_struct(&p);   break;
		case TOK_IMPL:   parse_impl(&p);     break;
		case TOK_EXTERN: parse_extern(&p);   break;
		case TOK_IMPORT: next(&p); next(&p); break;

		case TOK_MACRO_OBJ:
			while (peek(&p).kind != TOK_SEMI)
				next(&p);
			break;

		case TOK_MACRO_FUNC:
			next(&p);
			expect_token(next(&p), TOK_ID);
			expect_token(next(&p), TOK_OPAR);
			while (peek(&p).kind != TOK_CPAR)
				next(&p);

			next(&p);
			expect_token(next(&p), TOK_OBRA);
			int braCnt = 1;
			while (true) {
				next(&p);
				if (peek(&p).kind == TOK_CBRA) {
					braCnt--;
					if (braCnt == 0) break;
				} else if (peek(&p).kind == TOK_OBRA) {
					braCnt++;
				}
			}
			break;

		case TOK_STATIC:
		case TOK_FUNC: {
			AST_Node *func = parse_function(&p, NULL);
			if (func) da_append(&prog->program.stmts, func);
		} break;

		case TOK_ID:
			if (peek2(&p).kind == TOK_COL) {
				parse_var_def(&p);
			} else if (peek2(&p).kind == TOK_ASSIGN) {
				parse_var_assign(&p);
			} else throw_error(peek(&p).loc, "unexpected top level declaration");
			break;

		default:
			throw_error(peek(&p).loc, "unexpected top level declaration");
		}

		next(&p);
	}

	return p;
}
