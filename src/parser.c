#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>

#include "../include/parser.h"

uint nested_uniq = 1;
#define nested_push_next(p) (p)->nested[(p)->nptr++] = nested_uniq
#define nested_push(p) (p)->nested[(p)->nptr++] = nested_uniq++
#define nested_pop(p) (p)->nested[--(p)->nptr] = 0

bool check_nested(int pn[16], int n[16]) {
	for (size_t i = 0; n[i] != 0; i++) {
		if (pn[i] != n[i]) return false;
	}

	return true;
}

void set_nested(int pn[16], int n[16]) {
	memcpy(n, pn, 16 * sizeof(int));
}

HT_IMPL_STR(UserTypes, UserType)
HT_IMPL(SymbolTable, SymbolKey, Symbol)

u64 SymbolTable_hashf(SymbolKey key) {
	u64 res; strhash(&res, key.id);
	return hash_combine(res, numhash(key.type));
}

int SymbolTable_compare(SymbolKey cur_key, SymbolKey key) {
	if (key.type == SBL_VAR) if (!check_nested(key.nested, cur_key.nested)) return 1;
	return !(strcmp(key.id, cur_key.id) == 0 && key.type == cur_key.type);
}

void parser_symbol_table_add(Parser *p, SymbolType st, char *id, Symbol smbl) {
	int *nested = malloc(sizeof p->nested);
	set_nested(p->nested, nested);
	SymbolTable_add(&p->st, (SymbolKey) { st, id, nested }, smbl);
}

Symbol *parser_symbol_table_get(Parser *p, SymbolType st, char *id) {
	return SymbolTable_get(&p->st, (SymbolKey) { st, id, p->nested });
}

Type parser_get_type(Parser *p, AST_Node *n) {
	switch (n->kind) {
		case AST_BIN_EXP:   return n->expr_binary.type;
		case AST_UN_EXP:    return n->expr_unary.type;
		case AST_LITERAL:   return n->literal.type;
		case AST_FUNC_CALL: return n->func_call.type;
		case AST_VID:       return parser_symbol_table_get(p, SBL_VAR, n->vid)->variable.type;
		default: UNREACHABLE;
	}
}

bool compare_types(Type a, Type b) {
	if (a.kind != b.kind) return false;
	if (is_pointer(a) && is_pointer(b)) {
		if (get_pointer_base(a)->kind != get_pointer_base(b)->kind &&
			!(get_pointer_base(a)->kind == TYPE_NULL || get_pointer_base(b)->kind == TYPE_NULL)) {
			return false;
		}
	}
	return true;
}

Token *parser_peek(Parser *p) { return p->cur_token; }
Token *parser_looknext(Parser *p) { return p->cur_token+1; }
Token *parser_next(Parser *p) { return p->cur_token++; }

Type parse_type(Parser *p) {
	expect_token(parser_peek(p), TOK_COL);
	Location loc = parser_peek(p)->loc;
	Type type = {0};
	parser_next(p);

	bool is_ptr = false;
	bool is_arr = false;
	size_t arr_len;

	if (parser_peek(p)->kind == TOK_STAR) {
		is_ptr = true;
		parser_next(p);
	} else if (parser_peek(p)->kind == TOK_OSQBRA) {
		is_arr = true;
		parser_next(p);
		expect_token(parser_peek(p), TOK_INT);
		arr_len = (size_t) parse_int(parser_peek(p)->data);
		parser_next(p);
		expect_token(parser_next(p), TOK_CSQBRA);
	}

	char *tn = parser_peek(p)->data;
	if      (!strcmp(tn, "int"))   type.kind = TYPE_INT;
	else if (!strcmp(tn, "uint"))  type.kind = TYPE_UINT;
	else if (!strcmp(tn, "float")) type.kind = TYPE_FLOAT;
	else if (!strcmp(tn, "bool"))  type.kind = TYPE_BOOL;
	else if (!strcmp(tn, "i16"))   type.kind = TYPE_I16;
	else if (!strcmp(tn, "i8"))    type.kind = TYPE_I8;
	else if (!strcmp(tn, "i64"))   type.kind = TYPE_I64;
	else if (!strcmp(tn, "u16"))   type.kind = TYPE_U16;
	else if (!strcmp(tn, "u8"))    type.kind = TYPE_U8;
	else if (!strcmp(tn, "u64"))   type.kind = TYPE_U64;
	else if (!strcmp(tn, "iptr"))  type.kind = TYPE_IPTR;
	else if (!strcmp(tn, "uptr"))  type.kind = TYPE_UPTR;
	else if (!strcmp(tn, "u0"))    type.kind = TYPE_NULL;
	else {
		UserType *utype = UserTypes_get(&p->ut, tn);
		if (utype) {
			type.kind = utype->kind;
			type.user = utype;
		} else lexer_error(loc, "error: no such type");
	}

	if (is_ptr) {
		Type *base = malloc(sizeof(Type));
		*base = type;
		type = (Type) {
			.kind = TYPE_POINTER,
			.pointer.base = base
		};
	} else if (is_arr) {
		Type *base = malloc(sizeof(Type));
		*base = type;
		type = (Type) {
			.kind = TYPE_ARRAY,
			.array.elem = base,
			.array.length = arr_len
		};
	}

	return type;
}

AST_Node *ast_alloc(AST_Node node) {
	AST_Node *new = malloc(sizeof(AST_Node));
	memcpy(new, &node, sizeof(node));
	return new;
}

void expect_token(Token *token, TokenKind type) {
	if (token->kind == type) return;

	char err[256];
	sprintf(err, "error: unexpected token");
	lexer_error(token->loc, err);
	exit(1);
}

AST_Node *parse_func_call(Parser *p) {
	AST_Node *fcn = ast_new({ .kind = AST_FUNC_CALL });
	fcn->loc = parser_peek(p)->loc;
	fcn->func_call.id = parser_next(p)->data;
	expect_token(parser_peek(p), TOK_OPAR);
	parser_next(p);

	Symbol *fcf = parser_symbol_table_get(p, SBL_FUNC_DEF, fcn->func_call.id);
	Symbol *fce = parser_symbol_table_get(p, SBL_FUNC_EXTERN, fcn->func_call.id);
	if (!fcf && !fce) lexer_error(fcn->loc, "error: calling an undeclared function");

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

	while (parser_peek(p)->kind != TOK_CPAR) {
		if (fargs.items[arg_cnt]->kind == AST_FUNC_DEF_ARG_ANY)
			met_any = true;

		if (arg_cnt < fargs.count - 1)
			if (fargs.items[arg_cnt + 1]->kind == AST_FUNC_DEF_ARG_ANY)
				is_next_any = true;

		if (!met_any) {
			if (arg_cnt + 1 > fargs.count)
				lexer_error(fcn->loc, "error: wrong number of arguments");

			Type farg_type = fargs.items[arg_cnt++]->func_def_arg.type;
			expr = parse_expr(p, EXPR_PARSING_FUNC_CALL, &farg_type);
			Type expr_type = parser_get_type(p, expr);

			if (!compare_types(expr_type, farg_type))
				lexer_error(expr->loc, "error: wrong type");
		} else {
			expr = parse_expr(p, EXPR_PARSING_FUNC_CALL, NULL);
		}

		da_append(&fcn->func_call.args, expr);
		parser_next(p);
	}

	if (!met_any && !is_next_any && arg_cnt < fargs.count)
		lexer_error(fcn->loc, "error: wrong number of arguments");

	parser_next(p);
	return fcn;
}

AST_Node *parse_var_def(Parser *p) {
	char *id = parser_peek(p)->data;
	Location loc = parser_next(p)->loc;
	Type type = parse_type(p);
	parser_next(p);

	AST_Node *vdn = ast_new({
		.kind = AST_VAR_DEF,
		.loc = loc,
		.var_def.id = id,
		.var_def.type = type,
		.var_def.exp = NULL,
	});

	if (parser_peek(p)->kind == TOK_EQ) {
		parser_next(p);
		vdn->var_def.exp = parse_expr(p, EXPR_PARSING_VAR, &type);
	}

	if (parser_symbol_table_get(p, SBL_VAR, vdn->var_def.id))
		lexer_error(vdn->loc, "error: redifinition of the variable");

	parser_symbol_table_add(p, SBL_VAR, vdn->var_def.id, (Symbol) {
		.variable.type = type,
	});

	return vdn;
}

AST_Node *parse_var_assign(Parser *p) {
	char *id = parser_peek(p)->data;
	Location loc = parser_peek(p)->loc;
	parser_next(p);
	parser_next(p);

	AST_Node *exp = parse_expr(p, EXPR_PARSING_VAR, NULL);

	AST_Node *vdn = ast_new({
		.kind = AST_VAR_DEF,
		.loc = loc,
		.var_def.id = id,
		.var_def.exp = exp,
	});

	switch (exp->kind) {
		case AST_BIN_EXP:   vdn->var_def.type = exp->expr_binary.type; break;
		case AST_UN_EXP:    vdn->var_def.type = exp->expr_unary.type;  break;
		case AST_LITERAL:   vdn->var_def.type = exp->literal.type;    break;
		case AST_FUNC_CALL: vdn->var_def.type = exp->func_call.type;  break;
		case AST_VID:       vdn->var_def.type = parser_symbol_table_get(p, SBL_VAR, exp->vid)->variable.type; break;
		default: UNREACHABLE;
	}

	assert(vdn->var_def.type.kind);
	Symbol *vds = parser_symbol_table_get(p, SBL_VAR, vdn->var_def.id);
	if (vds) lexer_error(vdn->loc, "error: redifinition of the variable");
	parser_symbol_table_add(p, SBL_VAR, vdn->var_def.id, (Symbol) {
		.variable.type = vdn->var_def.type,
	});

	return vdn;
}

AST_Node *parse_var_mut(Parser *p, ExprParsingType pt) {
	AST_Node *exp = parse_expr(p, pt, NULL);
	AST_Node *vmn = ast_new({
		.kind = AST_VAR_MUT,
		.loc = exp->loc,
		.var_mut.type = exp->expr_binary.type,
		.var_mut.exp = exp,
	});

	return vmn;
}

AST_Node *parse_func_return(Parser *p, AST_Node *func) {
	AST_Node *ret = ast_new({
		.kind = AST_FUNC_RET,
		.loc = parser_peek(p)->loc,
		.func_ret.type = func->func_def.type,
	});

	parser_next(p);
	if (parser_peek(p)->kind == TOK_SEMI) {
		if (ret->func_ret.type.kind != TYPE_NULL)
			lexer_error(ret->loc, "error: you must return something");
		ret->func_ret.type = (Type) {.kind = TYPE_NULL};
	} else {
		ret->func_ret.expr = parse_expr(p, EXPR_PARSING_VAR, NULL);
		if (!compare_types(parser_get_type(p, ret->func_ret.expr), ret->func_ret.type))
			lexer_error(ret->func_ret.expr->loc, "error: wrong type");
	}

	return ret;
}

AST_Node *parse_body(Parser *p, AST_Node *func);

AST_Node *parse_if_stmt(Parser *p, AST_Node *func) {
	parser_next(p);
	AST_Node *r = ast_new({
		.kind = AST_IF_STMT,
		.loc = (parser_peek(p)-1)->loc,
	});

	r->stmt_if.exp = parse_expr(p, EXPR_PARSING_STMT, NULL);
	parser_next(p);
	r->stmt_if.body = parse_body(p, func);

	if (parser_looknext(p)->kind == TOK_ELSE_SYM) {
		parser_next(p);
		if (parser_looknext(p)->kind == TOK_IF_SYM) {
			parser_next(p);
			r->stmt_if.next = parse_if_stmt(p, func);
		} else {
			r->stmt_if.next = ast_new({
				.kind = AST_ELSE_STMT,
				.loc = parser_next(p)->loc,
			});
			r->stmt_if.next->stmt_else.body = parse_body(p, func);
		}
	} else r->stmt_if.next = NULL;

	return r;
}

AST_Node *parse_while_stmt(Parser *p, AST_Node *func) {
	parser_next(p);
	AST_Node *r = ast_new({.kind = AST_WHILE_STMT});

	r->stmt_while.exp = parse_expr(p, EXPR_PARSING_STMT, NULL);
	parser_next(p);
	r->stmt_while.body = parse_body(p, func);

	return r;
}

AST_Node *parse_for_stmt(Parser *p, AST_Node *func) {
	parser_next(p);

	AST_Node *r = ast_new({
		.kind = AST_FOR_STMT,
		.loc = (parser_peek(p)-1)->loc,
	});

	nested_push_next(p);

	if ((parser_looknext(p))->kind == TOK_COL)
		r->stmt_for.var = parse_var_def(p);
	else if ((parser_looknext(p))->kind == TOK_EQ)
		r->stmt_for.var = parse_var_mut(p, EXPR_PARSING_VAR);
	else if ((parser_looknext(p))->kind == TOK_ASSIGN)
		r->stmt_for.var = parse_var_assign(p);
	parser_next(p);

	r->stmt_for.expr = parse_expr(p, EXPR_PARSING_VAR, &r->stmt_for.var->var_def.type);
	parser_next(p);
	r->stmt_for.mut = parse_var_mut(p, EXPR_PARSING_STMT);
	parser_next(p);

	nested_pop(p);

	r->stmt_for.body = parse_body(p, func);
	return r;
}

AST_Node *parse_body(Parser *p, AST_Node *func) {
	nested_push(p);

	AST_Node *body = ast_new({.kind = AST_BODY});
	expect_token(parser_peek(p), TOK_OBRA);
	parser_next(p);

	while (true) {
		switch (parser_peek(p)->kind) {
			case TOK_CBRA: goto ex;
			case TOK_OBRA:
				da_append(&body->body.stmts, parse_body(p, func));
				break;

			case TOK_ID: {
				if ((parser_looknext(p))->kind == TOK_COL)
					da_append(&body->body.stmts, parse_var_def(p));
				else if ((parser_looknext(p))->kind == TOK_ASSIGN)
					da_append(&body->body.stmts, parse_var_assign(p));
				else if ((parser_looknext(p))->kind == TOK_OPAR)
					da_append(&body->body.stmts, parse_func_call(p));
				else da_append(&body->body.stmts, parse_var_mut(p, EXPR_PARSING_VAR));
			} break;

			case TOK_BREAK:
				da_append(&body->body.stmts, ast_new({
					.kind = AST_LOOP_BREAK,
					.loc = parser_next(p)->loc,
				}));
				expect_token(parser_peek(p), TOK_SEMI);
				break;

			case TOK_CONTINUE:
				da_append(&body->body.stmts, ast_new({
					.kind = AST_LOOP_CONTINUE,
					.loc = parser_next(p)->loc,
				}));
				expect_token(parser_peek(p), TOK_SEMI);
				break;

			case TOK_IF_SYM:    da_append(&body->body.stmts, parse_if_stmt(p, func));     break;
			case TOK_WHILE_SYM: da_append(&body->body.stmts, parse_while_stmt(p, func));  break;
			case TOK_FOR_SYM:   da_append(&body->body.stmts, parse_for_stmt(p, func));    break;
			case TOK_RET:       da_append(&body->body.stmts, parse_func_return(p, func)); break;
			default: da_append(&body->body.stmts, parse_var_mut(p, EXPR_PARSING_VAR));    break;
		}

		parser_next(p);
	}

ex:
	nested_pop(p);
	return body;
}

void parse_func_args(Parser *p, AST_Nodes *fargs) {
	while (parser_peek(p)->kind != TOK_CPAR) {
		switch (parser_peek(p)->kind) {
			case TOK_COM: break;
			case TOK_ID: {
				expect_token(parser_looknext(p), TOK_COL);
				AST_Node *arg = ast_new({
					.kind = AST_FUNC_DEF_ARG,
					.func_def_arg.id = parser_peek(p)->data
				});

				parser_next(p);
				arg->func_def_arg.type = parse_type(p);
				da_append(fargs, arg);

				nested_push_next(p);
				parser_symbol_table_add(p, SBL_VAR, arg->func_def_arg.id, (Symbol) {
					.variable.type = arg->func_def_arg.type,
				});
				nested_pop(p);
			} break;

			case TOK_ANY: {
				AST_Node *arg = ast_new({.kind = AST_FUNC_DEF_ARG_ANY});
				da_append(fargs, arg);
			} break;

			default: {
				expect_token(parser_peek(p), p->cur_token->kind);
			} break;
		}

		parser_next(p);
	}

	parser_next(p);
}

AST_Node *parse_function(Parser *p) {
	parser_next(p);
	AST_Node *fdn = ast_new({
		.kind = AST_FUNC_DEF,
		.loc = parser_peek(p)->loc,
		.func_def.id = parser_peek(p)->data
	});

	parser_next(p);
	expect_token(parser_next(p), TOK_OPAR);

	parse_func_args(p, &fdn->func_def.args);

	if (parser_peek(p)->kind == TOK_COL) {
		fdn->func_def.type = parse_type(p);
		parser_next(p);
	} else {
		fdn->func_def.type = (Type) {.kind = TYPE_NULL};
	}

	Symbol fds = {
		.func_def.type = fdn->func_def.type,
		.func_def.is_def = true,
	};

	for (size_t i = 0; i < fdn->func_def.args.count; i++)
		da_append(&fds.func_def.args, da_get(&fdn->func_def.args, i));

	Symbol *fdsc_e = parser_symbol_table_get(p, SBL_FUNC_EXTERN, fdn->func_def.id);
	Symbol *fdsc_f = parser_symbol_table_get(p, SBL_FUNC_DEF, fdn->func_def.id);
	if (fdsc_e) lexer_error(fdn->loc, "error: the symbol is already in use");
	else if (fdsc_f) {
		if (fdsc_f->func_def.is_def)
			lexer_error(fdn->loc, "error: function redefinition");
		if (!compare_types(fdsc_f->func_def.type, fds.func_def.type))
			lexer_error(fdn->loc, "error: wrong function return type in the function declaration");
		if (fdsc_f->func_def.args.count != fds.func_def.args.count)
			lexer_error(fdn->loc, "error: wrong number of arguments in the function declaration");
		for (size_t i = 0; i < fds.func_def.args.count; i++) {
			Type l = fds.func_def.args.items[i]->func_def_arg.type;
			Type r = fdsc_f->func_def.args.items[i]->func_def_arg.type;
			if (!compare_types(l, r))
				lexer_error(fdn->loc, "error: wrong type in the function declaration");
		}

		fdsc_f->func_def.is_def = true;
	} else {
		if (parser_peek(p)->kind == TOK_SEMI) {
			fds.func_def.is_def = false;
			parser_symbol_table_add(p, SBL_FUNC_DEF, fdn->func_def.id, fds);
			return NULL;
		} else parser_symbol_table_add(p, SBL_FUNC_DEF, fdn->func_def.id, fds);
	}

	fdn->func_def.body = parse_body(p, fdn);
	nested_uniq++;
	return fdn;
}

void parse_extern(Parser *p) {
	parser_next(p);

	expect_token(parser_peek(p), TOK_ID);
	Location loc = parser_peek(p)->loc;
	char *extern_smb = parser_peek(p)->data;

	if (parser_peek(p)[1].kind == TOK_ID)
		parser_next(p);

	expect_token(parser_peek(p), TOK_ID);

	char *id = parser_peek(p)->data;
	Symbol fes = { .func_extern.extern_smb = extern_smb };

	parser_next(p);
	expect_token(parser_next(p), TOK_OPAR);

	parse_func_args(p, &fes.func_extern.args);

	if (parser_peek(p)->kind == TOK_COL) {
		fes.func_extern.type = parse_type(p);
		parser_next(p);
	} else {
		fes.func_extern.type = (Type) {.kind = TYPE_NULL};
	}

	Symbol *smbl_f = parser_symbol_table_get(p, SBL_FUNC_DEF, id);
	Symbol *smbl_e = parser_symbol_table_get(p, SBL_FUNC_EXTERN, id);
	if (smbl_f || smbl_e)
		lexer_error(loc, "error: the symbol is already in use used");

	parser_symbol_table_add(p, SBL_FUNC_EXTERN, id, fes);
	expect_token(parser_peek(p), TOK_SEMI);
	nested_uniq++;
}

void parse_struct(Parser *p) {
	parser_next(p);

	UserType st = { .kind = TYPE_STRUCT };
	char *struct_id = parser_next(p)->data;

	expect_token(parser_next(p), TOK_OBRA);
	while (parser_peek(p)->kind != TOK_CBRA) {
		expect_token(parser_peek(p), TOK_ID);
		char *id = parser_next(p)->data;
		Type type = parse_type(p);
		da_append(&st.ustruct.fields, ((Field){type, id}));
		parser_next(p);
		if (parser_peek(p)->kind == TOK_COM)
			parser_next(p);
	}

	UserTypes_add(&p->ut, struct_id, st);
}

Parser parser_parse(Token *tokens) {
	Parser p = {0};
	AST_Node *prog = ast_new({.kind = AST_PROG});
	p.program = prog;
	p.cur_token = tokens;

	while (parser_peek(&p)->kind != TOK_EOF) {
		switch (parser_peek(&p)->kind) {
			case TOK_STRUCT:
				parse_struct(&p);
				break;

			case TOK_EXTERN:
				parse_extern(&p);
				break;

			case TOK_IMPORT:
				parser_next(&p);
				parser_next(&p);
				break;

			case TOK_MACRO:
				while (parser_peek(&p)->kind != TOK_SEMI)
					parser_next(&p);
				break;

			case TOK_FUNC: {
				AST_Node *func = parse_function(&p);
				if (func) da_append(&prog->program.stmts, func);
			} break;

			case TOK_ID:
				if ((parser_looknext(&p))->kind == TOK_COL) {
					parse_var_def(&p);
				} else if ((parser_looknext(&p))->kind == TOK_ASSIGN) {
					parse_var_assign(&p);
				} else lexer_error(parser_peek(&p)->loc, "error: unexpected top level declaration");
				break;

			default:
				lexer_error(parser_peek(&p)->loc, "error: unexpected top level declaration");
				break;
		}

		parser_next(&p);
	}

	return p;
}
