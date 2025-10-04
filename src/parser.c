#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <threads.h>

#include "../include/parser.h"

bool check_nested(int pn[16], int n[16]) {
	for (size_t i = 0; n[i] != 0; i++) {
		if (pn[i] != n[i]) return false;
	}

	return true;
}

void set_nested(int pn[16], int n[16]) {
	memcpy(n, pn, 16 * sizeof(int));
}

void parser_st_add(Parser *parser, Symbol smbl) {
	set_nested(parser->nested, smbl.nested);
	da_append(&parser->st, smbl);
}

Symbol *parser_st_get(Parser *parser, const char *id) {
	for (int i = parser->st.count - 1; i >= 0; i--) {
		if (strcmp(da_get(&parser->st, i).id, id) == 0 &&
			check_nested(da_get(&parser->st, i).nested, parser->nested)) {
			return &da_get(&parser->st, i);
		}
	}

	return NULL;
}

Type parse_type(Parser *parser) {
	expect_token(parser->cur_token, TOK_TYPE);
	Type type = {0};

	if (parser->cur_token->type == 0) {}

	char *tok_data = parser->cur_token->data;
	if (strcmp(tok_data, "int") == 0) {
		type.kind = TYPE_INT;
		type.size = 4;
	} else if (strcmp(tok_data, "float") == 0) {
		type.kind = TYPE_FLOAT;
		type.size = 4;
	} else if (strcmp(tok_data, "bool") == 0) {
		type.kind = TYPE_BOOL;
		type.size = 1;
	}

	return type;
}

AST_Node *ast_alloc(AST_Node node) {
	AST_Node *new = malloc(sizeof(AST_Node)); 
	memcpy(new, &node, sizeof(node));
	return new;
}

void expect_token(Token *token, TokenType type) {
	if (token->type == type) return;

	size_t lines_num = token->location.line_num + 1;
	const char *tok_str = tok_to_str(type);

	printf("%zu parser error: %s token expected\n", lines_num, tok_str);
	exit(1);
}

void unexpect_token(Token *token, TokenType type) {
	if (token->type != type) return;

	size_t lines_num = token->location.line_num + 1;
	const char *tok_str = tok_to_str(type);

	printf("%zu parser error: unexpected token %s\n", lines_num, tok_str);
	exit(1);
}

AST_Node *parse_var_def(Parser *parser) {
	char *id = (parser->cur_token++)->data;
	Type type = parse_type(parser);
	parser->cur_token++;

	AST_Node *vdn = ast_alloc((AST_Node){
		.type = AST_VAR_DEF,
		.var_def.id = id,
		.var_def.type = type,
		.var_def.exp = NULL,
	});

	if (parser->cur_token->type == TOK_EQ) {
		parser->cur_token++;
		vdn->var_def.exp = parse_expr(parser, EXPR_PARSING_VAR);
	}

	parser_st_add(parser, (Symbol) {
		.id = vdn->var_def.id,
		.variable.type = type,
	});

	return vdn;
}

AST_Node *parse_var_assign(Parser *parser) {
	char *id = parser->cur_token->data;
	parser->cur_token += 2;

	AST_Node *exp = parse_expr(parser, EXPR_PARSING_VAR);

	AST_Node *vdn = ast_alloc((AST_Node){
		.type = AST_VAR_DEF,
		.var_def.id = id,
		.var_def.exp = exp,
	});

	switch (exp->type) {
		case AST_BIN_EXP: vdn->var_def.type = exp->exp_binary.type; break;
		case AST_UN_EXP:  vdn->var_def.type = exp->exp_unary.type; break;
		case AST_INT:     vdn->var_def.type = (Type) { .kind = TYPE_INT, .size = 4 }; break;
		default: break;
	}

	parser_st_add(parser, (Symbol) {
		.id = vdn->var_def.id,
		.variable.type = vdn->var_def.type,
	});

	return vdn;
}

AST_Node *parse_var_mut(Parser *parser) {
	char *id = (parser->cur_token++)->data;
	Symbol *s = parser_st_get(parser, id);
	parser->cur_token++;

	AST_Node *vmn = ast_alloc((AST_Node){
		.type = AST_VAR_MUT,
		.var_mut.type = s->variable.type,
		.var_mut.id = id,
		.var_mut.exp = parse_expr(parser, EXPR_PARSING_VAR),
	});

	return vmn;
}

AST_Node *parse_func_return(Parser *parser, AST_Node *func) {
	AST_Node *ret = ast_alloc((AST_Node){.type = AST_FUNC_RET});
	parser->cur_token++;
	ret->func_ret.exp = parse_expr(parser, EXPR_PARSING_VAR);
	ret->func_ret.type = func->func_def.type;
	return ret;
}

AST_Node *parse_body(Parser *parser, AST_Node *func);

AST_Node *parse_if_stmt(Parser *parser, AST_Node *func) {
	parser->cur_token++;

	AST_Node *r = ast_alloc((AST_Node){
		.type = AST_IF_STMT });

	r->stmt_if.exp = parse_expr(parser, EXPR_PARSING_STMT);
	r->stmt_if.body = parse_body(parser, func);

	return r;
}

AST_Node *parse_while_stmt(Parser *parser, AST_Node *func) {
	parser->cur_token++;

	AST_Node *r = ast_alloc((AST_Node){
		.type = AST_WHILE_STMT });

	r->stmt_while.exp = parse_expr(parser, EXPR_PARSING_STMT);
	r->stmt_while.body = parse_body(parser, func);

	return r;
}

AST_Node *parse_body(Parser *parser, AST_Node *func) {
	AST_Node *body = ast_alloc((AST_Node){.type = AST_BODY});
	int br_cnt = 0;

	while (true) {
		switch (parser->cur_token->type) {
			case TOK_OBRA:
				br_cnt++;
				break;

			case TOK_CBRA:
				br_cnt--;
				if (br_cnt == 0)
					goto ex;
				break;

			case TOK_ID:
				if ((parser->cur_token+1)->type == TOK_TYPE)
					da_append(&body->body.stmts, parse_var_def(parser));
				else if ((parser->cur_token+1)->type == TOK_EQ)
					da_append(&body->body.stmts, parse_var_mut(parser));
				else if ((parser->cur_token+1)->type == TOK_ASSIGN)
					da_append(&body->body.stmts, parse_var_assign(parser));
				else if ((parser->cur_token+1)->type == TOK_OPAR)
					da_append(&body->body.stmts, parse_func_call(parser));
				break;

			case TOK_IF_SYM:
				da_append(&body->body.stmts, parse_if_stmt(parser, func));
				break;

			case TOK_WHILE_SYM:
				da_append(&body->body.stmts, parse_while_stmt(parser, func));
				break;

			case TOK_RET:
				da_append(&body->body.stmts, parse_func_return(parser, func));
				break;

			default:
				/* TODO: error handling */
				break;
		}

		parser->cur_token++;
	}

ex:
	return body;
}

AST_Node *parse_function(Parser *parser) {
	parser->cur_token++;

	expect_token(parser->cur_token, TOK_ID);
	AST_Node *fdn = ast_alloc((AST_Node){
		.type = AST_FUNC_DEF,
		.func_def.id = parser->cur_token->data,
	});

	parser->cur_token++;
	expect_token(parser->cur_token++, TOK_OPAR);


	while (parser->cur_token->type != TOK_CPAR) {
		switch (parser->cur_token->type) {
			case TOK_COM:
				unexpect_token(parser->cur_token+1, TOK_COM);
				break;

			case TOK_ID:
				expect_token(parser->cur_token+1, TOK_TYPE);
				AST_Node *farg = ast_alloc((AST_Node){
					.type = AST_FUNC_DEF_ARG,
					.func_def_arg.id = parser->cur_token->data,
				});

				parser->cur_token++;
				farg->func_def_arg.type = parse_type(parser);
				da_append(&fdn->func_def.args, farg);
				parser->cur_token++;
				break;

			default:
				unexpect_token(parser->cur_token, parser->cur_token->type);
				break;
		}

		parser->cur_token++;
	}

	parser->cur_token++;
	if (parser->cur_token->type == TOK_TYPE) {
		fdn->func_def.type = parse_type(parser);
		parser->cur_token++;
	}

	Symbol fds = {
		.type = SBL_FUNC_DEF,
		.id = fdn->func_def.id,
	};

	for (size_t i = 0; i < fdn->func_def.args.count; i++)
		da_append(&fds.func_def.args, da_get(&fdn->func_def.args, i));

	parser_st_add(parser, fds);
	fdn->func_def.body = parse_body(parser, fdn);
	return fdn;
}

void parser_parse(Parser *parser, Token *tokens) {
	AST_Node *prog = ast_alloc((AST_Node){.type = AST_PROG});
	parser->program = prog;
	parser->cur_token = tokens;

	while (parser->cur_token->type != TOK_EOF) {
		switch (parser->cur_token->type) {
			case TOK_FUNC:
				da_append(&prog->program.stmts, parse_function(parser));
				break;

			default:
				// TODO: error handling
				break;
		}

		parser->cur_token++;
	}
}
