#ifndef PARSER_H
#define PARSER_H

#include <stdlib.h>
#include <stdint.h>
#include <threads.h>
#include <stdbool.h>

#define DA_DEBUG
#include "../thirdparty/da.h"
#include "../include/lexer.h"

typedef enum {
	EXPR_PARSING_VAR, EXPR_PARSING_FUNC_CALL,
	EXPR_PARSING_VAR_BRA,
} ExprParsingType;

typedef enum {
	AST_IF_STMT, AST_BIN_EXP, AST_UN_EXP,
	AST_VAR_DEF, AST_VAR, AST_INT, AST_FLOAT,
	AST_FUNC_CALL_ARG,AST_FUNC_CALL,
	AST_STRING, AST_TYPE, AST_OP_PLUS,
	AST_FUNC_DEF, AST_FUNC_DEF_ARG,
	AST_FUNC_RET_TYPE, AST_FOR_STMT,
	AST_UN_OP, AST_BIN_OP, AST_PROG,
} AST_NodeType;

typedef da(struct AST_Node*) AST_Nodes;

typedef struct AST_Node {
	AST_NodeType type;
	union {
		struct {
			char *id;
			AST_Nodes args;
			struct AST_Node *block;
		} func_def;
		struct {
			AST_Nodes stmts;
		} program;
		struct {
			char *id;
			AST_Nodes args;
		} func_call;
		struct {
			AST_Nodes stmts;
		} block;
		struct {
			char *id, *type;
			struct AST_Node *exp;
		} variable;
		struct {
			struct AST_Node *exp;
			struct AST_Node *block;
		} stmt_if;
		struct {
			struct AST_Node *exp;
			struct AST_Node *block;
		} stmt_while;
		struct {
			char op;
			struct AST_Node *v;
		} exp_unary;
		struct {
			char op;
			struct AST_Node *l, *r;
		} exp_binary;
		struct {
			char *id;
			char *type;
		} func_def_arg;
		char *var_id;
		int64_t num_int;
		double num_float;
	} payload;
} AST_Node;

typedef enum {
	SBL_FUNC_DEF, SBL_VAR,
} SymbolType;

typedef struct {
	SymbolType type;
	char *id;
	int nested[16];
	union {
		struct {
			da(AST_Node*) args;
		} func_def;
		struct {
			char *type;
		} variable;
	} payload;
} Symbol;

typedef da(Symbol) SymbolTable;

Symbol *st_get(SymbolTable *st, int nested[16], const char *id);

typedef struct {
	Token *cur_token;
	SymbolTable st;
	int nested[16];
	size_t nested_ptr;
	AST_Node *program;
} Parser;

void parser_alloc();
void parser_parse(Parser *parser);
void parser_free(Parser parser);
void expect_token(Token *token, TokenType type);
AST_Node *parse_expr(Parser *parser, ExprParsingType type);
AST_Node *ast_alloc(AST_Node node);

#endif
