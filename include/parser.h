#ifndef _PARSER_H_
#define _PARSER_H_

#include <stdlib.h>
#include <stdint.h>
#include <threads.h>
#include "../include/lexer.h"

typedef enum {
	AST_IF_STMT, AST_BIN_EXP, AST_UN_EXP,
	AST_VARIABLE, AST_INT, AST_FUNC_ARG,
	AST_FUNC_CALL, AST_ID, AST_FLOAT,
	AST_STRING, AST_TYPE, AST_OP_PLUS,
	AST_OP_MINUS, AST_OP_MULT, AST_OP_DEV,
	AST_FUNC_DEF, AST_FUNC_RET_TYPE,
	AST_FOR_STMT,
} AST_NodeType;

typedef struct AST_Node {
    AST_NodeType type;
    union {
		struct {
			struct AST_Node *name;
			struct AST_Node *block;
			//struct AST_Node *args;
			//size_t argc;
		} function;
		struct {
			struct AST_Node **stmts;
			size_t count;
		} block;
		struct {	
			struct AST_Node *name;
			struct AST_Node *type;
			struct AST_Node *exp;
		} variable;
		struct {
			struct AST_Node *op;
			struct AST_Node *v;
		} unary_exp;
		struct {
			struct AST_Node *op;
			struct AST_Node *l, *r;
		} binary_exp;
		struct {
			char op;
		} exp_oper;
		struct {
			char *id;
		} identifier;
		struct {
			int64_t val;
		} int_num;
		struct {
			double val;
		} float_num;
    } payload;
} AST_Node;

typedef struct {
	Token *tokens;
	size_t tokens_num;
	AST_Node *tail;
	AST_Node *head;
} Parser;

void ast_parse_bin_exp();
void parser_alloc();
void parser_parse(Parser *parser);
void parser_free(Parser parser);

#endif
