#ifndef PARSER_H
#define PARSER_H

#include <stdlib.h>
#include <stdint.h>
#include <threads.h>
#include <stdbool.h>

#include "../thirdparty/da.h"
#include "../include/lexer.h"

typedef enum {
	TYPE_INT, TYPE_UINT,
	TYPE_I8, TYPE_U8,
	TYPE_I16, TYPE_U16,
	TYPE_I32, TYPE_U32,
	TYPE_I64, TYPE_U64,

	TYPE_FLOAT,
	TYPE_F16,
	TYPE_F32,
	TYPE_F64,

	TYPE_BOOL,
	TYPE_POINTER,
	TYPE_ARRAY,
	TYPE_FUNCTION,
	TYPE_STRUCT,
} TypeKind;

typedef struct Type Type;

struct Type {
	TypeKind kind;

	union {
		struct { Type *base; } pointer;
		struct { Type *elem; size_t length; } array;
		struct { Type **params; size_t param_count; struct Type* ret; } function;
		struct { char *name; } user;
	};

	size_t size;
};

typedef enum {
	EXPR_PARSING_VAR, EXPR_PARSING_FUNC_CALL,
	EXPR_PARSING_PAR, EXPR_PARSING_STMT,
} ExprParsingType;

typedef enum {
	AST_WHILE_STMT,
	AST_IF_STMT, AST_BIN_EXP, AST_UN_EXP,
	AST_VAR_DEF, AST_VAR, AST_INT, AST_FLOAT,
	AST_FUNC_CALL_ARG,AST_FUNC_CALL, AST_BODY,
	AST_FUNC_DEF, AST_FUNC_DEF_ARG, AST_FUNC_RET,
	AST_STRING, AST_TYPE, AST_OP_PLUS, AST_VAR_MUT,
	AST_FUNC_RET_TYPE, AST_FOR_STMT,
	AST_UN_OP, AST_BIN_OP, AST_PROG,
} AST_NodeType;

typedef struct AST_Node AST_Node;
typedef da(AST_Node*) AST_Nodes;

struct AST_Node {
	AST_NodeType type;

	union {
		struct {
			char *id;
			AST_Nodes args;
			Type type;
			AST_Node *body;
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
		} body;
		struct {
			char *id;
			Type type;
			AST_Node *exp;
		} var_def;
		struct {
			char *id;
			Type type;
			AST_Node *exp;
		} var_mut;
		struct {
			AST_Node *exp;
			AST_Node *body;
		} stmt_if;
		struct {
			AST_Node *exp;
			AST_Node *body;
		} stmt_while;
		struct {
			TokenType op;
			Type type;
			AST_Node *v;
		} exp_unary;
		struct {
			TokenType op;
			Type type;
			AST_Node *l, *r;
		} exp_binary;
		struct {
			char *id;
			Type type;
		} func_def_arg;
		struct {
			AST_Node *exp;
			Type type;
		} func_ret;

		AST_Node *func_ret_exp;
		char *var_id;
		int64_t num_int;
		double num_float;
	};
};

typedef enum {
	SBL_FUNC_DEF, SBL_VAR,
} SymbolType;

typedef struct {
	SymbolType type;
	char *id;
	int nested[16];
	union {
		struct {
			AST_Nodes args;
			AST_Node *type;
		} func_def;
		struct {
			Type type;
		} variable;
	};
} Symbol;

typedef da(Symbol) SymbolTable;

typedef struct {
	Token *cur_token;
	SymbolTable st;
	int nested[16], nptr;
	AST_Node *program;
} Parser;

void parser_st_add(Parser *parser, Symbol smbl);
Symbol *parser_st_get(Parser *parser, const char *id);

Symbol *st_get(SymbolTable *st, const char *id);
void parser_alloc();
void parser_parse(Parser *parser, Token *tokens);
void parser_free(Parser parser);
void expect_token(Token *token, TokenType type);
AST_Node *parse_expr(Parser *parser, ExprParsingType type);
AST_Node *parse_func_call(Parser *parser);
AST_Node *ast_alloc(AST_Node node);

#endif
