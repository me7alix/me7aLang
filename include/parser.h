#ifndef PARSER_H
#define PARSER_H

#include <stdlib.h>
#include <stdint.h>
#include <threads.h>
#include <stdbool.h>
#include <assert.h>

#include "../thirdparty/da.h"
#include "../include/lexer.h"

typedef enum {
	TYPE_NULL,
	TYPE_INT,  TYPE_UINT,
	TYPE_I8,   TYPE_U8,
	TYPE_I16,  TYPE_U16,
	TYPE_I32,  TYPE_U32,
	TYPE_I64,  TYPE_U64,
	TYPE_IPTR, TYPE_UPTR,

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
		struct { Type **params; size_t param_count; Type* ret; } function;
		struct { char *name; } user;
	};
};

typedef enum {
	EXPR_PARSING_VAR, EXPR_PARSING_FUNC_CALL,
	EXPR_PARSING_PAR, EXPR_PARSING_STMT,
} ExprParsingType;

typedef enum {
	LIT_INT,
	LIT_CHAR,
	LIT_FLOAT,
	LIT_BOOL,
} LiteralKind;

typedef struct {
	LiteralKind kind;
	Type type;

	union {
		int64_t lint;
		double lfloat;
		uint8_t lbool;
	};
} Literal;

typedef enum {
	// number
	AST_OP_ADD,
	AST_OP_SUB,
	AST_OP_DIV,
	AST_OP_MUL,
	AST_OP_NEG,
	AST_OP_ADD_EQ,
	AST_OP_SUB_EQ,
	AST_OP_DIV_EQ,
	AST_OP_MUL_EQ,

	// boolean
	AST_OP_EQ,
	AST_OP_NOT_EQ,
	AST_OP_LESS,
	AST_OP_GREAT,
	AST_OP_LESS_EQ,
	AST_OP_GREAT_EQ,
	AST_OP_AND,
	AST_OP_OR,
	AST_OP_NOT,

	// other
	AST_OP_SIZEOF,
	AST_OP_CAST,
	AST_OP_REF,
	AST_OP_DEREF,
	AST_OP_VAR_EQ,
} AST_ExprOp;

typedef enum {
	AST_WHILE_STMT, AST_IF_STMT, AST_BIN_EXP,
	AST_VAR_DEF, AST_VAR, AST_LITERAL, AST_TYPE,
	AST_FUNC_CALL_ARG, AST_FUNC_CALL, AST_BODY,
	AST_FUNC_DEF, AST_FUNC_DEF_ARG, AST_FUNC_RET,
	AST_STRING, AST_VAR_MUT, AST_FUNC_RET_TYPE,
	AST_FOR_STMT, AST_UN_OP, AST_BIN_OP, AST_PROG,
	AST_LOOP_BREAK, AST_LOOP_CONTINUE, AST_UN_EXP,
	AST_ELSE_STMT,
} AST_NodeKind;

typedef struct AST_Node AST_Node;
typedef da(AST_Node*) AST_Nodes;

struct AST_Node {
	AST_NodeKind kind;
	Location loc;

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
			Type type;
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
			AST_Node *next; // chain of if, else if and else
		} stmt_if;
		struct {
			AST_Node *body;
		} stmt_else;
		struct {
			AST_Node *exp;
			AST_Node *body;
		} stmt_while;
		struct {
			AST_Node *var;
			AST_Node *exp;
			AST_Node *mut;
			AST_Node *body;
		} stmt_for;
		struct {
			AST_ExprOp op;
			Type type;
			AST_Node *v;
		} exp_unary;
		struct {
			AST_ExprOp op;
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
		Literal literal;
		Type vtype;
		AST_Node *func_ret_exp;
		char *var_id;
	};
};

typedef enum {
	SBL_FUNC_DEF, SBL_VAR,
	SBL_FUNC_EXTERN,
} SymbolType;

typedef struct {
	SymbolType type;
	char *id;
	int nested[16];

	union {
		struct {
			AST_Nodes args;
			Type type;
			bool is_def;
		} func_def;
		struct {
			AST_Nodes args;
			Type type;
			char *extern_smb;
		} func_extern;
		struct {
			Type type;
		} variable;
	};
} Symbol;

typedef da(Symbol) SymbolTable;

typedef struct {
	Token *cur_token;
	SymbolTable symbols;
	int nested[16], nptr;
	AST_Node *program;
} Parser;

Type parse_type(Parser *parser);
void parser_symbols_add(Parser *parser, Symbol smbl);
Symbol *parser_symbols_get(Parser *p, const char *id);
Type parser_get_type(Parser *p, AST_Node *n);
bool compare_types(Type a, Type b);

Token *parser_peek(Parser *p);
Token *parser_looknext(Parser *p);
Token *parser_next(Parser *p);

int64_t parse_int(char *data);
Symbol *st_get(SymbolTable *st, const char *id);
void expect_token(Token *token, TokenType type);
Parser parser_parse(Token *tokens);
void parser_free(Parser parser);
void expect_token(Token *token, TokenType type);
AST_Node *parse_expr(Parser *parser, ExprParsingType type, Type *vart);
AST_Node *parse_func_call(Parser *parser);
AST_Node *ast_alloc(AST_Node node);

#define is_pointer(t) ((t).kind == TYPE_ARRAY || (t).kind == TYPE_POINTER)
#define get_pointer_base(t) ((t).kind == TYPE_POINTER ? (t).pointer.base : (t).array.elem)
#define unreachable assert(!"unreachable")
#define ast_new(...) ast_alloc((AST_Node) __VA_ARGS__ )

#endif
