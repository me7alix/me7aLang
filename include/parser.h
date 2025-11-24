#ifndef PARSER_H
#define PARSER_H

#include <stdlib.h>
#include <stdbool.h>

#include "type.h"
#include "lexer.h"

typedef enum {
	EXPR_PARSING_VAR, EXPR_PARSING_FUNC_CALL,
	EXPR_PARSING_PAR, EXPR_PARSING_STMT,
	EXPR_PARSING_SQBRA,
} ExprParsingType;

typedef enum {
	LIT_INT,
	LIT_CHAR,
	LIT_FLOAT,
	LIT_BOOL,
	LIT_STR,
} LiteralKind;

typedef struct {
	LiteralKind kind;
	Type type;

	union {
		long int lint;
		double lfloat;
		u8 lbool;
		char *str;
	};
} Literal;

typedef enum {
	// number
	AST_OP_ADD,
	AST_OP_SUB,
	AST_OP_DIV,
	AST_OP_MUL,
	AST_OP_MOD,
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
	AST_OP_ARR,
	AST_OP_VAR_EQ,
	AST_OP_FIELD,
} AST_ExprOp;

typedef enum {
	AST_WHILE_STMT, AST_IF_STMT, AST_BIN_EXP,
	AST_VAR_DEF, AST_VID, AST_LITERAL, AST_TYPE,
	AST_FUNC_CALL_ARG, AST_FUNC_CALL, AST_BODY,
	AST_FUNC_DEF, AST_FUNC_DEF_ARG, AST_FUNC_RET,
	AST_STRING, AST_VAR_MUT, AST_FUNC_RET_TYPE,
	AST_FOR_STMT, AST_UN_OP, AST_BIN_OP, AST_PROG,
	AST_LOOP_BREAK, AST_LOOP_CONTINUE, AST_UN_EXP,
	AST_ELSE_STMT,
} AST_NodeKind;

typedef struct AST_Node AST_Node;
typedef DA(AST_Node*) AST_Nodes;

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
			AST_Node *next;
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
			AST_Node *expr;
			AST_Node *mut;
			AST_Node *body;
		} stmt_for;
		struct {
			AST_ExprOp op;
			Type type;
			AST_Node *v;
		} expr_unary;
		struct {
			AST_ExprOp op;
			Type type;
			AST_Node *l, *r;
		} expr_binary;
		struct {
			char *id;
			Type type;
		} func_def_arg;
		struct {
			AST_Node *expr;
			Type type;
		} func_ret;
		Literal literal;
		Type vtype;
		AST_Node *func_ret_exp;
		char *vid;
	};
};

typedef enum {
	SBL_FUNC_DEF, SBL_VAR,
	SBL_FUNC_EXTERN,
} SymbolType;

typedef struct {
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

typedef struct { SymbolType type; char *id; int *nested; } SymbolKey;
HT_DECL(SymbolTable, SymbolKey, Symbol)

typedef struct {
	Token *cur_token;
	SymbolTable st;
	UserTypes ut;
	int nested[16], nptr;
	AST_Node *program;
} Parser;

void parser_symbol_table_add(Parser *p, SymbolType st, char *id, Symbol smbl);
Symbol *parser_symbol_table_get(Parser *p, SymbolType st, char *id);

Type parse_type(Parser *parser);
Type parser_get_type(Parser *p, AST_Node *n);
bool compare_types(Type a, Type b);

Token *parser_peek(Parser *p);
Token *parser_looknext(Parser *p);
Token *parser_next(Parser *p);

long long parse_int(char *data);
Symbol *st_get(SymbolTable *st, const char *id);
void expect_token(Token *token, TokenKind type);
Parser parser_parse(Token *tokens);
void parser_free(Parser parser);
void expect_token(Token *token, TokenKind type);
AST_Node *parse_expr(Parser *parser, ExprParsingType type, Type *vart);
AST_Node *parse_func_call(Parser *parser);
AST_Node *ast_alloc(AST_Node node);

#define UNREACHABLE do { fprintf(stderr, "%s:%d: unreachable\n", __FILE__, __LINE__); exit(1); } while(0)
#define ast_new(...) ast_alloc((AST_Node) __VA_ARGS__)

#endif
