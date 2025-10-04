#ifndef IR_H
#define IR_H

#include <math.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <threads.h>
#include "../include/parser.h"
#include "../thirdparty/da.h"

typedef enum {
	// Binops
	OP_ADD, OP_SUB, OP_MUL, OP_DIV,
	OP_OR, OP_AND, OP_EQ, OP_NOT_EQ,
	OP_LESS, OP_GREAT, OP_LESS_EQ,
	OP_GREAT_EQ,

	OP_LABEL,
	OP_JUMP, OP_JUMP_IF_NOT,
	OP_ASSIGN, OP_RETURN,
	OP_FUNC_CALL,
} OpCode;

typedef enum {
	OPR_NULL,
	OPR_LITERAL,
	OPR_LABEL,
	OPR_NAME,
	OPR_VAR,
} OperandType;

typedef struct {
	OperandType type;

	union {
		Literal literal;
		size_t label_index;
		char *name;
		struct {
			Type type;
			size_t index;
		} var;
	};
} Operand;

typedef struct {
	OpCode op;
	Operand dst;

	union {
		Operand args[8];
		struct {
			Operand arg1;
			Operand arg2;
		};
	};
} Instruction;

typedef struct {
	Type type;
	char *name;
} FuncArg;

typedef struct {
	char *name;
	Type ret_type;
	da(FuncArg) args;
	da(Instruction) body;
} Func;

typedef struct {
	da(Func) funcs;
} Program;

Program ir_gen_prog(AST_Node *pn);

#endif // IR_H
