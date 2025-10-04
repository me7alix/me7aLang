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
	OP_ADD, OP_SUB, OP_NEG, OP_MUL, OP_DIV,
	OP_OR, OP_AND, OP_EQ, OP_NOT_EQ,
	OP_LESS, OP_GREAT, OP_LESS_EQ,
	OP_GREAT_EQ, OP_CAST, OP_NOT,
	OP_REF, OP_DEREF,

	OP_LABEL,
	OP_JUMP, OP_JUMP_IF_NOT,
	OP_ASSIGN, OP_RETURN,
	OP_FUNC_CALL,
} OpCode;

typedef enum {
	OPR_NULL,
	OPR_FUNC_RET,
	OPR_FUNC_INP,
	OPR_LITERAL,
	OPR_LABEL,
	OPR_NAME,
	OPR_VAR,
	OPR_SIZEOF,
} OperandType;

typedef struct {
	OperandType type;

	union {
		Literal literal;
		size_t label_index;
		char *name;
		struct {
			Type type;
			Type v_type;
		} size_of;
		struct {
			Type type;
		} func_ret;
		struct {
			size_t arg_ind;
			Type type;
		} func_inp;
		struct {
			Type type;
			bool is_mem_addr;
			int64_t index;
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
	char *name;
	Type ret_type;
	da(FuncArg) args;
} Extern;

typedef struct {
	da(Extern) externs;
	da(Func) funcs;
} Program;

Type ir_get_opr_type(Operand op);
Program ir_gen_prog(Parser *parser);

#endif // IR_H
