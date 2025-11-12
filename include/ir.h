#ifndef IR_H
#define IR_H

#include <math.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include "../include/parser.h"

typedef enum {
	// Binops
	OP_ADD, OP_SUB, OP_NEG, OP_MUL, OP_DIV,
	OP_MOD, OP_OR, OP_AND, OP_EQ, OP_NOT_EQ,
	OP_LESS, OP_GREAT, OP_LESS_EQ,
	OP_GREAT_EQ, OP_CAST, OP_NOT,
	OP_REF, OP_DEREF, OP_FADDR,

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
	OPR_FIELD,
} OperandType;

typedef enum {
	VAR_STACK,
	VAR_ADDR,
	VAR_DATAOFF,
} VarKind;

typedef struct {
	OperandType type;

	union {
		Literal literal;
		u64 label_id;
		char *field_id;
		char *name;
		struct {
			Type type;
			Type v_type;
		} size_of;
		struct {
			Type type;
		} func_ret;
		struct {
			u64 arg_id;
			Type type;
		} func_inp;
		struct {
			VarKind kind;
			Type type;
			VarKind addr_kind;
			u64 addr_id;
			DA(char*) off;
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
	DA(FuncArg) args;
	DA(Instruction) body;
} Func;

typedef struct {
	char *name;
	Type ret_type;
	DA(FuncArg) args;
} Extern;

typedef struct {
	Type type;
	u64 index;
	u8 *data;
} GlobalVar;

typedef struct {
	DA(Extern) externs;
	DA(GlobalVar) globals;
	DA(Func) funcs;
} Program;

Type ir_get_opr_type(Operand op);
Program ir_gen_prog(Parser *parser);

#endif // IR_H
