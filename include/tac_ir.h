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
} TAC_OpCode;

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
} TAC_OperandKind;

typedef enum {
	VAR_STACK,
	VAR_ADDR,
	VAR_DATAOFF,
} TAC_VarKind;

typedef struct {
	TAC_OperandKind kind;

	union {
		Literal literal;
		uint label_id;
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
			uint arg_id;
			Type type;
		} func_inp;
		struct {
			TAC_VarKind kind;
			Type type;
			TAC_VarKind addr_kind;
			uint addr_id;
			DA(char*) off;
		} var;
	};
} TAC_Operand;

typedef struct {
	TAC_OpCode op;
	TAC_Operand dst;

	union {
		TAC_Operand args[16];
		struct {
			TAC_Operand arg1;
			TAC_Operand arg2;
		};
	};
} TAC_Instruction;

typedef struct {
	Type type;
	char *name;
} TAC_FuncArg;

typedef struct {
	char *name;
	Type ret_type;
	DA(TAC_FuncArg) args;
	DA(TAC_Instruction) body;
} TAC_Func;

typedef struct {
	char *name;
	Type ret_type;
	DA(TAC_FuncArg) args;
} TAC_Extern;

typedef struct {
	Type type;
	uint index;
	u8 *data;
} TAC_GlobalVar;

typedef struct {
	DA(TAC_Extern) externs;
	DA(TAC_GlobalVar) globals;
	DA(TAC_Func) funcs;
} TAC_Program;

Type tac_ir_get_opr_type(TAC_Operand op);
TAC_Program tac_ir_gen_prog(Parser *parser);

#endif // IR_H
