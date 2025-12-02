#include "../include/tac_ir.h"

bool tac_ir_opr_calc(AST_Node *en, TAC_Operand l, TAC_Operand r, TAC_Operand *ret) {
	i64 res;

	if (en->kind == AST_BIN_EXP) {
		if (!(l.kind == OPR_LITERAL && r.kind == OPR_LITERAL))
			return false;

		if (l.literal.type.kind != r.literal.type.kind)
			return false;

		int op = en->expr_binary.op;
		i64 lv = l.literal.lint;
		i64 rv = r.literal.lint;

		switch (op) {
			case AST_OP_ADD: res = lv + rv; break;
			case AST_OP_SUB: res = lv - rv; break;
			case AST_OP_MUL: res = lv * rv; break;
			case AST_OP_DIV:
				if (rv == 0)
					lexer_error(en->loc, "error: division by zero");
				res = lv / rv;
				break;
			default: return false;
		}
	} else if (en->kind == AST_BIN_EXP) {
		if (!(l.kind == OPR_LITERAL))
			return false;

		int op = en->expr_binary.op;
		i64 lv = l.literal.lint;

		switch (op) {
			case AST_OP_NEG: res = -lv; break;
			default: return false;
		}
	} else return false;

#define CAST_TO_OPR(tk, t) \
	case tk: \
		*ret = (TAC_Operand){ \
			.kind = OPR_LITERAL, \
			.literal = { \
				.kind = LIT_INT, \
				.lint = (t)res, \
				.type = l.literal.type, \
			}, \
		}; \
		return true

	switch (l.literal.type.kind) {
		CAST_TO_OPR(TYPE_INT, i32);
		CAST_TO_OPR(TYPE_I8,  i8);
		CAST_TO_OPR(TYPE_I16, i16);
		CAST_TO_OPR(TYPE_I32, i32);
		CAST_TO_OPR(TYPE_I64, i32);
		CAST_TO_OPR(TYPE_U8,  u8);
		CAST_TO_OPR(TYPE_U16, u16);
		CAST_TO_OPR(TYPE_U32, u32);
		CAST_TO_OPR(TYPE_U64, u32);
		default: return false;
	}

	return false;
}
