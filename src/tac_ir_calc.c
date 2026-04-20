#include "../include/tac_ir.h"

bool tac_ir_opr_calc(AST_Node *en, TAC_Operand l, TAC_Operand r, TAC_Operand *ret) {
	i64 res;

	if (en->kind == AST_BIN_EXP) {
		if (l.kind != OPR_LITERAL || r.kind != OPR_LITERAL)
			return false;

		if (l.as.literal.type.kind != r.as.literal.type.kind)
			return false;

		int op = en->as.ebin.op;
		i64 lv = l.as.literal.as.lint;
		i64 rv = r.as.literal.as.lint;

		switch (op) {
			case AST_OP_ADD: res = lv + rv; break;
			case AST_OP_SUB: res = lv - rv; break;
			case AST_OP_MUL: res = lv * rv; break;
			case AST_OP_DIV:
				if (rv == 0)
					throw_error(en->loc, "division by zero");
				res = lv / rv;
				break;
			default: return false;
		}
	} else if (en->kind == AST_UN_EXP) {
		if (!(l.kind == OPR_LITERAL))
			return false;

		int op = en->as.ebin.op;
		i64 lv = l.as.literal.as.lint;

		switch (op) {
			case AST_OP_NEG: res = -lv; break;
			default: return false;
		}
	} else return false;

#define CAST_TO_OPR(tk, t) \
	case tk: \
		*ret = (TAC_Operand){ \
			.kind = OPR_LITERAL, \
			.as.literal = { \
				.kind = LIT_INT, \
				.as.lint = (t)res, \
				.type = l.as.literal.type, \
			}, \
		}; \
		return true

	switch (l.as.literal.type.kind) {
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
