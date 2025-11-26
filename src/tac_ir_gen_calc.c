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

	switch (l.literal.type.kind) {
		case TYPE_I8:
			*ret = (TAC_Operand){
				.kind = OPR_LITERAL,
				.literal = {
					.kind = LIT_INT,
					.lint = (int8_t)res,
					.type = l.literal.type,
				},
			};
			return true;

		case TYPE_INT:
		case TYPE_I32:
			*ret = (TAC_Operand){
				.kind = OPR_LITERAL,
				.literal = {
					.kind = LIT_INT,
					.lint = (int32_t)res,
					.type = l.literal.type,
				},
			};
			return true;

		case TYPE_I64:
			*ret = (TAC_Operand){
				.kind = OPR_LITERAL,
				.literal = {
					.kind = LIT_INT,
					.lint = (int64_t)res,
					.type = l.literal.type,
				},
			};
			return true;

		default:
			return false;
	}

	return false;
}
