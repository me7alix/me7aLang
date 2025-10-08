#include "../include/ir.h"

Operand ir_opr_calc(AST_Node *en, Operand l, Operand r, bool *ret) {
	*ret = true;
	int64_t res;

	if (en->kind == AST_BIN_EXP) {
		if (!(l.type == OPR_LITERAL && r.type == OPR_LITERAL))
			goto fail;
		if (l.literal.type.kind != r.literal.type.kind)
			goto fail;

		int op = en->exp_binary.op;
		int64_t lv = l.literal.lint;
		int64_t rv = r.literal.lint;

		switch (op) {
			case AST_OP_ADD: res = lv + rv; break;
			case AST_OP_SUB: res = lv - rv; break;
			case AST_OP_MUL: res = lv * rv; break;
			case AST_OP_DIV:
				if (rv == 0) lexer_error(en->loc, "error: division by zero");
				res = lv / rv; break;
			default: goto fail;
		}
	} else if (en->kind == AST_BIN_EXP) {
		if (!(l.type == OPR_LITERAL))
			goto fail;

		int op = en->exp_binary.op;
		int64_t lv = l.literal.lint;

		switch (op) {
			case AST_OP_NEG: res = -lv; break;
			default: goto fail;
		}
	} else goto fail;

	switch (l.literal.type.kind) {
		case TYPE_I8:
			return (Operand){
				.type = OPR_LITERAL,
				.literal = {
					.kind = LIT_INT,
					.lint = (int8_t)res,
					.type = l.literal.type,
				},
			};
		case TYPE_INT:
		case TYPE_I32:
			return (Operand){
				.type = OPR_LITERAL,
				.literal = {
					.kind = LIT_INT,
					.lint = (int32_t)res,
					.type = l.literal.type,
				},
			};
		case TYPE_I64:
			return (Operand){
				.type = OPR_LITERAL,
				.literal = {
					.kind = LIT_INT,
					.lint = (int64_t)res,
					.type = l.literal.type,
				},
			};
		default: goto fail;
	}

fail:
	*ret = false;
	return (Operand){0};
}
