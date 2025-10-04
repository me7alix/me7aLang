#include "../include/ir.h"

Operand ir_opr_calc(AST_Node *en, Operand l, Operand r, bool *ret) {
	*ret = true;

	if (!(l.type == OPR_LITERAL && r.type == OPR_LITERAL && en->kind == AST_BIN_EXP))
		goto fail;
	if (l.literal.type.kind != r.literal.type.kind)
		goto fail;

	int op = en->exp_binary.op;
	int64_t lv = (int64_t) l.literal.lint;
	int64_t rv = (int64_t) r.literal.lint;
	int64_t res;

	switch (op) {
		case AST_OP_ADD: res = lv + rv; break;
		case AST_OP_SUB: res = lv - rv; break;
		case AST_OP_MUL: res = lv * rv; break;
		case AST_OP_DIV:
			if (rv == 0) lexer_error(en->loc, "irgen error: division by zero");
			res = lv / rv; break;
		default: goto fail;
	}

	switch (l.literal.type.kind) {
		case TYPE_I8:
			return (Operand){
				.type = OPR_LITERAL,
				.literal = {
					.kind = LIT_INT,
					.lint = (int8_t)res,
					.type = l.literal.type.kind,
				},
			};
		case TYPE_INT:
		case TYPE_I32:
			return (Operand){
				.type = OPR_LITERAL,
				.literal = {
					.kind = LIT_INT,
					.lint = (int32_t)res,
					.type = l.literal.type.kind,
				},
			};
		case TYPE_I64:
			return (Operand){
				.type = OPR_LITERAL,
				.literal = {
					.kind = LIT_INT,
					.lint = (int64_t)res,
					.type = l.literal.type.kind,
				},
			};
		default:
			unreachable;
	}

fail:
	*ret = false;
	return (Operand){0};
}
