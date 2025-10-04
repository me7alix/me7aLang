#include "../include/ir.h"

Operand ir_opr_calc(AST_Node *en, Operand l, Operand r, bool *ret) {
	*ret = true;
	if (l.type == OPR_LITERAL && r.type == OPR_LITERAL && en->kind == AST_BIN_EXP) {
		if (l.literal.type.kind == r.literal.type.kind) {
			switch (l.literal.type.kind) {
				case TYPE_INT: {
					int32_t lv = (int32_t) l.literal.lint;
					int32_t rv = (int32_t) r.literal.lint;
					int32_t res;

					switch (en->exp_binary.op) {
						case AST_OP_ADD: res = lv + rv; break;
						case AST_OP_SUB: res = lv - rv; break;
						case AST_OP_MUL: res = lv * rv; break;
						case AST_OP_DIV: res = lv / rv; break;
						default: *ret = false; return (Operand) {};
					}

					return (Operand) {
						.type = OPR_LITERAL,
						.literal.kind = LIT_INT,
						.literal.lint = res,
						.literal.type = (Type) {
							.kind = TYPE_INT,
							.size = 4,
						},
					};
				} break;

				case TYPE_I64: {
					int64_t lv = l.literal.lint;
					int64_t rv = r.literal.lint;
					int64_t res;

					switch (en->exp_binary.op) {
						case AST_OP_ADD: res = lv + rv; break;
						case AST_OP_SUB: res = lv - rv; break;
						case AST_OP_MUL: res = lv * rv; break;
						case AST_OP_DIV: res = lv / rv; break;
						default: *ret = false; return (Operand) {};
					}

					return (Operand) {
						.type = OPR_LITERAL,
						.literal.kind = LIT_INT,
						.literal.lint = res,
						.literal.type = (Type) {
							.kind = TYPE_I64,
							.size = 8,
						},
					};
				} break;

				case TYPE_I8: {
					int8_t lv = (int8_t) l.literal.lint;
					int8_t rv = (int8_t) r.literal.lint;
					int8_t res;

					switch (en->exp_binary.op) {
						case AST_OP_ADD: res = lv + rv; break;
						case AST_OP_SUB: res = lv - rv; break;
						case AST_OP_MUL: res = lv * rv; break;
						case AST_OP_DIV: res = lv / rv; break;
						default: *ret = false; return (Operand) {};
					}

					return (Operand) {
						.type = OPR_LITERAL,
						.literal.kind = LIT_INT,
						.literal.lint = res,
						.literal.type = (Type) {
							.kind = TYPE_I8,
							.size = 1,
						},
					};
				} break;

				default: unreachable;
			}
		}
	}

	*ret = false;
	return (Operand) {0};
}
