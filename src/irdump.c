#include <stdint.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "../include/ir.h"

void ir_dump_opr(Operand opr, char *buf) {
	switch (opr.type) {
		case OPR_NULL:      sprintf(buf, "NULL"); break;
		case OPR_FUNC_RET:  sprintf(buf, "fr:%d", opr.func_ret.type.kind); break;
		case OPR_NAME:      sprintf(buf, "\"%s\"", opr.name); break;
		case OPR_VAR:       sprintf(buf, "(%zu):%d", opr.var.index, opr.var.type.kind); break;

		case OPR_LITERAL: {
			switch (opr.literal.type.kind) {
				case TYPE_INT:   sprintf(buf, "%d:%d", (int32_t) opr.literal.lint, opr.literal.type.kind); break;
				case TYPE_I8:
				case TYPE_BOOL:  sprintf(buf, "%d:%d", (int8_t) opr.literal.lint, opr.literal.type.kind); break;
				case TYPE_I64:   sprintf(buf, "%li:%d", opr.literal.lint, opr.literal.type.kind); break;
				case TYPE_FLOAT: sprintf(buf, "%f:%d", (float) opr.literal.lfloat, opr.literal.type.kind); break;
				default: sprintf(buf, "ERR\n"); break;
			}
		} break;
		case OPR_LABEL:     sprintf(buf, ".L%zu", opr.label_index);  break;
	}
}

void ir_dump_inst(Instruction inst, char *res) {
	char arg1[64], arg2[64], dst[64];

	ir_dump_opr(inst.arg1, arg1);
	ir_dump_opr(inst.arg2, arg2);
	ir_dump_opr(inst.dst, dst);

	switch (inst.op) {
		case OP_ADD:    sprintf(res, "    var%s = %s %s %s", dst, arg1, "+", arg2);  break;
		case OP_EQ:     sprintf(res, "    var%s = %s %s %s", dst, arg1, "==", arg2); break;
		case OP_OR:     sprintf(res, "    var%s = %s %s %s", dst, arg1, "||", arg2); break;
		case OP_NOT_EQ: sprintf(res, "    var%s = %s %s %s", dst, arg1, "!=", arg2); break;
		case OP_JUMP_IF_NOT: sprintf(res, "    jmp_if_not %s %s", dst, arg1); break;
		case OP_JUMP:   sprintf(res, "    jmp %s", dst); break;
		case OP_CAST:   sprintf(res, "    var%s = cast %s", dst, arg1); break;
		case OP_LABEL:  sprintf(res, "%s", arg1); break;
		case OP_SUB:    sprintf(res, "    var%s = %s %s %s", dst, arg1, "-", arg2);  break;
		case OP_MUL:    sprintf(res, "    var%s = %s %s %s", dst, arg1, "*", arg2);  break;
		case OP_DIV:    sprintf(res, "    var%s = %s %s %s", dst, arg1, "/", arg2);  break;
		case OP_ASSIGN: sprintf(res, "    var%s = %s", dst, arg1); break;
		case OP_RETURN: sprintf(res, "    return %s", arg1); break;
		case OP_FUNC_CALL: {
			char buf[128];
			ir_dump_opr(inst.dst, dst);
			sprintf(res, "    call %s", dst);

			for (size_t i = 0; inst.args[i].type != OPR_NULL; i++) {
				ir_dump_opr(inst.args[i], arg1);
				sprintf(buf, " %s", arg1);
				strncat(res, buf, 128);
			}
		} break;

		default: unreachable;
	}
}

void ir_dump_func(Func func, FILE *fl) {
	fprintf(fl, "%s:\n", func.name);
	for (size_t i = 0; i < func.body.count; i++) {
		char res[256];
		ir_dump_inst(da_get(&func.body, i), res);
		fprintf(fl, "%s\n", res);
	}

	fprintf(fl, "\n");
}

void ir_dump_prog(Program *prog, char *filename) {
	FILE *fl = fopen(filename, "w");
	for (size_t i = 0; i < prog->funcs.count; i++) {
		ir_dump_func(da_get(&prog->funcs, i), fl);
	}

	fclose(fl);
}
