#include <stdio.h>
#include <assert.h>

#include "../include/ir.h"

void dump_opr(Operand opr, char *buf) {
	switch (opr.type) {
		case OPR_VAR:       sprintf(buf, "(%zu)", opr.var.index);   break;
		case OPR_IMM_INT:   sprintf(buf, "%li", opr.imm_int);       break;
		case OPR_IMM_FLOAT: sprintf(buf, "%lf", opr.imm_float);     break;
		case OPR_LABEL:     sprintf(buf, "L%zu", opr.label_index); break;
		default: assert(!"unreachable");
	}
}

void dump_func(Func func) {
	printf("%s:\n", func.name);
	for (size_t i = 0; i < func.body.count; i++) {
		Instruction inst = da_get(&func.body, i);
		char arg1[64], arg2[64], dst[64];

		switch (inst.op) {
			case OP_ADD:
				dump_opr(inst.arg1, arg1);
				dump_opr(inst.arg2, arg2);
				dump_opr(inst.dst, dst);
				printf("    ADD    %-6s %-6s %-6s\n", arg1, arg2, dst);
				break;

			case OP_EQ:
				dump_opr(inst.arg1, arg1);
				dump_opr(inst.arg2, arg2);
				dump_opr(inst.dst, dst);
				printf("    EQ     %-6s %-6s %-6s\n", arg1, arg2, dst);
				break;

			case OP_NOT_EQ:
				dump_opr(inst.arg1, arg1);
				dump_opr(inst.arg2, arg2);
				dump_opr(inst.dst, dst);
				printf("    NEQ    %-6s %-6s %-6s\n", arg1, arg2, dst);
				break;

			case OP_JUMP_IF_NOT:
				dump_opr(inst.arg1, arg1);
				dump_opr(inst.dst, dst);
				printf("    JIN    %-13s %-6s\n", arg1, dst);
				break;

			case OP_JUMP:
				dump_opr(inst.arg1, arg1);
				dump_opr(inst.dst, dst);
				printf("    JMP    %s\n", arg1);
				break;

			case OP_LABEL:
				dump_opr(inst.arg1, arg1);
				printf("    LABEL  %s\n", arg1);
				break;

			case OP_SUB:
				dump_opr(inst.arg1, arg1);
				dump_opr(inst.arg2, arg2);
				dump_opr(inst.dst, dst);
				printf("    SUB    %-6s %-6s %-6s\n", arg1, arg2, dst);
				break;

			case OP_MUL:
				dump_opr(inst.arg1, arg1);
				dump_opr(inst.arg2, arg2);
				dump_opr(inst.dst, dst);
				printf("    MUL    %-6s %-6s %-6s\n", arg1, arg2, dst);
				break;

			case OP_DIV:
				dump_opr(inst.arg1, arg1);
				dump_opr(inst.arg2, arg2);
				dump_opr(inst.dst, dst);
				printf("    DIV    %-6s %-6s %-6s\n", arg1, arg2, dst);
				break;

			case OP_ASSIGN:
				dump_opr(inst.arg1, arg1);
				dump_opr(inst.dst, dst);
				printf("    ASSIGN %-13s %-6s\n", arg1, dst);
				break;

			case OP_RETURN:
				dump_opr(inst.arg1, arg1);
				dump_opr(inst.dst, dst);
				printf("    RETURN %s\n", arg1);
				break;

			default: assert(!"unreachable");
		}
	}
}
