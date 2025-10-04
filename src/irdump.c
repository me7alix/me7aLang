#include <stdio.h>
#include <assert.h>

#include "../include/ir.h"

void ir_dump_opr(Operand opr, char *buf) {
	switch (opr.type) {
		case OPR_NULL:      sprintf(buf, "NO");                     break;
		case OPR_NAME:      sprintf(buf, "\"%s\"", opr.name);       break;
		case OPR_VAR:       sprintf(buf, "(%zu)", opr.var.index);   break;
		case OPR_IMM_INT:   sprintf(buf, "%li", opr.imm_int);       break;
		case OPR_IMM_FLOAT: sprintf(buf, "%lf", opr.imm_float);     break;
		case OPR_LABEL:     sprintf(buf, "L%zu", opr.label_index);  break;
	}
}

void ir_dump_func(Func func, FILE *fl) {
	fprintf(fl, "%s:\n", func.name);
	for (size_t i = 0; i < func.body.count; i++) {
		Instruction inst = da_get(&func.body, i);
		char arg1[64], arg2[64], dst[64];

		switch (inst.op) {
			case OP_ADD:
				ir_dump_opr(inst.arg1, arg1);
				ir_dump_opr(inst.arg2, arg2);
				ir_dump_opr(inst.dst, dst);
				fprintf(fl, "    ADD    %-6s %-6s %-6s\n", arg1, arg2, dst);
				break;

			case OP_EQ:
				ir_dump_opr(inst.arg1, arg1);
				ir_dump_opr(inst.arg2, arg2);
				ir_dump_opr(inst.dst, dst);
				fprintf(fl, "    EQ     %-6s %-6s %-6s\n", arg1, arg2, dst);
				break;

			case OP_OR:
				ir_dump_opr(inst.arg1, arg1);
				ir_dump_opr(inst.arg2, arg2);
				ir_dump_opr(inst.dst, dst);
				fprintf(fl, "    OR     %-6s %-6s %-6s\n", arg1, arg2, dst);
				break;

			case OP_NOT_EQ:
				ir_dump_opr(inst.arg1, arg1);
				ir_dump_opr(inst.arg2, arg2);
				ir_dump_opr(inst.dst, dst);
				fprintf(fl, "    NEQ    %-6s %-6s %-6s\n", arg1, arg2, dst);
				break;

			case OP_JUMP_IF_NOT:
				ir_dump_opr(inst.arg1, arg1);
				ir_dump_opr(inst.dst, dst);
				fprintf(fl, "    JIN    %-13s %-6s\n", arg1, dst);
				break;

			case OP_JUMP:
				ir_dump_opr(inst.arg1, arg1);
				ir_dump_opr(inst.dst, dst);
				fprintf(fl, "    JMP    %s\n", arg1);
				break;

			case OP_LABEL:
				ir_dump_opr(inst.arg1, arg1);
				fprintf(fl, "    LABEL  %s\n", arg1);
				break;

			case OP_SUB:
				ir_dump_opr(inst.arg1, arg1);
				ir_dump_opr(inst.arg2, arg2);
				ir_dump_opr(inst.dst, dst);
				fprintf(fl, "    SUB    %-6s %-6s %-6s\n", arg1, arg2, dst);
				break;

			case OP_MUL:
				ir_dump_opr(inst.arg1, arg1);
				ir_dump_opr(inst.arg2, arg2);
				ir_dump_opr(inst.dst, dst);
				fprintf(fl, "    MUL    %-6s %-6s %-6s\n", arg1, arg2, dst);
				break;

			case OP_DIV:
				ir_dump_opr(inst.arg1, arg1);
				ir_dump_opr(inst.arg2, arg2);
				ir_dump_opr(inst.dst, dst);
				fprintf(fl, "    DIV    %-6s %-6s %-6s\n", arg1, arg2, dst);
				break;

			case OP_ASSIGN:
				ir_dump_opr(inst.arg1, arg1);
				ir_dump_opr(inst.dst, dst);
				fprintf(fl, "    ASSIGN %-13s %-6s\n", arg1, dst);
				break;

			case OP_RETURN:
				ir_dump_opr(inst.arg1, arg1);
				ir_dump_opr(inst.dst, dst);
				fprintf(fl, "    RETURN %s\n", arg1);
				break;

			case OP_FUNC_CALL:
				ir_dump_opr(inst.dst, dst);
				fprintf(fl, "    CALL   %s", dst);

				for (size_t i = 0; i < inst.args[i].type != OPR_NULL; i++) {
					ir_dump_opr(inst.args[i], arg1);
					fprintf(fl, " %s", arg1);
				}

				fprintf(fl, "\n");
				break;

			default: assert(!"unreachable");
		}
	}
}

void ir_dump_prog(Program *prog, char *filename) {
	FILE *fl = fopen(filename, "w");
	for (size_t i = 0; i < prog->funcs.count; i++) {
		ir_dump_func(da_get(&prog->funcs, i), fl);
	}
	
	fclose(fl);
}
