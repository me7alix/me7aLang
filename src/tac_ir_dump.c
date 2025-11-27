#include <stdint.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "../include/tac_ir.h"

void tac_ir_dump_opr(TAC_Operand opr, char *buf) {
	switch (opr.kind) {
		case OPR_NULL:     sprintf(buf, "NULL"); break;
		case OPR_SIZEOF:   sprintf(buf, "sizeof:%d", opr.size_of.v_type.kind); break;
		case OPR_FUNC_INP: sprintf(buf, "FI(%u):%d", opr.func_inp.arg_id, opr.func_inp.type.kind); break;
		case OPR_FUNC_RET: sprintf(buf, "FR:%d", opr.func_ret.type.kind); break;
		case OPR_NAME:     sprintf(buf, "\"%s\"", opr.name); break;
		case OPR_LABEL:    sprintf(buf, ".L%u", opr.label_id);  break;
		case OPR_FIELD:    sprintf(buf, ">%s", opr.field_id);  break;

		case OPR_VAR: {
			switch (opr.var.kind) {
				case VAR_STACK:   sprintf(buf, "(%u):%d", opr.var.addr_id, opr.var.type.kind); break;
				case VAR_ADDR:    sprintf(buf, "[%u]:%d", opr.var.addr_id, opr.var.type.kind); break;
				case VAR_DATAOFF: sprintf(buf, "{%u}:%d", opr.var.addr_id, opr.var.type.kind); break;
			}
		} break;

		case OPR_LITERAL: {
			switch (opr.literal.type.kind) {
				case TYPE_INT:   sprintf(buf, "%d:%d", (i32) opr.literal.lint, opr.literal.type.kind); break;
				case TYPE_I8:
				case TYPE_BOOL:  sprintf(buf, "%d:%d", (i8) opr.literal.lint, opr.literal.type.kind); break;
				case TYPE_POINTER:
				case TYPE_IPTR:
				case TYPE_I64:   sprintf(buf, "%lli:%d", opr.literal.lint, opr.literal.type.kind); break;
				case TYPE_F32:   sprintf(buf, "%f:%d", (float) opr.literal.lfloat, opr.literal.type.kind); break;
				default: sprintf(buf, "ERR\n"); break;
			}
		} break;
	}
}

void tac_ir_dump_inst(TAC_Instruction inst, char *res) {
	char arg1[64], arg2[64], dst[64];

	tac_ir_dump_opr(inst.arg1, arg1);
	tac_ir_dump_opr(inst.arg2, arg2);
	tac_ir_dump_opr(inst.dst, dst);

	switch (inst.op) {
		case OP_ADD:    sprintf(res, "    var%s = %s %s %s", dst, arg1, "+", arg2);  break;
		case OP_EQ:     sprintf(res, "    var%s = %s %s %s", dst, arg1, "==", arg2); break;
		case OP_NOT:    sprintf(res, "    var%s = !%s", dst, arg1); break;
		case OP_OR:     sprintf(res, "    var%s = %s %s %s", dst, arg1, "||", arg2); break;
		case OP_AND:    sprintf(res, "    var%s = %s %s %s", dst, arg1, "&&", arg2); break;
		case OP_GREAT:  sprintf(res, "    var%s = %s %s %s", dst, arg1, ">", arg2);  break;
		case OP_LESS:   sprintf(res, "    var%s = %s %s %s", dst, arg1, "<", arg2);  break;
		case OP_GREAT_EQ:sprintf(res, "    var%s = %s %s %s", dst, arg1, ">=", arg2);  break;
		case OP_LESS_EQ:sprintf(res, "    var%s = %s %s %s", dst, arg1, "<=", arg2);  break;
		case OP_NOT_EQ: sprintf(res, "    var%s = %s %s %s", dst, arg1, "!=", arg2); break;
		case OP_JUMP_IF_NOT: sprintf(res, "    jmp_if_not %s %s", dst, arg1); break;
		case OP_JUMP:   sprintf(res, "    jmp %s", dst); break;
		case OP_CAST:   sprintf(res, "    var%s = cast %s", dst, arg1); break;
		case OP_NEG:    sprintf(res, "    var%s = -%s", dst, arg1); break;
		case OP_LABEL:  sprintf(res, "%s", arg1); break;
		case OP_SUB:    sprintf(res, "    var%s = %s %s %s", dst, arg1, "-", arg2);  break;
		case OP_MUL:    sprintf(res, "    var%s = %s %s %s", dst, arg1, "*", arg2);  break;
		case OP_DEREF:  sprintf(res, "    var%s = deref %s", dst, arg1);  break;
		case OP_REF:    sprintf(res, "    var%s = ref %s", dst, arg1);  break;
		case OP_DIV:    sprintf(res, "    var%s = %s %s %s", dst, arg1, "/", arg2);  break;
		case OP_MOD:    sprintf(res, "    var%s = %s %s %s", dst, arg1, "%", arg2);  break;
		case OP_ASSIGN: sprintf(res, "    var%s = %s", dst, arg1); break;
		case OP_RETURN: sprintf(res, "    return %s", arg1); break;
		case OP_FADDR:  sprintf(res, "    var%s = faddr %s %s", dst, arg1, arg2); break;
		case OP_FUNC_CALL: {
			char buf[128];
			tac_ir_dump_opr(inst.dst, dst);
			sprintf(res, "    call %s", dst);

			for (size_t i = 0; inst.args[i].kind != OPR_NULL; i++) {
				tac_ir_dump_opr(inst.args[i], arg1);
				sprintf(buf, " %s", arg1);
				strncat(res, buf, 128);
			}
		} break;

		default: UNREACHABLE;
	}
}

void ir_dump_func(TAC_Func func, FILE *fl) {
	fprintf(fl, "%s:\n", func.name);
	for (size_t i = 0; i < func.body.count; i++) {
		char res[256];
		tac_ir_dump_inst(da_get(&func.body, i), res);
		fprintf(fl, "%s\n", res);
	}

	fprintf(fl, "\n");
}

void tac_ir_dump_prog(TAC_Program *prog, char *filename) {
	FILE *fl = fopen(filename, "w");
	for (size_t i = 0; i < prog->funcs.count; i++) {
		ir_dump_func(da_get(&prog->funcs, i), fl);
	}

	fclose(fl);
}
