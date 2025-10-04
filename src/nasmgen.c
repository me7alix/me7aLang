#include <assert.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <threads.h>
#include <stdbool.h>
#include <stdarg.h>

#include "../include/ir.h"
#include "../thirdparty/sb.h"

#define TAB "    "

void ir_dump_opr(Operand opr, char *buf);
void ir_dump_inst(Instruction inst, char *res);

static char *argreg8[] = {"dil", "sil", "dl", "cl", "r8b", "r9b"};
static char *argreg16[] = {"di", "si", "dx", "cx", "r8w", "r9w"};
static char *argreg32[] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
static char *argreg64[] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};

da(struct {
	size_t index;
	size_t stack_off;
}) iot = {0};

void iot_add(size_t index, size_t off) {
	if (index >= iot.count) da_resize(&iot, index + 1);
	da_get(&iot, index).index = index;
	da_get(&iot, index).stack_off = off;
}

size_t iot_get(size_t index) {
	if (index >= iot.count) return 0;
	return da_get(&iot, index).stack_off;
}

StringBuilder body = {0};
size_t total_offset = 0;

void type_to_stack(Type type, char *buf) {
	switch (type.kind) {
		case TYPE_I64: sprintf(buf, "qword"); break;
		case TYPE_INT:
		case TYPE_I32: sprintf(buf, "dword"); break;
		case TYPE_I16: sprintf(buf, "word");  break;
		case TYPE_BOOL:
		case TYPE_I8:  sprintf(buf, "byte");  break;
		default: unreachable;
	}
}

char opr_to_nasm_buf[64];
char *opr_to_nasm(Operand opr) {
	switch (opr.type) {
		case OPR_LABEL: {
			sprintf(opr_to_nasm_buf, ".L%zu", opr.label_index);
		} break;

		case OPR_VAR: {
			size_t off = iot_get(opr.var.index); assert(off);
			char ts[32]; type_to_stack(opr.var.type, ts);
			sprintf(opr_to_nasm_buf, "%s [rbp - %zu]", ts, off);
		} break;

		case OPR_LITERAL: {
			switch (opr.literal.type.kind) {
				case TYPE_INT:   sprintf(opr_to_nasm_buf, "%d", (int32_t) opr.literal.lint); break;
				case TYPE_FLOAT: sprintf(opr_to_nasm_buf, "%f", (float) opr.literal.lfloat); break;
				case TYPE_BOOL:
				case TYPE_I8:    sprintf(opr_to_nasm_buf, "%d", (int8_t) opr.literal.lint); break;
				case TYPE_I64:   sprintf(opr_to_nasm_buf, "%li", opr.literal.lint); break;
				default: unreachable;
			}
		} break;

		case OPR_FUNC_RET: {
			switch (opr.func_ret.type.kind) {
				case TYPE_INT:   sprintf(opr_to_nasm_buf, "eax"); break;
				case TYPE_FLOAT: unreachable;
				case TYPE_BOOL:
				case TYPE_I8:    sprintf(opr_to_nasm_buf, "al"); break;
				case TYPE_I64:   sprintf(opr_to_nasm_buf, "rax"); break;
				default: unreachable;
			}
		} break;

		case OPR_FUNC_INP: {
			switch (opr.func_inp.type.kind) {
				case TYPE_INT:   sprintf(opr_to_nasm_buf, "%s", argreg32[opr.func_inp.arg_ind]); break;
				case TYPE_FLOAT: unreachable;
				case TYPE_BOOL:
				case TYPE_I8:    sprintf(opr_to_nasm_buf, "%s", argreg8[opr.func_inp.arg_ind]); break;
				case TYPE_I64:   sprintf(opr_to_nasm_buf, "%s", argreg64[opr.func_inp.arg_ind]); break;
				default: unreachable;
			}
		} break;

		default: unreachable;
	}

	return opr_to_nasm_buf;
}

void type_to_reg(Type type, char *arg1, char *arg2) {
	switch (type.kind) {
		case TYPE_I8: case TYPE_BOOL:
			sprintf(arg1, "al");
			sprintf(arg2, "bl");
			break;

		case TYPE_I64:
			sprintf(arg1, "rax");
			sprintf(arg2, "rcx");
			break;

		case TYPE_INT:
			sprintf(arg1, "eax");
			sprintf(arg2, "ecx");
			break;

		default: unreachable;
	}
}

void reg_alloc(Instruction inst, char *arg1, char *arg2) {
	Type type = inst.dst.var.type;
	if (type.kind == TYPE_BOOL) {
		switch (inst.arg1.type) {
			case OPR_LITERAL:  type = inst.arg1.literal.type;  break;
			case OPR_VAR:      type = inst.arg1.var.type;      break;
			case OPR_FUNC_RET: type = inst.arg1.func_ret.type; break;
			case OPR_FUNC_INP: type = inst.arg1.func_inp.type; break;
			default: unreachable;
		}
	}

	type_to_reg(type, arg1, arg2);
}

void nasm_gen_func(StringBuilder *code, Func func) {
	sb_append_strf(code, "global %s\n", func.name);
	sb_append_strf(code, "%s:\n", func.name);

	sb_append_strf(code, TAB"push rbp\n");
	sb_append_strf(code, TAB"mov rbp, rsp\n");

	sb_reset(&body);

	total_offset = 0;

	for (size_t i = 0; i < func.body.count; i++) {
		char arg1[64], arg2[64], dst[64];
		Instruction ci = da_get(&func.body, i);

		/*{ // log information
			char res[256];
			ir_dump_inst(ci, res);
			sb_append_strf(&body, ";%s\n", res);
		}*/

		switch (ci.op) {
			case OP_ADD: case OP_SUB:
			case OP_MUL: case OP_DIV:
			case OP_EQ: case OP_NOT_EQ:
			case OP_AND: case OP_OR:
			case OP_GREAT: case OP_LESS:
			case OP_GREAT_EQ: case OP_LESS_EQ: {
				char ts[32]; type_to_stack(ci.dst.var.type, ts);
				total_offset += ci.dst.var.type.size;
				iot_add(ci.dst.var.index, total_offset);
				sprintf(dst, "%s [rbp - %zu]", ts, total_offset);
				reg_alloc(ci, arg1, arg2);

				sb_append_strf(&body, TAB"mov %s, %s\n", arg1, opr_to_nasm(ci.arg1));
				sb_append_strf(&body, TAB"mov %s, %s\n", arg2, opr_to_nasm(ci.arg2));

				if      (ci.op == OP_ADD) sb_append_strf(&body, TAB"add %s, %s\n", arg1, arg2);
				else if (ci.op == OP_SUB) sb_append_strf(&body, TAB"sub %s, %s\n", arg1, arg2);
				else if (ci.op == OP_MUL) sb_append_strf(&body, TAB"imul %s, %s\n", arg1, arg2);
				else if (ci.op == OP_DIV) {
					sb_append_strf(&body, TAB"mov edx, 0\n");
					sb_append_strf(&body, TAB"idiv %s\n", arg2);
				}

				else if (ci.op == OP_EQ) {
					sb_append_strf(&body, TAB"cmp %s, %s\n", arg1, arg2);
					sb_append_strf(&body, TAB"sete al\n");
					sprintf(arg1, "al");
				} else if (ci.op == OP_NOT_EQ) {
					sb_append_strf(&body, TAB"cmp %s, %s\n", arg1, arg2);
					sb_append_strf(&body, TAB"setne al\n");
					sprintf(arg1, "al");
				} else if (ci.op == OP_AND) {
					sb_append_strf(&body, TAB"and %s, %s\n", arg1, arg2);
					sprintf(arg1, "al");
				} else if (ci.op == OP_OR) {
					sb_append_strf(&body, TAB"or %s, %s\n", arg1, arg2);
					sprintf(arg1, "al");
				} else if (ci.op == OP_GREAT) {
					sb_append_strf(&body, TAB"cmp %s, %s\n", arg1, arg2);
					sb_append_strf(&body, TAB"setg al\n");
					sprintf(arg1, "al");
				} else if (ci.op == OP_LESS) {
					sb_append_strf(&body, TAB"cmp %s, %s\n", arg1, arg2);
					sb_append_strf(&body, TAB"setl al\n");
					sprintf(arg1, "al");
				} else if (ci.op == OP_GREAT_EQ) {
					sb_append_strf(&body, TAB"cmp %s, %s\n", arg1, arg2);
					sb_append_strf(&body, TAB"setge al\n");
					sprintf(arg1, "al");
				} else if (ci.op == OP_LESS_EQ) {
					sb_append_strf(&body, TAB"cmp %s, %s\n", arg1, arg2);
					sb_append_strf(&body, TAB"setle al\n");
					sprintf(arg1, "al");
				}

				sb_append_strf(&body, TAB"mov %s, %s\n", dst, arg1);
			} break;

			case OP_NOT: case OP_NEG: {
				char ts[32]; type_to_stack(ci.dst.var.type, ts);
				total_offset += ci.dst.var.type.size;
				iot_add(ci.dst.var.index, total_offset);
				sprintf(dst, "%s [rbp - %zu]", ts, total_offset);
				reg_alloc(ci, arg1, arg2);

				sb_append_strf(&body, TAB"mov %s, %s\n", arg1, opr_to_nasm(ci.arg1));

				if      (ci.op == OP_NEG) sb_append_strf(&body, TAB"neg %s\n", arg1);
				else if (ci.op == OP_NOT) {
					sb_append_strf(&body, TAB"test %s, %s\n", arg1, arg1);
					sb_append_strf(&body, TAB"setz al\n");
					sprintf(arg1, "al");
				}

				sb_append_strf(&body, TAB"mov %s, %s\n", dst, arg1);
			} break;

			case OP_CAST: {
				Type dst_type = ci.dst.var.type;
				Type arg1_type;

				switch (ci.arg1.type) {
					case OPR_LITERAL: arg1_type = ci.arg1.literal.type; break;
					case OPR_VAR:     arg1_type = ci.arg1.var.type;     break;
					default: unreachable;
				}

				char ts[32]; type_to_stack(ci.dst.var.type, ts);
				total_offset += ci.dst.var.type.size;
				iot_add(ci.dst.var.index, total_offset);
				sprintf(dst, "%s [rbp - %zu]", ts, total_offset);
				reg_alloc(ci, arg1, arg2);

				if (dst_type.kind == arg1_type.kind) {
					reg_alloc(ci, arg1, arg2);
					sb_append_strf(&body, TAB"mov %s, %s\n", dst, opr_to_nasm(ci.arg1));
				} else if (dst_type.kind == TYPE_I8 && arg1_type.kind == TYPE_INT) {
					sb_append_strf(&body, TAB"mov eax, %s\n", opr_to_nasm(ci.arg1));
					sb_append_strf(&body, TAB"mov al, al\n");
					sb_append_strf(&body, TAB"movsx eax, al\n");
					sb_append_strf(&body, TAB"mov %s, al\n", opr_to_nasm(ci.dst));
				} else if (dst_type.kind == TYPE_INT && arg1_type.kind == TYPE_I8) {
					sb_append_strf(&body, TAB"movsx eax, %s\n", opr_to_nasm(ci.arg1));
					sb_append_strf(&body, TAB"mov %s, eax\n", opr_to_nasm(ci.dst));
				} else {
					assert(!"unreachable");
				}
			} break;

			case OP_ASSIGN: {
				char ts[32]; type_to_stack(ci.dst.var.type, ts);
				size_t off = iot_get(ci.dst.var.index);
				if (!off) {
					total_offset += ci.dst.var.type.size;
					off = total_offset;
					iot_add(ci.dst.var.index, off);
				}

				sprintf(dst, "%s [rbp - %zu]", ts, off);
				reg_alloc(ci, arg1, arg2);

				sb_append_strf(&body, TAB"mov %s, %s\n", arg1, opr_to_nasm(ci.arg1));
				sb_append_strf(&body, TAB"mov %s, %s\n", dst, arg1);
			} break;

			case OP_JUMP_IF_NOT: {
				sb_append_strf(&body, TAB"cmp %s, 0\n", opr_to_nasm(ci.arg1));
				sb_append_strf(&body, TAB"je %s\n", opr_to_nasm(ci.dst));
			} break;

			case OP_LABEL: {
				sb_append_strf(&body, "%s:\n", opr_to_nasm(ci.arg1));
			} break;

			case OP_JUMP: {
				sb_append_strf(&body, TAB"jmp %s\n", opr_to_nasm(ci.dst));
			} break;

			case OP_RETURN: {
				switch (func.ret_type.kind) {
					case TYPE_INT:
						sb_append_strf(&body, TAB"mov eax, %s\n", opr_to_nasm(ci.arg1));
						break;

					default: unreachable;
				}

				sb_append_strf(&body, TAB"mov rsp, rbp\n");
				sb_append_strf(&body, TAB"pop rbp\n");
				sb_append_strf(&body, TAB"ret\n");
			} break;

			case OP_FUNC_CALL: {
				for (size_t i = 0; ci.args[i].type != OPR_NULL; i++) {
					Type type;
					switch (ci.args[i].type) {
						case OPR_LITERAL:  type = ci.args[i].literal.type;  break;
						case OPR_VAR:      type = ci.args[i].var.type;      break;
						case OPR_FUNC_RET: type = ci.args[i].func_ret.type; break;
						case OPR_FUNC_INP: type = ci.args[i].func_inp.type; break;
						default: unreachable;
					}

					switch (type.kind) {
						case TYPE_I64:
							sb_append_strf(&body, TAB"mov %s, %s\n", argreg64[i], opr_to_nasm(ci.args[i]));
							break;

						case TYPE_INT:
							sb_append_strf(&body, TAB"mov %s, %s\n", argreg32[i], opr_to_nasm(ci.args[i]));
							break;

						case TYPE_I8:
							sb_append_strf(&body, TAB"mov %s, %s\n", argreg8[i], opr_to_nasm(ci.args[i]));
							break;

						default: printf("%d\n", type.kind); unreachable;
					}
				}

				sb_append_strf(&body, TAB"call %s\n", ci.dst.name);
			} break;

			default: unreachable;
		}
	}

	total_offset += 8;

	if (total_offset % 16 != 0) {
		total_offset += 16 - total_offset % 16;
	}

	sb_append_strf(code, TAB"sub rsp, %zu\n", total_offset);
	sb_append_strf(code, sb_to_str(body));
	if (da_last(&func.body).op != OP_RETURN) {
		sb_append_strf(code, TAB"mov rsp, rbp\n");
		sb_append_strf(code, TAB"pop rbp\n");
		sb_append_strf(code, TAB"ret\n");
	}

	sb_append_strf(code, "\n");
}

StringBuilder nasm_gen_prog(Program *prog) {
	StringBuilder code = {0};

	for (size_t i = 0; i < prog->externs.count; i++) {
		sb_append_strf(&code, "extern %s\n", da_get(&prog->externs, i).name);
	}
	sb_append_str(&code, "\n");

	sb_append_str(&code, "section .text\n");

	for (size_t i = 0; i < prog->funcs.count; i++) {
		nasm_gen_func(&code, da_get(&prog->funcs, i));
	}

	return code;
}
