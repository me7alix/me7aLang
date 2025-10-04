#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <threads.h>
#include <stdbool.h>
#include <stdarg.h>

#include "../include/ir.h"
#include "../thirdparty/sb.h"

#include "irdump.c"

#define TAB "    "

typedef struct {
	size_t index;
	size_t stack_off;
} IndexOff;

da(IndexOff) iot = {0};

void iot_add(size_t index, size_t off) {
	da_append(&iot, ((IndexOff){index, off}));
	//printf("cnt %zu\n", iot.count);
}

size_t iot_get(size_t index) {
	//printf("%zu[");
	for (int i = iot.count - 1; i >= 0; i--) {
		IndexOff cio = da_get(&iot, i);
		if (cio.index == index) {
			return cio.stack_off;
		}
	}

	return 0;
}

StringBuilder body = {0};
size_t total_offset = 0;

void opr_to_nasm(Operand opr, char *buf) {
	switch (opr.type) {
		case OPR_IMM_FLOAT: sprintf(buf, "%lf", opr.imm_float);     break;
		case OPR_IMM_INT:   sprintf(buf, "%li", opr.imm_int);       break;
		case OPR_LABEL:     sprintf(buf, ".L%zu", opr.label_index); break;
		case OPR_VAR: {
			assert(opr.var.type);
			switch (opr.var.type->kind) {
				case TYPE_INT: case TYPE_BOOL: {
					size_t off = iot_get(opr.var.index);
					//if (off == 0) {
					//	total_offset += 4;
					//	off = total_offset;
					//	iot_add(opr.var.index, off);
					//}

					sprintf(buf, "dword [rbp - %zu]", off);
				} break;

				default: assert(!"unreachable");
			}
		} break;
	}
}

void nasm_gen_func(StringBuilder *code, Func func) {
	dump_func(func);
	sb_append_strf(code, "global %s\n", func.name);
	sb_append_strf(code, "%s:\n", func.name);

	sb_append_strf(code, TAB"push rbp\n");
	sb_append_strf(code, TAB"mov rbp, rsp\n");

	sb_reset(&body);

	total_offset = 0;

	for (size_t i = 0; i < func.body.count; i++) {
		Instruction ci = da_get(&func.body, i);
		char arg1[64], arg2[64], dst[64], opr_buf[64];

		switch (ci.op) {
			case OP_ADD: case OP_SUB:
			case OP_MUL: case OP_DIV:
			case OP_EQ: case OP_NOT_EQ: {
				switch (ci.dst.var.type->kind) {
					case TYPE_INT: case TYPE_BOOL:
						total_offset += 4;
						iot_add(ci.dst.var.index, total_offset);
						sprintf(dst, "dword [rbp - %zu]", total_offset);
						sprintf(arg1, "eax");
						sprintf(arg2, "ecx");

						opr_to_nasm(ci.arg1, opr_buf);
						sb_append_strf(&body, TAB"mov %s, %s\n", arg1, opr_buf);
						opr_to_nasm(ci.arg2, opr_buf);
						sb_append_strf(&body, TAB"mov %s, %s\n", arg2, opr_buf);
						break;

					default: assert(!"unreachable");
				}

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
					sb_append_strf(&body, TAB"movzx eax, al\n");
				}

				else if (ci.op == OP_NOT_EQ) {
					sb_append_strf(&body, TAB"cmp %s, %s\n", arg1, arg2);
					sb_append_strf(&body, TAB"setne al\n");
					sb_append_strf(&body, TAB"movzx eax, al\n");
				}

				sb_append_strf(&body, TAB"mov %s, %s\n", dst, arg1);
			} break;

			case OP_ASSIGN: {
				switch (ci.dst.var.type->kind) {
					case TYPE_INT: case TYPE_BOOL: {
						size_t off = iot_get(ci.dst.var.index);
						if (off == 0) { // no value
							total_offset += 4;
							off = total_offset;
							iot_add(ci.dst.var.index, off);
						}

						sprintf(dst, "dword [rbp - %zu]", off);
						sprintf(arg1, "eax");
						opr_to_nasm(ci.arg1, opr_buf);
						sb_append_strf(&body, TAB"mov %s, %s\n", arg1, opr_buf);
					} break;

					default: assert(!"unreachable");
				}

				sb_append_strf(&body, TAB"mov %s, %s\n", dst, arg1);
			} break;

			case OP_JUMP_IF_NOT: {
				opr_to_nasm(ci.arg1, opr_buf);
				sb_append_strf(&body, TAB"cmp %s, 0\n", opr_buf);
				opr_to_nasm(ci.dst, opr_buf);
				sb_append_strf(&body, TAB"je %s\n", opr_buf);
			} break;

			case OP_LABEL: {
				opr_to_nasm(ci.arg1, opr_buf);
				sb_append_strf(&body, "%s:\n", opr_buf);
			} break;

			case OP_JUMP: {
				opr_to_nasm(ci.arg1, opr_buf);
				sb_append_strf(&body, TAB"jmp %s\n", opr_buf);
			} break;

			case OP_RETURN: {
				opr_to_nasm(ci.arg1, opr_buf);
				switch (func.ret_type->kind) {
					case TYPE_INT:
						sb_append_strf(&body, TAB"mov eax, %s\n", opr_buf);
						break;

					default: assert(!"unreachable");
				}

				sb_append_strf(&body, TAB"mov rsp, rbp\n");
				sb_append_strf(&body, TAB"pop rbp\n");
				sb_append_strf(&body, TAB"ret\n\n");
			} break;

			default: assert(!"unreachable");
		}
	}

	sb_append_strf(code, sb_to_str(body));
	sb_append_strf(code, TAB"mov rsp, rbp\n");
	sb_append_strf(code, TAB"pop rbp\n");
	sb_append_strf(code, TAB"ret\n\n");
}

StringBuilder nasm_gen_prog(Program *prog) {
	StringBuilder code = {0};

	sb_append_str(&code, "section .text\n");

	for (size_t i = 0; i < prog->funcs.count; i++) {
		nasm_gen_func(&code, da_get(&prog->funcs, i));
	}

	return code;
}
