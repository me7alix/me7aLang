#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <threads.h>
#include <stdbool.h>
#include <stdarg.h>

#include "../include/ir.h"
#include "../thirdparty/sb.h"

#define TAB "    "

static char *argreg8[] = {"dil", "sil", "dl", "cl", "r8b", "r9b"};
static char *argreg16[] = {"di", "si", "dx", "cx", "r8w", "r9w"};
static char *argreg32[] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
static char *argreg64[] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};

da(struct {
	size_t index;
	size_t stack_off;
}) iot = {0};

void iot_add(size_t index, size_t off) {
	da_resize(&iot, index + 1);
	da_get(&iot, index).index = index;
	da_get(&iot, index).stack_off = off;
}

size_t iot_get(size_t index) {
	if (index >= iot.count) return 0;
	return da_get(&iot, index).stack_off;
}

StringBuilder body = {0};
size_t total_offset = 0;

char opr_to_nasm_buf[64];

char *opr_to_nasm(Operand opr) {
	switch (opr.type) {
		case OPR_NULL: case OPR_NAME: assert(!"unreachable"); 
		case OPR_LABEL:     sprintf(opr_to_nasm_buf, ".L%zu", opr.label_index); break;
		case OPR_VAR: {
			size_t off = iot_get(opr.var.index);
			switch (opr.var.type.kind) {
				case TYPE_INT:  sprintf(opr_to_nasm_buf, "dword [rbp - %zu]", off); break;
				case TYPE_BOOL: sprintf(opr_to_nasm_buf, "byte [rbp - %zu]", off); break;
				case TYPE_I8: sprintf(opr_to_nasm_buf, "byte [rbp - %zu]", off); break;
				default: assert(!"unreachable");
			}
		} break;
		case OPR_LITERAL: {
			switch (opr.literal.type.kind) {
				case TYPE_INT:   sprintf(opr_to_nasm_buf, "%d", (int32_t) opr.literal.lint); break;
				case TYPE_FLOAT: sprintf(opr_to_nasm_buf, "%f", (float) opr.literal.lfloat); break;
				case TYPE_I8:    sprintf(opr_to_nasm_buf, "%d", (int8_t) opr.literal.lint); break;
				default: assert(!"unreachable");
			}
		} break;
	}

	return opr_to_nasm_buf;
}

void reg_alloc(Instruction inst, char *arg1, char *arg2) {
	switch (inst.dst.var.type.kind) {
		case TYPE_I8:
			sprintf(arg1, "al");
			sprintf(arg2, "bl");
			break;

		case TYPE_INT:
			sprintf(arg1, "eax");
			sprintf(arg2, "ecx");
			break;

		case TYPE_BOOL: {
			switch (inst.arg1.type) {
				case OPR_VAR:
					switch (inst.arg1.var.type.kind) {
						case TYPE_BOOL: case TYPE_I8:
							sprintf(arg1, "al");
							sprintf(arg2, "bl");
							break;

						case TYPE_INT:
							sprintf(arg1, "eax");
							sprintf(arg2, "ecx");
							break;
						default: assert(!"unreachable");
					}

					break;

				case OPR_LITERAL: {
					switch (inst.arg1.literal.type.kind) {
						case TYPE_INT:
							sprintf(arg1, "eax");
							sprintf(arg2, "ecx");
							break;
						default: assert(!"unreachable");
					}
				} break;

				default: assert(!"unreachable");
			}
		} break;

		default: assert(!"unreachable");
	}
}

void nasm_gen_func(StringBuilder *code, Func func) {
	sb_append_strf(code, "global %s\n", func.name);
	sb_append_strf(code, "%s:\n", func.name);

	sb_append_strf(code, TAB"push rbp\n");
	sb_append_strf(code, TAB"mov rbp, rsp\n");

	sb_reset(&body);

	total_offset = 8;

	for (size_t i = 0; i < func.body.count; i++) {
		Instruction ci = da_get(&func.body, i);
		char arg1[64], arg2[64], dst[64];

		switch (ci.op) {
			case OP_ADD: case OP_SUB:
			case OP_MUL: case OP_DIV:
			case OP_EQ: case OP_NOT_EQ:
			case OP_AND: {
				switch (ci.dst.var.type.kind) {
					case TYPE_BOOL: case TYPE_I8:
						total_offset += 1;
						iot_add(ci.dst.var.index, total_offset);
						sprintf(dst, "byte [rbp - %zu]", total_offset);
						reg_alloc(ci, arg1, arg2);
						break;

					case TYPE_INT:
						total_offset += 4;
						iot_add(ci.dst.var.index, total_offset);
						sprintf(dst, "dword [rbp - %zu]", total_offset);
						reg_alloc(ci, arg1, arg2);
						break;

					default: assert(!"unreachable");
				}

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
				}

				else if (ci.op == OP_NOT_EQ) {
					sb_append_strf(&body, TAB"cmp %s, %s\n", arg1, arg2);
					sb_append_strf(&body, TAB"setne al\n");
					sprintf(arg1, "al");
				}

				else if (ci.op == OP_AND) {
					sb_append_strf(&body, TAB"and %s, %s\n", arg1, arg2);
					sprintf(arg1, "al");
				}

				sb_append_strf(&body, TAB"mov %s, %s\n", dst, arg1);
			} break;

			case OP_ASSIGN: {
				switch (ci.dst.var.type.kind) {
					case TYPE_BOOL: case TYPE_I8: {
						size_t off = iot_get(ci.dst.var.index);
						if (off == 0) { // no value
							total_offset += 1;
							off = total_offset;
							iot_add(ci.dst.var.index, off);
						}

						sprintf(arg1, "al");
						sprintf(dst, "byte [rbp - %zu]", off);
						sb_append_strf(&body, TAB"mov %s, %s\n", arg1, opr_to_nasm(ci.arg1));
					} break;

					case TYPE_INT: {
						size_t off = iot_get(ci.dst.var.index);
						if (off == 0) { // no value
							total_offset += 4;
							off = total_offset;
							iot_add(ci.dst.var.index, off);
						}

						sprintf(arg1, "eax");
						sprintf(dst, "dword [rbp - %zu]", off);
						sb_append_strf(&body, TAB"mov %s, %s\n", arg1, opr_to_nasm(ci.arg1));
					} break;

					default: assert(!"unreachable");
				}

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
				sb_append_strf(&body, TAB"jmp %s\n", opr_to_nasm(ci.arg1));
			} break;

			case OP_RETURN: {
				switch (func.ret_type.kind) {
					case TYPE_INT:
						sb_append_strf(&body, TAB"mov eax, %s\n", opr_to_nasm(ci.arg1));
						break;

					default: assert(!"unreachable");
				}

				sb_append_strf(&body, TAB"mov rsp, rbp\n");
				sb_append_strf(&body, TAB"pop rbp\n");
				sb_append_strf(&body, TAB"ret\n");
			} break;

			case OP_FUNC_CALL: {
				for (size_t i = 0; i < ci.args[i].type != OPR_NULL; i++) {
					switch (ci.args[i].type) {
						case OPR_LITERAL: {
							switch (ci.args[i].literal.type.kind) {
								case TYPE_INT:
									sb_append_strf(&body, TAB"mov %s, %s\n", argreg32[i], opr_to_nasm(ci.args[i]));
									break;
								case TYPE_I8:
									sb_append_strf(&body, TAB"mov %s, %s\n", argreg8[i], opr_to_nasm(ci.args[i]));
									break;
								default: assert(!"unreachable");
							}
						} break;

						case OPR_VAR: {
							switch (ci.args[i].var.type.kind) {
								case TYPE_INT:
									sb_append_strf(&body, TAB"mov %s, %s\n", argreg32[i], opr_to_nasm(ci.args[i]));
									break;
								case TYPE_I8:
									sb_append_strf(&body, TAB"mov %s, %s\n", argreg8[i], opr_to_nasm(ci.args[i]));
									break;

								default: assert(!"unreachable");
							}
						} break;

						default: assert(!"unreachable");
					}
				}

				sb_append_strf(&body, TAB"call %s\n", ci.dst.name);
			} break;

			default: assert(!"unreachable");
		}
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

	// runtime
	sb_append_str(&code, "extern print_char\n");
	sb_append_str(&code, "extern print_int\n\n");

	sb_append_str(&code, "section .text\n");

	for (size_t i = 0; i < prog->funcs.count; i++) {
		nasm_gen_func(&code, da_get(&prog->funcs, i));
	}

	return code;
}
