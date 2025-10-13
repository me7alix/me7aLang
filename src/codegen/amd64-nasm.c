#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <threads.h>
#include <stdbool.h>
#include <stdarg.h>

#include "../../include/ir.h"
#include "../../thirdparty/sb.h"

#define TAB "    "

void ir_dump_opr(Operand opr, char *buf);
void ir_dump_inst(Instruction inst, char *res);

static char *argreg8[]  = {"dil", "sil", "dl",  "cl",  "r8b", "r9b"};
static char *argreg16[] = {"di",  "si",  "dx",  "cx",  "r8w", "r9w"};
static char *argreg32[] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
static char *argreg64[] = {"rdi", "rsi", "rdx", "rcx", "r8",  "r9" };

typedef da(struct {
	size_t index;
	size_t off;
}) OffTable;

void OffTable_add(OffTable *t, size_t index, size_t off) {
	if (index >= t->count) da_resize(t, index + 1);
	da_get(t, index).index = index;
	da_get(t, index).off = off;
}

size_t OffTable_get(OffTable *t, size_t index) {
	if (index >= t->count) return 0;
	return da_get(t, index).off;
}

OffTable stack_table = {0};
OffTable data_table  = {0};

size_t get_type_size(Type type) {
	switch (type.kind) {
		case TYPE_ARRAY:
		case TYPE_POINTER:
		case TYPE_IPTR:
		case TYPE_I64:
			return 8;

		case TYPE_INT:
		case TYPE_I32:
			return 4;

		case TYPE_BOOL:
		case TYPE_I8:
			return 1;

		default: unreachable;
	}
}

StringBuilder body = {0};
size_t total_offset = 0;

void type_to_stack(Type type, char *buf) {
	switch (type.kind) {
		case TYPE_ARRAY:
		case TYPE_POINTER:
		case TYPE_IPTR:
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
		case OPR_SIZEOF: {
			size_t size = get_type_size(opr.size_of.v_type);
			if (opr.size_of.v_type.kind == TYPE_ARRAY)
				size = get_type_size(*opr.size_of.v_type.array.elem) * opr.size_of.v_type.array.length;
			sprintf(opr_to_nasm_buf, "%zu", size);
		} break;

		case OPR_LABEL: {
			sprintf(opr_to_nasm_buf, ".L%zu", opr.label_index);
		} break;

		case OPR_VAR: {
			char ts[32]; type_to_stack(opr.var.type, ts);

			if (opr.var.kind == VAR_STACK) {
				size_t off = OffTable_get(&stack_table, opr.var.index); assert(off != -1);
				sprintf(opr_to_nasm_buf, "%s [rbp - %zu]", ts, off);
			} else if (opr.var.kind == VAR_ADDR) {
				if (opr.var.addr_kind == VAR_STACK) {
					size_t off = OffTable_get(&stack_table, opr.var.index); assert(off != -1);
					sb_append_strf(&body, TAB"mov rdx, qword [rbp - %zu]\n", off);
					sprintf(opr_to_nasm_buf, "%s [rdx]", ts);
				} else if (opr.var.addr_kind == VAR_DATAOFF) {
					sb_append_strf(&body, TAB"lea rdx, [D%zu]\n", opr.var.index);
					sprintf(opr_to_nasm_buf, "%s [rdx]", ts);
				} else unreachable;
			} else if (opr.var.kind == VAR_DATAOFF) {
				sprintf(opr_to_nasm_buf, "%s [D%zu]", ts, opr.var.index);
			}
		} break;

		case OPR_LITERAL: {
			switch (opr.literal.type.kind) {
				case TYPE_INT:   sprintf(opr_to_nasm_buf, "%d", (int32_t) opr.literal.lint); break;
				case TYPE_BOOL:
				case TYPE_I8:    sprintf(opr_to_nasm_buf, "%d", (int8_t) opr.literal.lint); break;
				case TYPE_ARRAY:
				case TYPE_POINTER:
				case TYPE_IPTR:
				case TYPE_I64:   sprintf(opr_to_nasm_buf, "%li", opr.literal.lint); break;
				default: unreachable;
			}
		} break;

		case OPR_FUNC_RET: {
			switch (opr.func_ret.type.kind) {
				case TYPE_INT:   sprintf(opr_to_nasm_buf, "eax"); break;
				case TYPE_BOOL:
				case TYPE_I8:    sprintf(opr_to_nasm_buf, "al"); break;
				case TYPE_ARRAY:
				case TYPE_POINTER:
				case TYPE_IPTR:
				case TYPE_I64:   sprintf(opr_to_nasm_buf, "rax"); break;
				default: unreachable;
			}
		} break;

		case OPR_FUNC_INP: {
			switch (opr.func_inp.type.kind) {
				case TYPE_INT:   sprintf(opr_to_nasm_buf, "%s", argreg32[opr.func_inp.arg_ind]); break;
				case TYPE_BOOL:
				case TYPE_I8:    sprintf(opr_to_nasm_buf, "%s", argreg8[opr.func_inp.arg_ind]); break;
				case TYPE_ARRAY:
				case TYPE_POINTER:
				case TYPE_IPTR:
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
		case TYPE_I8:
		case TYPE_BOOL:
			sprintf(arg1, "al");
			sprintf(arg2, "bl");
			break;

		case TYPE_ARRAY:
		case TYPE_POINTER:
		case TYPE_IPTR:
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
	if (inst.dst.type == OPR_LABEL) {
		type_to_reg((Type){.kind = TYPE_BOOL}, arg1, arg2);
		return;
	}

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

void total_offset_add(size_t off) {
	total_offset += off;
	if (total_offset % 8 != 0) {
		total_offset += 8 - total_offset % 8;
	}
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

		/*{ // debug information
			char res[256];
			ir_dump_inst(ci, res);
			printf("%s\n", res);
			sb_append_strf(&body, ";%s\n", res);
		}*/

		switch (ci.op) {
			case OP_ADD: case OP_SUB:
			case OP_MUL: case OP_DIV:
			case OP_EQ: case OP_NOT_EQ:
			case OP_AND: case OP_OR:
			case OP_GREAT: case OP_LESS:
			case OP_GREAT_EQ: case OP_LESS_EQ:
			case OP_MOD: {
				char ts[32]; type_to_stack(ci.dst.var.type, ts);
				total_offset_add(get_type_size(ci.dst.var.type));
				OffTable_add(&stack_table, ci.dst.var.index, total_offset);
				sprintf(dst, "%s [rbp - %zu]", ts, total_offset);
				reg_alloc(ci, arg1, arg2);

				sb_append_strf(&body, TAB"mov %s, %s\n", arg1, opr_to_nasm(ci.arg1));
				sb_append_strf(&body, TAB"mov %s, %s\n", arg2, opr_to_nasm(ci.arg2));

				if      (ci.op == OP_ADD) sb_append_strf(&body, TAB"add %s, %s\n", arg1, arg2);
				else if (ci.op == OP_SUB) sb_append_strf(&body, TAB"sub %s, %s\n", arg1, arg2);
				else if (ci.op == OP_MUL) sb_append_strf(&body, TAB"imul %s, %s\n", arg1, arg2);
				else if (ci.op == OP_DIV || ci.op == OP_MOD) {
					switch (ci.dst.var.type.kind) {
						case TYPE_INT: sb_append_strf(&body, TAB"mov edx, 0\n"); break;
						case TYPE_I64: sb_append_strf(&body, TAB"mov rdx, 0\n"); break;
						default: unreachable;
					}

					sb_append_strf(&body, TAB"idiv %s\n", arg2);
					if (ci.op == OP_MOD) {
						switch (ci.dst.var.type.kind) {
							case TYPE_INT: sprintf(arg1, "edx"); break;
							case TYPE_I64: sprintf(arg1, "rdx"); break;
							default: unreachable;
						}
					}
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
				total_offset_add(get_type_size(ci.dst.var.type));
				OffTable_add(&stack_table, ci.dst.var.index, total_offset);
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
					case OPR_SIZEOF:  arg1_type = ci.arg1.size_of.type; break;
					default: unreachable;
				}

				char ts[32]; type_to_stack(ci.dst.var.type, ts);
				total_offset_add(get_type_size(ci.dst.var.type));
				OffTable_add(&stack_table, ci.dst.var.index, total_offset);
				sprintf(dst, "%s [rbp - %zu]", ts, total_offset);
				reg_alloc(ci, arg1, arg2);

				if (dst_type.kind == arg1_type.kind) { unreachable;
				} else if (dst_type.kind == TYPE_I8 && arg1_type.kind == TYPE_INT) {
					sb_append_strf(&body, TAB"mov eax, %s\n", opr_to_nasm(ci.arg1));
					sb_append_strf(&body, TAB"mov al, al\n");
					sb_append_strf(&body, TAB"movsx eax, al\n");
					sb_append_strf(&body, TAB"mov %s, al\n", opr_to_nasm(ci.dst));
				} else if (dst_type.kind == TYPE_INT && arg1_type.kind == TYPE_I8) {
					sb_append_strf(&body, TAB"movsx eax, %s\n", opr_to_nasm(ci.arg1));
					sb_append_strf(&body, TAB"mov %s, eax\n", opr_to_nasm(ci.dst));
				} else if ((dst_type.kind == TYPE_POINTER || dst_type.kind == TYPE_I64 || dst_type.kind == TYPE_IPTR) && arg1_type.kind == TYPE_INT) {
					sb_append_strf(&body, TAB"mov eax, %s\n", opr_to_nasm(ci.arg1));
					sb_append_strf(&body, TAB"movsxd rax, eax\n");
					sb_append_strf(&body, TAB"mov %s, rax\n", opr_to_nasm(ci.dst));
				} else if (dst_type.kind == TYPE_INT && (dst_type.kind == TYPE_POINTER || arg1_type.kind == TYPE_I64 || arg1_type.kind == TYPE_IPTR)) {
					sb_append_strf(&body, TAB"mov rax, %s\n", opr_to_nasm(ci.arg1));
					sb_append_strf(&body, TAB"mov eax, eax\n");
					sb_append_strf(&body, TAB"movsx rax, eax\n");
					sb_append_strf(&body, TAB"mov %s, eax\n", opr_to_nasm(ci.dst));
				} else unreachable;
			} break;

			case OP_ASSIGN: {
				bool is_first_assign = false;
				if (ci.dst.var.kind != VAR_ADDR) {
					size_t off = OffTable_get(&stack_table, ci.dst.var.index);

					if (off == 0) {
						is_first_assign = true;
						total_offset_add(get_type_size(ci.dst.var.type));
						off = total_offset;
						OffTable_add(&stack_table, ci.dst.var.index, off);
					}
				}

				sprintf(dst, "%s", opr_to_nasm(ci.dst));
				reg_alloc(ci, arg1, arg2);

				if (ci.dst.var.type.kind == TYPE_ARRAY && is_first_assign) {
					total_offset += get_type_size(*ci.dst.var.type.array.elem) * ci.dst.var.type.array.length;
					sb_append_strf(&body, TAB"lea %s, [rbp - %zu]\n", arg1, total_offset);
					sb_append_strf(&body, TAB"mov %s, %s\n", opr_to_nasm(ci.dst), arg1);
				}

				if (ci.arg1.type != OPR_NULL) {
					sb_append_strf(&body, TAB"mov %s, %s\n", arg2, opr_to_nasm(ci.arg1));
					sb_append_strf(&body, TAB"mov %s, %s\n", dst, arg2);
				}
			} break;

			case OP_REF: {
				if (ci.arg1.var.kind == VAR_ADDR) {
					if (ci.arg1.var.addr_kind == VAR_STACK) {
						size_t off = OffTable_get(&stack_table, ci.arg1.var.index);
						sb_append_strf(&body, TAB"mov rax, [rbp - %zu]\n", off);
					} else if (ci.arg1.var.addr_kind == VAR_DATAOFF) {
						sb_append_strf(&body, TAB"lea rax, [D%zu]\n", ci.arg1.var.index);
					}
				} else if (ci.arg1.var.kind == VAR_STACK) {
					size_t off = OffTable_get(&stack_table, ci.arg1.var.index);
					sb_append_strf(&body, TAB"lea rax, [rbp - %zu]\n", off);
				} else if (ci.arg1.var.kind == VAR_DATAOFF) {
					sb_append_strf(&body, TAB"lea rax, [D%zu]\n", ci.arg1.var.index);
				}

				sprintf(dst, "%s", opr_to_nasm(ci.dst));
				sb_append_strf(&body, TAB"mov %s, rax\n", dst);
			} break;

			case OP_JUMP_IF_NOT: {
				reg_alloc(ci, arg1, arg2);
				sb_append_strf(&body, TAB"mov %s, %s\n", arg1, opr_to_nasm(ci.arg1));
				sb_append_strf(&body, TAB"cmp %s, 0\n", arg1);
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
					case TYPE_POINTER:
					case TYPE_I64:
						sb_append_strf(&body, TAB"mov rax, %s\n", opr_to_nasm(ci.arg1));
						break;

					case TYPE_INT:
						sb_append_strf(&body, TAB"mov eax, %s\n", opr_to_nasm(ci.arg1));
						break;

					case TYPE_I8:
					case TYPE_BOOL:
						sb_append_strf(&body, TAB"mov al, %s\n", opr_to_nasm(ci.arg1));
						break;

					default: unreachable;
				}

				sb_append_strf(&body, TAB"mov rsp, rbp\n");
				sb_append_strf(&body, TAB"pop rbp\n");
				sb_append_strf(&body, TAB"ret\n");
			} break;

			case OP_FUNC_CALL: {
				for (size_t i = 0; ci.args[i].type != OPR_NULL; i++) {
					switch (ir_get_opr_type(&ci.args[i])->kind) {
						case TYPE_ARRAY:
						case TYPE_POINTER:
						case TYPE_IPTR:
						case TYPE_I64:
							sb_append_strf(&body, TAB"mov %s, %s\n", argreg64[i], opr_to_nasm(ci.args[i]));
							break;

						case TYPE_INT:
							sb_append_strf(&body, TAB"mov %s, %s\n", argreg32[i], opr_to_nasm(ci.args[i]));
							break;

						case TYPE_I8:
							sb_append_strf(&body, TAB"mov %s, %s\n", argreg8[i], opr_to_nasm(ci.args[i]));
							break;

						default: unreachable;
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

const char *nasm_gen_prog(Program *prog) {
	StringBuilder code = {0};

	da_foreach(Extern, ext, &prog->externs) {
		sb_append_strf(&code, "extern %s\n", ext->name);
	}
	sb_append_str(&code, "\n");

	sb_append_str(&code, "section .data\n");
	size_t uniq_data_off = 0;
	da_foreach(GlobalVar, g, &prog->globals) {
		if (g->type.kind == TYPE_ARRAY) {
			size_t s = get_type_size(*g->type.array.elem) * g->type.array.length;
			sb_append_strf(&code, TAB"U%zu times %zu db 0\n", uniq_data_off, s);
			sb_append_strf(&code, TAB"D%zu dq U%zu\n", g->index, uniq_data_off++);
		} else {
			sb_append_strf(&code, TAB"D%zu times %li db 0\n", g->index, get_type_size(g->type));
		}
	}
	sb_append_str(&code, "\n");

	sb_append_str(&code, "section .text\n");
	for (size_t i = 0; i < prog->funcs.count; i++) {
		nasm_gen_func(&code, da_get(&prog->funcs, i));
	}

	return sb_to_str(code);
}
