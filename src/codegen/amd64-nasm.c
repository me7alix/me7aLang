#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <threads.h>
#include <stdbool.h>
#include <stdarg.h>
#include <assert.h>

#include "../../include/tac_ir.h"

#define TAB "    "

void ir_dump_opr(TAC_Operand opr, char *buf);
void ir_dump_inst(TAC_Instruction inst, char *res);

static char *argreg8[]  = {"dil", "sil", "dl",  "cl",  "r8b", "r9b"};
static char *argreg16[] = {"di",  "si",  "dx",  "cx",  "r8w", "r9w"};
static char *argreg32[] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
static char *argreg64[] = {"rdi", "rsi", "rdx", "rcx", "r8",  "r9" };

HT_DECL(OffTable, uint, uint)
HT_IMPL_NUM(OffTable, uint, uint)

OffTable stack_table = {0};
OffTable data_table  = {0};

uint get_type_size(Type type) {
	switch (type.kind) {
		case TYPE_STRUCT: {
			size_t total = 0;
			da_foreach (Field, field, &type.user->ustruct.fields)
				total += get_type_size(field->type);
			return total;
		} break;
		case TYPE_ARRAY:
		case TYPE_POINTER:
		case TYPE_UPTR:
		case TYPE_IPTR:
		case TYPE_I64:
			return 8;
		case TYPE_FLOAT:
		case TYPE_UINT:
		case TYPE_INT:
		case TYPE_I32:
			return 4;
		case TYPE_U16:
		case TYPE_I16:
			return 2;
		case TYPE_BOOL:
		case TYPE_I8:
			return 1;
		default: unreachable;
	}
}

uint get_struct_alignment(TAC_Operand var, bool is_data) {
	Type vt = var.var.type, lt;
	uint total = 0;

	for (size_t i = 0; i < var.var.off.count; i++) {
		char *off = da_get(&var.var.off, i);
		da_foreach (Field, field, &var.var.type.user->ustruct.fields) {
			total += get_type_size(field->type);
			if (strcmp(field->id, off) == 0) {
				var.var.type = field->type;
				lt = field->type;
				total -= get_type_size(field->type);
				break;
			}
		}
	}

	total += get_type_size(lt);
	if (is_data) return total;
	return get_type_size(vt) - total;
}

StringBuilder body = {0};
uint total_offset = 0;

void var_type_to_stack(TAC_Operand t, char *buf) {
	switch (tac_ir_get_opr_type(t).kind) {
		case TYPE_ARRAY:
		case TYPE_POINTER:
		case TYPE_IPTR:
		case TYPE_I64:
			sprintf(buf, "qword");
			break;
		case TYPE_INT:
		case TYPE_I32:
		case TYPE_FLOAT:
			sprintf(buf, "dword");
			break;
		case TYPE_I16:
			sprintf(buf, "word");
			break;
		case TYPE_BOOL:
		case TYPE_I8:
			sprintf(buf, "byte");
			break;
		default: unreachable;
	}
}

char opr_to_nasm_buf[64];
char *opr_to_nasm(TAC_Operand opr) {
	switch (opr.kind) {
		case OPR_SIZEOF: {
			uint size = get_type_size(opr.size_of.v_type);
			if (opr.size_of.v_type.kind == TYPE_ARRAY)
				size = get_type_size(*opr.size_of.v_type.array.elem) * opr.size_of.v_type.array.length;
			sprintf(opr_to_nasm_buf, "%u", size);
		} break;

		case OPR_LABEL: {
			sprintf(opr_to_nasm_buf, ".L%u", opr.label_id);
		} break;

		case OPR_VAR: {
			char ts[32]; var_type_to_stack(opr, ts);

			if (opr.var.kind == VAR_STACK) {
				uint off = *OffTable_get(&stack_table, opr.var.addr_id);
				if (opr.var.off.count != 0) off -= get_struct_alignment(opr, false);
				sprintf(opr_to_nasm_buf, "%s [rbp - %u]", ts, off);
			} else if (opr.var.kind == VAR_ADDR) {
				if (opr.var.addr_kind == VAR_STACK) {
					uint off = *OffTable_get(&stack_table, opr.var.addr_id);
					sb_appendf(&body, TAB"mov rdx, qword [rbp - %u]\n", off);
					uint doff = 0;
					if (opr.var.off.count != 0) doff = get_struct_alignment(opr, false);
					sprintf(opr_to_nasm_buf, "%s [rdx + %u]", ts, doff);
				} else if (opr.var.addr_kind == VAR_DATAOFF) {
					uint off = 0;
					if (opr.var.off.count != 0) off += get_struct_alignment(opr, true);
					sb_appendf(&body, TAB"lea rax, [rel D%u + %lu]\n", opr.var.addr_id, off);
					sprintf(opr_to_nasm_buf, "%s [rax]", ts);
				} else unreachable;
			} else if (opr.var.kind == VAR_DATAOFF) {
				size_t off = 0;
				if (opr.var.off.count != 0) off += get_struct_alignment(opr, true);
				sprintf(opr_to_nasm_buf, "%s [rel D%u + %lu]", ts, opr.var.addr_id, off);
			}
		} break;

		case OPR_LITERAL: {
			switch (opr.literal.type.kind) {
				case TYPE_F32: {
					float x = (float)opr.literal.lfloat;
					uint32_t bits;
					memcpy(&bits, &x, 4);
					sb_appendf(&body, TAB"mov r10d, 0x%08X\n", bits);
					sb_appendf(&body, TAB"movd xmm0, r10d\n", bits);
					sprintf(opr_to_nasm_buf, "xmm0");
				} break;
				case TYPE_INT:
					sprintf(opr_to_nasm_buf, "%d", (i32) opr.literal.lint);
					break;
				case TYPE_BOOL:
				case TYPE_I8:
					sprintf(opr_to_nasm_buf, "%d", (i8) opr.literal.lint);
					break;
				case TYPE_ARRAY:
				case TYPE_POINTER:
				case TYPE_IPTR:
				case TYPE_I64:
					sprintf(opr_to_nasm_buf, "%li", opr.literal.lint);
					break;
				default: printf("%d\n", opr.literal.type.kind); unreachable;
			}
		} break;

		case OPR_FUNC_RET: {
			switch (opr.func_ret.type.kind) {
				case TYPE_INT:
					sprintf(opr_to_nasm_buf, "eax");
					break;
				case TYPE_BOOL:
				case TYPE_I8:
					sprintf(opr_to_nasm_buf, "al");
					break;
				case TYPE_ARRAY:
				case TYPE_POINTER:
				case TYPE_IPTR:
				case TYPE_I64:
					sprintf(opr_to_nasm_buf, "rax");
					break;
				default: unreachable;
			}
		} break;

		case OPR_FUNC_INP: {
			switch (opr.func_inp.type.kind) {
				case TYPE_INT:
					sprintf(opr_to_nasm_buf, "%s", argreg32[opr.func_inp.arg_id]);
					break;
				case TYPE_BOOL:
				case TYPE_I8:
					sprintf(opr_to_nasm_buf, "%s", argreg8[opr.func_inp.arg_id]);
					break;
				case TYPE_ARRAY:
				case TYPE_POINTER:
				case TYPE_IPTR:
				case TYPE_I64:
					sprintf(opr_to_nasm_buf, "%s", argreg64[opr.func_inp.arg_id]);
					break;
				default: unreachable;
			}
		} break;

		default: unreachable;
	}

	return opr_to_nasm_buf;
}

void type_to_reg(TAC_Operand opr, char *arg1, char *arg2) {
	switch (tac_ir_get_opr_type(opr).kind) {
		case TYPE_I8:
		case TYPE_BOOL:
			sprintf(arg1, "al");
			sprintf(arg2, "bl");
			break;
		case TYPE_FLOAT:
			sprintf(arg1, "xmm0");
			sprintf(arg2, "xmm1");
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

void reg_alloc(TAC_Instruction inst, char *arg1, char *arg2) {
	if (inst.dst.kind == OPR_LABEL) {
		inst.dst.kind = OPR_VAR;
		inst.dst.var.type = (Type){.kind = TYPE_BOOL};
		type_to_reg(inst.dst, arg1, arg2);
		return;
	}

	if (inst.dst.var.type.kind == TYPE_BOOL) {
		type_to_reg(inst.arg1, arg1, arg2);
		return;
	}

	type_to_reg(inst.dst, arg1, arg2);
}

void total_offset_add(size_t off) {
	total_offset += off;
	if (total_offset % 8 != 0) {
		total_offset += 8 - total_offset % 8;
	}
}

void nasm_gen_new_stack_var(TAC_Instruction ci, char *dst, char *arg1, char *arg2) {
	char ts[32]; var_type_to_stack(ci.dst, ts);
	total_offset_add(get_type_size(ci.dst.var.type));
	OffTable_add(&stack_table, ci.dst.var.addr_id, total_offset);
	sprintf(dst, "%s [rbp - %u]", ts, total_offset);
	reg_alloc(ci, arg1, arg2);
}

void nasm_gen_func(StringBuilder *code, TAC_Func func) {
	sb_appendf(code, "global %s\n", func.name);
	sb_appendf(code, "%s:\n", func.name);

	sb_appendf(code, TAB"push rbp\n");
	sb_appendf(code, TAB"mov rbp, rsp\n");

	sb_reset(&body);

	total_offset = 0;

	for (size_t i = 0; i < func.body.count; i++) {
		char arg1[64], arg2[64], dst[64];
		TAC_Instruction ci = da_get(&func.body, i);

		/*{ // debug information
			char res[256];
			ir_dump_inst(ci, res);
			printf("%s\n", res);
			sb_appendf(&body, ";%s\n", res);
		}*/

		switch (ci.op) {
			case OP_ADD:   case OP_SUB:
			case OP_MUL:   case OP_DIV:
			case OP_EQ:    case OP_NOT_EQ:
			case OP_AND:   case OP_OR:
			case OP_GREAT: case OP_LESS:
			case OP_MOD:   case OP_LESS_EQ:
			case OP_GREAT_EQ: {
				nasm_gen_new_stack_var(ci, dst, arg1, arg2);

				sb_appendf(&body, TAB"mov %s, %s\n", arg1, opr_to_nasm(ci.arg1));
				sb_appendf(&body, TAB"mov %s, %s\n", arg2, opr_to_nasm(ci.arg2));

				if      (ci.op == OP_ADD) sb_appendf(&body, TAB"add %s, %s\n", arg1, arg2);
				else if (ci.op == OP_SUB) sb_appendf(&body, TAB"sub %s, %s\n", arg1, arg2);
				else if (ci.op == OP_MUL) sb_appendf(&body, TAB"imul %s, %s\n", arg1, arg2);
				else if (ci.op == OP_DIV || ci.op == OP_MOD) {
					switch (ci.dst.var.type.kind) {
						case TYPE_INT: sb_appendf(&body, TAB"mov edx, 0\n"); break;
						case TYPE_I64: sb_appendf(&body, TAB"mov rdx, 0\n"); break;
						default: unreachable;
					}

					sb_appendf(&body, TAB"idiv %s\n", arg2);
					if (ci.op == OP_MOD) {
						switch (ci.dst.var.type.kind) {
							case TYPE_INT: sprintf(arg1, "edx"); break;
							case TYPE_I64: sprintf(arg1, "rdx"); break;
							default: unreachable;
						}
					}
				}

				else if (ci.op == OP_EQ) {
					sb_appendf(&body, TAB"cmp %s, %s\n", arg1, arg2);
					sb_appendf(&body, TAB"sete al\n");
					sprintf(arg1, "al");
				} else if (ci.op == OP_NOT_EQ) {
					sb_appendf(&body, TAB"cmp %s, %s\n", arg1, arg2);
					sb_appendf(&body, TAB"setne al\n");
					sprintf(arg1, "al");
				} else if (ci.op == OP_AND) {
					sb_appendf(&body, TAB"and %s, %s\n", arg1, arg2);
					sprintf(arg1, "al");
				} else if (ci.op == OP_OR) {
					sb_appendf(&body, TAB"or %s, %s\n", arg1, arg2);
					sprintf(arg1, "al");
				} else if (ci.op == OP_GREAT) {
					sb_appendf(&body, TAB"cmp %s, %s\n", arg1, arg2);
					sb_appendf(&body, TAB"setg al\n");
					sprintf(arg1, "al");
				} else if (ci.op == OP_LESS) {
					sb_appendf(&body, TAB"cmp %s, %s\n", arg1, arg2);
					sb_appendf(&body, TAB"setl al\n");
					sprintf(arg1, "al");
				} else if (ci.op == OP_GREAT_EQ) {
					sb_appendf(&body, TAB"cmp %s, %s\n", arg1, arg2);
					sb_appendf(&body, TAB"setge al\n");
					sprintf(arg1, "al");
				} else if (ci.op == OP_LESS_EQ) {
					sb_appendf(&body, TAB"cmp %s, %s\n", arg1, arg2);
					sb_appendf(&body, TAB"setle al\n");
					sprintf(arg1, "al");
				}

				sb_appendf(&body, TAB"mov %s, %s\n", dst, arg1);
			} break;

			case OP_NOT: case OP_NEG: {
				nasm_gen_new_stack_var(ci, dst, arg1, arg2);

				sb_appendf(&body, TAB"mov %s, %s\n", arg1, opr_to_nasm(ci.arg1));

				if      (ci.op == OP_NEG) sb_appendf(&body, TAB"neg %s\n", arg1);
				else if (ci.op == OP_NOT) {
					sb_appendf(&body, TAB"test %s, %s\n", arg1, arg1);
					sb_appendf(&body, TAB"setz al\n");
					sprintf(arg1, "al");
				}

				sb_appendf(&body, TAB"mov %s, %s\n", dst, arg1);
			} break;

			case OP_CAST: {
				Type dst_type = ci.dst.var.type;
				Type arg1_type; switch (ci.arg1.kind) {
					case OPR_LITERAL: arg1_type = ci.arg1.literal.type; break;
					case OPR_VAR:     arg1_type = ci.arg1.var.type;     break;
					case OPR_SIZEOF:  arg1_type = ci.arg1.size_of.type; break;
					default: unreachable;
				}

				nasm_gen_new_stack_var(ci, dst, arg1, arg2);

				if (dst_type.kind == arg1_type.kind) { unreachable;
				} else if (dst_type.kind == TYPE_I8 && arg1_type.kind == TYPE_INT) {
					sb_appendf(&body, TAB"mov eax, %s\n", opr_to_nasm(ci.arg1));
					sb_appendf(&body, TAB"mov al, al\n");
					sb_appendf(&body, TAB"movsx eax, al\n");
					sb_appendf(&body, TAB"mov %s, al\n", opr_to_nasm(ci.dst));
				} else if (dst_type.kind == TYPE_INT && arg1_type.kind == TYPE_I8) {
					sb_appendf(&body, TAB"movsx eax, %s\n", opr_to_nasm(ci.arg1));
					sb_appendf(&body, TAB"mov %s, eax\n", opr_to_nasm(ci.dst));
				} else if ((dst_type.kind == TYPE_POINTER || dst_type.kind == TYPE_I64 || dst_type.kind == TYPE_IPTR) && arg1_type.kind == TYPE_INT) {
					sb_appendf(&body, TAB"mov eax, %s\n", opr_to_nasm(ci.arg1));
					sb_appendf(&body, TAB"movsxd rax, eax\n");
					sb_appendf(&body, TAB"mov %s, rax\n", opr_to_nasm(ci.dst));
				} else if (dst_type.kind == TYPE_INT && (dst_type.kind == TYPE_POINTER || arg1_type.kind == TYPE_I64 || arg1_type.kind == TYPE_IPTR)) {
					sb_appendf(&body, TAB"mov rax, %s\n", opr_to_nasm(ci.arg1));
					sb_appendf(&body, TAB"mov eax, eax\n");
					sb_appendf(&body, TAB"movsx rax, eax\n");
					sb_appendf(&body, TAB"mov %s, eax\n", opr_to_nasm(ci.dst));
				} else unreachable;
			} break;

			case OP_ASSIGN: {
				bool is_first_assign = false;
				if (ci.dst.var.kind != VAR_ADDR) {
					uint *off = OffTable_get(&stack_table, ci.dst.var.addr_id);

					if (!off) {
						is_first_assign = true;
						total_offset_add(get_type_size(ci.dst.var.type));
						OffTable_add(&stack_table, ci.dst.var.addr_id, total_offset);
					}
				}


				if (ci.dst.var.type.kind == TYPE_ARRAY && is_first_assign) {
					reg_alloc(ci, arg1, arg2);
					total_offset_add(get_type_size(*ci.dst.var.type.array.elem) * ci.dst.var.type.array.length);
					sb_appendf(&body, TAB"lea %s, [rbp - %u]\n", arg1, total_offset);
					sb_appendf(&body, TAB"mov %s, %s\n", opr_to_nasm(ci.dst), arg1);
				}

				if (ci.arg1.kind != OPR_NULL) {
					reg_alloc(ci, arg1, arg2);
					sprintf(dst, "%s", opr_to_nasm(ci.dst));

					sb_appendf(&body, TAB"mov %s, %s\n", arg2, opr_to_nasm(ci.arg1));
					sb_appendf(&body, TAB"mov %s, %s\n", dst, arg2);
				}
			} break;

			case OP_REF: {
				char ts[32]; var_type_to_stack(ci.dst, ts);
				total_offset_add(get_type_size(ci.dst.var.type));
				OffTable_add(&stack_table, ci.dst.var.addr_id, total_offset);

				if (ci.arg1.var.kind == VAR_ADDR) {
					if (ci.arg1.var.addr_kind == VAR_STACK) {
						size_t off = *OffTable_get(&stack_table, ci.arg1.var.addr_id);
						sb_appendf(&body, TAB"mov rax, [rbp - %u]\n", off);
					} else if (ci.arg1.var.addr_kind == VAR_DATAOFF) {
						sb_appendf(&body, TAB"lea rax, [rel D%u]\n", ci.arg1.var.addr_id);
					}
				} else if (ci.arg1.var.kind == VAR_STACK) {
					size_t off = *OffTable_get(&stack_table, ci.arg1.var.addr_id);
					sb_appendf(&body, TAB"lea rax, [rbp - %u]\n", off);
				} else if (ci.arg1.var.kind == VAR_DATAOFF) {
					sb_appendf(&body, TAB"lea rax, [rel D%u]\n", ci.arg1.var.addr_id);
				}

				sprintf(dst, "%s", opr_to_nasm(ci.dst));
				sb_appendf(&body, TAB"mov %s, rax\n", dst);
			} break;

			case OP_JUMP_IF_NOT: {
				reg_alloc(ci, arg1, arg2);
				sb_appendf(&body, TAB"mov %s, %s\n", arg1, opr_to_nasm(ci.arg1));
				sb_appendf(&body, TAB"cmp %s, 0\n", arg1);
				sb_appendf(&body, TAB"je %s\n", opr_to_nasm(ci.dst));
			} break;

			case OP_LABEL: {
				sb_appendf(&body, "%s:\n", opr_to_nasm(ci.arg1));
			} break;

			case OP_JUMP: {
				sb_appendf(&body, TAB"jmp %s\n", opr_to_nasm(ci.dst));
			} break;

			case OP_FADDR: {
				nasm_gen_new_stack_var(ci, dst, arg1, arg2);

				uint align = 0;
				da_foreach(Field, field, &ci.arg1.var.type.user->ustruct.fields) {
					if (strcmp(field->id, ci.arg2.field_id) == 0) {
						align += get_type_size(field->type);
					}
				}

				sb_appendf(&body, TAB"mov rdx, %s\n", opr_to_nasm(ci.arg1));
				sb_appendf(&body, TAB"lea rax, [rdx + %li]\n", align);
				sb_appendf(&body, TAB"mov %s, rax\n", opr_to_nasm(ci.dst));
			} break;

			case OP_RETURN: {
				if (ci.arg1.kind != OPR_NULL) {
					switch (func.ret_type.kind) {
						case TYPE_IPTR:
						case TYPE_POINTER:
						case TYPE_I64:
							sb_appendf(&body, TAB"mov rax, %s\n", opr_to_nasm(ci.arg1));
							break;
						case TYPE_INT:
							sb_appendf(&body, TAB"mov eax, %s\n", opr_to_nasm(ci.arg1));
							break;
						case TYPE_I8:
						case TYPE_BOOL:
							sb_appendf(&body, TAB"mov al, %s\n", opr_to_nasm(ci.arg1));
							break;
						default: unreachable;
					}
				}

				sb_appendf(&body, TAB"mov rsp, rbp\n");
				sb_appendf(&body, TAB"pop rbp\n");
				sb_appendf(&body, TAB"ret\n");
			} break;

			case OP_FUNC_CALL: {
				for (size_t i = 0; ci.args[i].kind != OPR_NULL; i++) {
					switch (tac_ir_get_opr_type(ci.args[i]).kind) {
						case TYPE_ARRAY:
						case TYPE_POINTER:
						case TYPE_IPTR:
						case TYPE_I64:
							sb_appendf(&body, TAB"mov %s, %s\n", argreg64[i], opr_to_nasm(ci.args[i]));
							break;
						case TYPE_INT:
							sb_appendf(&body, TAB"mov %s, %s\n", argreg32[i], opr_to_nasm(ci.args[i]));
							break;
						case TYPE_I8:
						case TYPE_BOOL:
							sb_appendf(&body, TAB"mov %s, %s\n", argreg8[i], opr_to_nasm(ci.args[i]));
							break;
						default: unreachable;
					}
				}

				sb_appendf(&body, TAB"call %s\n", ci.dst.name);
			} break;

			default: unreachable;
		}
	}

	total_offset += 8;

	if (total_offset % 16 != 0) {
		total_offset += 16 - total_offset % 16;
	}

	sb_appendf(code, TAB"sub rsp, %u\n", total_offset);
	sb_appendf(code, "%s", body.items);
	if (da_last(&func.body).op != OP_RETURN) {
		sb_appendf(code, TAB"mov rsp, rbp\n");
		sb_appendf(code, TAB"pop rbp\n");
		sb_appendf(code, TAB"ret\n");
	}

	sb_appendf(code, "\n");
}

char *nasm_gen_prog(TAC_Program *prog) {
	StringBuilder code = {0};

	da_foreach(TAC_Extern, ext, &prog->externs) {
		sb_appendf(&code, "extern %s\n", ext->name);
	}
	sb_appendf(&code, "\n");

	sb_appendf(&code, "section .data\n");
	uint uniq_data_off = 0;
	da_foreach(TAC_GlobalVar, g, &prog->globals) {
		if (g->type.kind == TYPE_ARRAY) {
			uint arr_size = get_type_size(*g->type.array.elem) * g->type.array.length;
			if (g->data) {
				sb_appendf(&code, TAB"U%u db ", uniq_data_off);
				for (size_t i = 0; i < g->type.array.length; i++) {
					sb_appendf(&code, "%#x", g->data[i]);
					if (i != g->type.array.length - 1) sb_appendf(&code, ", ");
				}
				sb_appendf(&code, "\n");
			} else sb_appendf(&code, TAB"U%u times %u db 0\n", uniq_data_off, arr_size);
			sb_appendf(&code, TAB"D%u dq U%u\n", g->index, uniq_data_off++);
		} else sb_appendf(&code, TAB"D%u times %u db 0\n", g->index, get_type_size(g->type));
	}
	sb_appendf(&code, "\n");

	sb_appendf(&code, "section .text\n");
	for (size_t i = 0; i < prog->funcs.count; i++) {
		nasm_gen_func(&code, da_get(&prog->funcs, i));
	}

	return code.items;
}
