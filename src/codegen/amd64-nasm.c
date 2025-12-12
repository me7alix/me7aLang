#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include <assert.h>

#include "../../include/platform.h"
#include "../../include/tac_ir.h"

const int sysv_regs_cnt = 6;
const char *sysv_regs[][6] = {
	{ "dil",  "sil",  "dl",   "cl",   "r8b",  "r9b" },
	{ "di",   "si",   "dx",   "cx",   "r8w",  "r9w" },
	{ "edi",  "esi",  "edx",  "ecx",  "r8d",  "r9d" },
	{ "rdi",  "rsi",  "rdx",  "rcx",  "r8",   "r9"  },
};

const int win_regs_cnt = 4;
const char *win_regs[][4] = {
	{ "cl",   "dl",   "r8b",  "r9b" },
	{ "cx",   "dx",   "r8w",  "r9w" },
	{ "ecx",  "edx",  "r8d",  "r9d" },
	{ "rcx",  "rdx",  "r8",   "r9"  },
};

const char *R10[] = {"r10b", "r10w", "r10d", "r10"};
const char *R11[] = {"r11b", "r11w", "r11d", "r11"};
const char *RAX[] = {"al",   "ax",   "eax",  "rax"};
const char *RDX[] = {"dl",   "dx",   "edx",  "rdx"};
const char *RCX[] = {"cl",   "cx",   "ecx",  "rcx"};

HT_DECL(OffTable, uint, uint)
HT_IMPL_NUM(OffTable, uint, uint)

OffTable stack_table = {0};
OffTable data_table  = {0};

StringBuilder body = {0};
TargetPlatform tp;
uint total_offset;

size_t get_reg_row(Type t) {
	switch (t.kind) {
		case TYPE_BOOL:
		case TYPE_I8:
		case TYPE_U8:
			return 0;
		case TYPE_I16:
		case TYPE_U16:
			return 1;
		case TYPE_U32:
		case TYPE_I32:
		case TYPE_INT:
		case TYPE_UINT:
			return 2;
		case TYPE_ARRAY:
		case TYPE_POINTER:
		case TYPE_IPTR:
		case TYPE_UPTR:
		case TYPE_I64:
		case TYPE_U64:
			return 3;
		default: UNREACHABLE;
	}
}

uint get_type_size(Type type) {
	switch (type.kind) {
		default: return 1 << get_reg_row(type);
		case TYPE_STRUCT: {
			uint total = 0;
			da_foreach (Field, field, &type.user->ustruct.fields)
				total += get_type_size(field->type);
			return total;
		} break;
	}
}

uint get_struct_alignment(TAC_Operand var) {
	if (var.var.fields.count == 0) return 0;
	uint total = 0;

	for (size_t i = 0; i < var.var.fields.count; i++) {
		char *off = da_get(&var.var.fields, i);
		da_foreach (Field, field, &var.var.type.user->ustruct.fields) {
			total += get_type_size(field->type);
			if (strcmp(field->id, off) == 0) {
				var.var.type = field->type;
				total -= get_type_size(field->type);
				break;
			}
		}
	}

	return total;
}

void opr_type_to_stack(TAC_Operand t, char *buf) {
	switch (tac_ir_get_opr_type(t).kind) {
		case TYPE_STRUCT:
			sprintf(buf, " ");
			break;
		default: {
			uint reg_row = get_reg_row(tac_ir_get_opr_type(t));
			sprintf(buf, "%s", (char*[]){"byte", "word", "dword", "qword"}[reg_row]);
		}
	}
}

char *opr_to_nasm(TAC_Operand opr) {
	static char rbuf[64];
	switch (opr.kind) {
		case OPR_SIZEOF: {
			uint size = get_type_size(opr.size_of.vtype);
			if (opr.size_of.vtype.kind == TYPE_ARRAY)
				size = get_type_size(*opr.size_of.vtype.array.elem) * opr.size_of.vtype.array.length;
			sprintf(rbuf, "%u", size);
		} break;

		case OPR_LABEL: {
			sprintf(rbuf, ".L%u", opr.label_id);
		} break;

		case OPR_VAR: {
			char ts[32]; opr_type_to_stack(opr, ts);
			if (opr.var.kind == VAR_STACK) {
				uint off = *OffTable_get(&stack_table, opr.var.addr_id);
				sprintf(rbuf, "%s [rbp - %u]", ts, off - get_struct_alignment(opr));
			} else if (opr.var.kind == VAR_ADDR) {
				if (opr.var.addr_kind == VAR_STACK) {
					uint off = *OffTable_get(&stack_table, opr.var.addr_id);
					sb_appendf(&body, "    mov rax, qword [rbp - %u]\n", off);
					sprintf(rbuf, "%s [rax + %u]", ts, get_struct_alignment(opr));
				} else if (opr.var.addr_kind == VAR_DATA) {
					uint field_off = get_struct_alignment(opr);
					sb_appendf(&body, "    lea rax, [rel D%u + %lu]\n", opr.var.addr_id, field_off);
					sprintf(rbuf, "%s [rax]", ts);
				} else UNREACHABLE;
			} else if (opr.var.kind == VAR_DATA) {
				uint field_off = get_struct_alignment(opr);
				sprintf(rbuf, "%s [rel D%u + %u]", ts, opr.var.addr_id, field_off);
			}
		} break;

		case OPR_LITERAL: {
			switch (opr.literal.type.kind) {
				case TYPE_FLOAT:
				case TYPE_F32: {
					float x = (float)opr.literal.lfloat;
					uint32_t bits;
					memcpy(&bits, &x, 4);
					sb_appendf(&body, "    mov r10d, 0x%08X\n", bits);
					sb_appendf(&body, "    movd xmm0, r10d\n", bits);
					sprintf(rbuf, "xmm0");
				} break;
				case TYPE_I32:
				case TYPE_INT:
					sprintf(rbuf, "%d", (int) opr.literal.lint);
					break;
				case TYPE_UINT:
					sprintf(rbuf, "%u", (uint) opr.literal.lint);
					break;
				case TYPE_BOOL:
				case TYPE_I8:
					sprintf(rbuf, "%d", (i8) opr.literal.lint);
					break;
				case TYPE_U8:
					sprintf(rbuf, "%d", (u8) opr.literal.lint);
					break;
				case TYPE_I16:
					sprintf(rbuf, "%hd", (i16) opr.literal.lint);
					break;
				case TYPE_U16:
					sprintf(rbuf, "%hu", (u16) opr.literal.lint);
					break;
				case TYPE_UPTR:
				case TYPE_U64:
					sprintf(rbuf, "%llu", opr.literal.lint);
					break;
				case TYPE_ARRAY:
				case TYPE_POINTER:
				case TYPE_IPTR:
				case TYPE_I64:
					sprintf(rbuf, "%lli", opr.literal.lint);
					break;
				default: UNREACHABLE;
			}
		} break;

		case OPR_FUNC_RET: {
			switch (opr.func_ret.type.kind) {
				case TYPE_ARRAY:
				case TYPE_STRUCT:
					assert(!"error: passing arrays/structs isn't supported yet\n");
				default: {
					uint reg_row = get_reg_row(opr.func_ret.type);
					sprintf(rbuf, "%s", RAX[reg_row]);
				}
			}
		} break;

		case OPR_FUNC_INP: {
			char ts[32]; opr_type_to_stack(opr, ts);
			uint arg_id = opr.func_inp.arg_id;
			size_t arg_row = get_reg_row(opr.func_inp.type);

			switch (tp) {
				case TP_NULL: break;
				case TP_MACOS:
				case TP_LINUX: {
					if (arg_id >= sysv_regs_cnt) {
						uint shadow_space = (arg_id - sysv_regs_cnt) * 8 + 48;
						sb_appendf(&body, "    mov %s, %s [rbp + %u]\n", R10[arg_row], ts, shadow_space);
						sprintf(rbuf, "%s", R10[arg_row]);
					} else {
						sprintf(rbuf, "%s", sysv_regs[arg_row][arg_id]);
					}
				} break;
				case TP_WINDOWS: {
					if (arg_id >= win_regs_cnt) {
						uint shadow_space = (arg_id - win_regs_cnt) * 8 + 48;
						sb_appendf(&body, "    mov %s, %s [rbp + %u]\n", R10[arg_row], ts, shadow_space);
						sprintf(rbuf, "%s", R10[arg_row]);
					} else {
						sprintf(rbuf, "%s", win_regs[arg_row][arg_id]);
					}
				} break;
			} break;
		} break;

		default: UNREACHABLE;
	}

	return rbuf;
}

void type_to_reg(TAC_Operand opr, char *arg1, char *arg2) {
	Type opr_type = tac_ir_get_opr_type(opr);

	switch (opr_type.kind) {
		case TYPE_F32:
		case TYPE_FLOAT:
			sprintf(arg1, "xmm0");
			sprintf(arg2, "xmm1");
			break;
		default: {
			sprintf(arg1, "%s", R10[get_reg_row(opr_type)]);
			sprintf(arg2, "%s", R11[get_reg_row(opr_type)]);
		}
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

void align_up(uint *x, uint a) {
	if (*x % a != 0) *x += a - *x % a;
}

void total_offset_add(uint off) {
	total_offset += off;
	align_up(&total_offset, 8);
}

void nasm_gen_new_stack_var(TAC_Instruction ci, char *dst, char *arg1, char *arg2) {
	char ts[32]; opr_type_to_stack(ci.dst, ts);
	total_offset_add(get_type_size(ci.dst.var.type));
	OffTable_add(&stack_table, ci.dst.var.addr_id, total_offset);
	sprintf(dst, "%s [rbp - %u]", ts, total_offset);
	reg_alloc(ci, arg1, arg2);
}

void nasm_gen_func(StringBuilder *code, TAC_Func func) {
	if (func.body.count == 0) return;

	sb_appendf(code, "global %s\n", func.name);
	sb_appendf(code, "%s%s:\n", (tp == TP_MACOS ? "_" : ""), func.name);

	sb_appendf(code, "    push rbp\n");
	sb_appendf(code, "    mov rbp, rsp\n");

	sb_reset(&body);

	total_offset = 0;

	for (size_t i = 0; i < func.body.count; i++) {
		char arg1[64], arg2[64], dst[64];
		TAC_Instruction ci = da_get(&func.body, i);

#ifdef _CP_RUNTIME_CHECKS
		char res[256];
		tac_ir_dump_inst(ci, res);
		printf("%s\n", res);
		sb_appendf(&body, ";%s\n", res);
#endif

		switch (ci.op) {
			case OP_ADD:    case OP_SUB:
			case OP_MUL:    case OP_DIV:
			case OP_EQ:     case OP_NOT_EQ:
			case OP_AND:    case OP_OR:
			case OP_GREAT:  case OP_LESS:
			case OP_MOD:    case OP_LESS_EQ:
			case OP_BW_AND: case OP_BW_OR:
			case OP_BW_LS:  case OP_BW_RS:
			case OP_BW_XOR: case OP_GREAT_EQ: {
				nasm_gen_new_stack_var(ci, dst, arg1, arg2);

				sb_appendf(&body, "    mov %s, %s\n", arg1, opr_to_nasm(ci.arg1));
				sb_appendf(&body, "    mov %s, %s\n", arg2, opr_to_nasm(ci.arg2));

				if      (ci.op == OP_ADD)    sb_appendf(&body, "    add %s, %s\n", arg1, arg2);
				else if (ci.op == OP_SUB)    sb_appendf(&body, "    sub %s, %s\n", arg1, arg2);
				else if (ci.op == OP_BW_AND) sb_appendf(&body, "    and %s, %s\n", arg1, arg2);
				else if (ci.op == OP_BW_OR)  sb_appendf(&body, "    or  %s, %s\n", arg1, arg2);
				else if (ci.op == OP_BW_XOR) sb_appendf(&body, "    xor %s, %s\n", arg1, arg2);

				else if (ci.op == OP_BW_LS || ci.op == OP_BW_RS) {
					const char *rcx = RCX[get_reg_row(ci.dst.var.type)];
					sb_appendf(&body, "    mov %s, %s\n", rcx, arg2);
					sb_appendf(&body, "    %s %s, cl\n", ci.op == OP_BW_LS ? "shl" : "shr", arg1);
				}

				else if (ci.op == OP_MUL) {
					switch (ci.dst.var.type.kind) {
						case TYPE_ARRAY:
						case TYPE_POINTER:
						case TYPE_UINT: case TYPE_U8:
						case TYPE_U32: case TYPE_U16:
						case TYPE_U64: case TYPE_UPTR:
							sb_appendf(&body, "    mul %s, %s\n", arg1, arg2);
							break;

						case TYPE_IPTR:
						case TYPE_BOOL: case TYPE_I8:
						case TYPE_INT: case TYPE_I32:
						case TYPE_I64: case TYPE_I16:
							sb_appendf(&body, "    imul %s, %s\n", arg1, arg2);
							break;

						default: UNREACHABLE;
					}
				}

				else if (ci.op == OP_DIV || ci.op == OP_MOD) {
					char *SEI[] = {"cbw", "cwd", "cdq", "cqo"};
					uint reg_row = get_reg_row(ci.dst.var.type);
					sb_appendf(&body, "    mov %s, %s\n", RAX[reg_row], arg1);

					switch (ci.dst.var.type.kind) {
						case TYPE_ARRAY:
						case TYPE_POINTER:
						case TYPE_UINT: case TYPE_U8:
						case TYPE_U32: case TYPE_U16:
						case TYPE_U64: case TYPE_UPTR:
							sb_appendf(&body, "    xor rdx, rdx\n");
							sb_appendf(&body, "    div %s\n", arg2);
							break;

						case TYPE_IPTR:
						case TYPE_BOOL: case TYPE_I8:
						case TYPE_INT: case TYPE_I32:
						case TYPE_I64: case TYPE_I16:
							sb_appendf(&body, "    %s\n", SEI[reg_row]);
							sb_appendf(&body, "    idiv %s\n", arg2);
							break;

						default: UNREACHABLE;
					}

					if (ci.op == OP_DIV) sprintf(arg1, "%s", RAX[reg_row]);
					else                 sprintf(arg1, "%s", RDX[reg_row]);
				}

				else if (ci.op == OP_EQ) {
					sb_appendf(&body, "    cmp %s, %s\n", arg1, arg2);
					sb_appendf(&body, "    sete al\n", arg1);
					sprintf(arg1, "al");
				} else if (ci.op == OP_NOT_EQ) {
					sb_appendf(&body, "    cmp %s, %s\n", arg1, arg2);
					sb_appendf(&body, "    setne al\n");
					sprintf(arg1, "al");
				} else if (ci.op == OP_AND) {
					sb_appendf(&body, "    and %s, %s\n", arg1, arg2);
				} else if (ci.op == OP_OR) {
					sb_appendf(&body, "    or %s, %s\n", arg1, arg2);
				} else if (ci.op == OP_GREAT) {
					sb_appendf(&body, "    cmp %s, %s\n", arg1, arg2);
					sb_appendf(&body, "    setg al\n");
					sprintf(arg1, "al");
				} else if (ci.op == OP_LESS) {
					sb_appendf(&body, "    cmp %s, %s\n", arg1, arg2);
					sb_appendf(&body, "    setl al\n");
					sprintf(arg1, "al");
				} else if (ci.op == OP_GREAT_EQ) {
					sb_appendf(&body, "    cmp %s, %s\n", arg1, arg2);
					sb_appendf(&body, "    setge al\n");
					sprintf(arg1, "al");
				} else if (ci.op == OP_LESS_EQ) {
					sb_appendf(&body, "    cmp %s, %s\n", arg1, arg2);
					sb_appendf(&body, "    setle al\n");
					sprintf(arg1, "al");
				}

				sb_appendf(&body, "    mov %s, %s\n", dst, arg1);
			} break;

			case OP_BW_NOT:
			case OP_NOT: case OP_NEG: {
				nasm_gen_new_stack_var(ci, dst, arg1, arg2);
				sb_appendf(&body, "    mov %s, %s\n", arg1, opr_to_nasm(ci.arg1));

				if      (ci.op == OP_NEG)    sb_appendf(&body, "    neg %s\n", arg1);
				else if (ci.op == OP_BW_NOT) sb_appendf(&body, "    not %s\n", arg1);
				else if (ci.op == OP_NOT) {
					sb_appendf(&body, "    test %s, %s\n", arg1, arg1);
					sb_appendf(&body, "    setz al\n");
					sprintf(arg1, "al");
				}

				sb_appendf(&body, "    mov %s, %s\n", dst, arg1);
			} break;

			case OP_CAST: {
				Type dst_type = ci.dst.var.type;
				Type arg1_type; switch (ci.arg1.kind) {
					case OPR_LITERAL: arg1_type = ci.arg1.literal.type; break;
					case OPR_VAR:     arg1_type = ci.arg1.var.type;     break;
					case OPR_SIZEOF:  arg1_type = ci.arg1.size_of.type; break;
					default: UNREACHABLE;
				}

				nasm_gen_new_stack_var(ci, dst, arg1, arg2);

				if (dst_type.kind == arg1_type.kind) { UNREACHABLE; }

				int dst_size = 0;
				int src_size = 0;
				bool src_signed = false;

				switch (dst_type.kind) {
					case TYPE_I8: dst_size = 1; break;
					case TYPE_U8: dst_size = 1; break;
					case TYPE_I16: dst_size = 2; break;
					case TYPE_U16: dst_size = 2; break;
					case TYPE_INT: case TYPE_I32: dst_size = 4; break;
					case TYPE_UINT: case TYPE_U32: dst_size = 4; break;
					case TYPE_I64: case TYPE_IPTR: dst_size = 8; break;
					case TYPE_U64: case TYPE_UPTR: case TYPE_POINTER: dst_size = 8; break;
					default: UNREACHABLE;
				}

				switch (arg1_type.kind) {
					case TYPE_I8: src_size = 1; src_signed = true; break;
					case TYPE_U8: src_size = 1; src_signed = false; break;
					case TYPE_I16: src_size = 2; src_signed = true; break;
					case TYPE_U16: src_size = 2; src_signed = false; break;
					case TYPE_INT: case TYPE_I32: src_size = 4; src_signed = true; break;
					case TYPE_UINT: case TYPE_U32: src_size = 4; src_signed = false; break;
					case TYPE_I64: case TYPE_IPTR: src_size = 8; src_signed = true; break;
					case TYPE_U64: case TYPE_UPTR: case TYPE_POINTER: src_size = 8; src_signed = false; break;
					default: UNREACHABLE;
				}

				const char* ext_inst = src_signed ? "movsx" : "movzx";
				const char* dst_reg = NULL;
				const char* src_reg = NULL;
				const char* low_reg = NULL;

				switch (dst_size) {
					case 1: dst_reg = "al"; low_reg = "al"; break;
					case 2: dst_reg = "ax"; low_reg = "ax"; break;
					case 4: dst_reg = "eax"; low_reg = "eax"; break;
					case 8: dst_reg = "rax"; low_reg = "eax"; break;
					default: UNREACHABLE;
				}

				switch (src_size) {
					case 1: src_reg = "al"; break;
					case 2: src_reg = "ax"; break;
					case 4: src_reg = "eax"; break;
					case 8: src_reg = "rax"; break;
					default: UNREACHABLE;
				}

				if (dst_size > src_size) {
					bool is_32_to_64_signed = (src_size == 4 && dst_size == 8 && src_signed);
					if (is_32_to_64_signed) {
						ext_inst = "movsxd";
					}
					sb_appendf(&body, "    %s %s, %s\n", ext_inst, dst_reg, opr_to_nasm(ci.arg1));
					sb_appendf(&body, "    mov %s, %s\n", opr_to_nasm(ci.dst), dst_reg);
				} else if (dst_size < src_size) {
					sb_appendf(&body, "    mov %s, %s\n", src_reg, opr_to_nasm(ci.arg1));
					sb_appendf(&body, "    mov %s, %s\n", opr_to_nasm(ci.dst), low_reg);
				} else {
					sb_appendf(&body, "    mov %s, %s\n", dst_reg, opr_to_nasm(ci.arg1));
					sb_appendf(&body, "    mov %s, %s\n", opr_to_nasm(ci.dst), dst_reg);
				}
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
					sb_appendf(&body, "    lea %s, [rbp - %u]\n", arg1, total_offset);
					sb_appendf(&body, "    mov %s, %s\n", opr_to_nasm(ci.dst), arg1);
				}

				if (ci.arg1.kind != OPR_NULL) {
					if (tac_ir_get_opr_type(ci.dst).kind == TYPE_STRUCT) {
						sb_appendf(&body, "    lea rsi, %s\n", opr_to_nasm(ci.arg1));
						sb_appendf(&body, "    lea rdi, %s\n", opr_to_nasm(ci.dst));
						sb_appendf(&body, "    mov rcx, %u\n", get_type_size(tac_ir_get_opr_type(ci.dst)));
						sb_appendf(&body, "    cld\n");
						sb_appendf(&body, "    rep movsb\n");
					} else {
						reg_alloc(ci, arg1, arg2);
						sprintf(dst, "%s", opr_to_nasm(ci.dst));

						sb_appendf(&body, "    mov %s, %s\n", arg2, opr_to_nasm(ci.arg1));
						sb_appendf(&body, "    mov %s, %s\n", dst, arg2);
					}
				}
			} break;

			case OP_REF: {
				char ts[32]; opr_type_to_stack(ci.dst, ts);
				total_offset_add(get_type_size(ci.dst.var.type));
				OffTable_add(&stack_table, ci.dst.var.addr_id, total_offset);

				if (ci.arg1.var.kind == VAR_ADDR) {
					if (ci.arg1.var.addr_kind == VAR_STACK) {
						uint off = *OffTable_get(&stack_table, ci.arg1.var.addr_id);
						sb_appendf(&body, "    mov rax, [rbp - %u]\n", off);
					} else if (ci.arg1.var.addr_kind == VAR_DATA) {
						sb_appendf(&body, "    lea rax, [rel D%u]\n", ci.arg1.var.addr_id);
					}
				} else if (ci.arg1.var.kind == VAR_STACK) {
					uint off = *OffTable_get(&stack_table, ci.arg1.var.addr_id);
					sb_appendf(&body, "    lea rax, [rbp - %u]\n", off);
				} else if (ci.arg1.var.kind == VAR_DATA) {
					sb_appendf(&body, "    lea rax, [rel D%u]\n", ci.arg1.var.addr_id);
				}

				sprintf(dst, "%s", opr_to_nasm(ci.dst));
				sb_appendf(&body, "    mov %s, rax\n", dst);
			} break;

			case OP_JUMP_IF_NOT: {
				reg_alloc(ci, arg1, arg2);
				sb_appendf(&body, "    mov %s, %s\n", arg1, opr_to_nasm(ci.arg1));
				sb_appendf(&body, "    cmp %s, 0\n", arg1);
				sb_appendf(&body, "    je %s\n", opr_to_nasm(ci.dst));
			} break;

			case OP_LABEL: {
				sb_appendf(&body, "%s:\n", opr_to_nasm(ci.arg1));
			} break;

			case OP_JUMP: {
				sb_appendf(&body, "    jmp %s\n", opr_to_nasm(ci.dst));
			} break;

			case OP_RETURN: {
				if (ci.arg1.kind != OPR_NULL) {
					switch (func.ret_type.kind) {
						case TYPE_STRUCT:
						case TYPE_ARRAY:
							assert(!"error: returning arrays/structs isn't supported yet\n");
						default: {
							uint reg_row = get_reg_row(func.ret_type);
							sb_appendf(&body, "    mov %s, %s\n", RAX[reg_row], opr_to_nasm(ci.arg1));
						}
					}
				}

				sb_appendf(&body, "    mov rsp, rbp\n");
				sb_appendf(&body, "    pop rbp\n");
				sb_appendf(&body, "    ret\n");
			} break;

			case OP_FUNC_CALL: {
				sb_appendf(&body, "    sub rsp, 32\n");

				for (size_t i = 0; ci.args[i].kind != OPR_NULL; i++) {
					char ts[32]; opr_type_to_stack(ci.args[i], ts);
					size_t arg_row = get_reg_row(tac_ir_get_opr_type(ci.args[i]));

					switch (tp) {
						case TP_NULL: break;
						case TP_MACOS:
						case TP_LINUX: {
							if (i >= sysv_regs_cnt) {
								sb_appendf(&body, "    mov %s, %s\n", R10[arg_row], opr_to_nasm(ci.args[i]));
								uint shadow_space = (i - sysv_regs_cnt) * 8 + 32;
								sb_appendf(&body, "    mov %s [rsp + %u], %s\n", ts, shadow_space, R10[arg_row]);
							} else {
								sb_appendf(&body, "    mov %s, %s\n", sysv_regs[arg_row][i], opr_to_nasm(ci.args[i]));
							}
						} break;
						case TP_WINDOWS: {
							if (i >= sysv_regs_cnt) {
								sb_appendf(&body, "    mov %s, %s\n", R10[arg_row], opr_to_nasm(ci.args[i]));
								uint shadow_space = (i - win_regs_cnt) * 8 + 32;
								sb_appendf(&body, "    mov %s [rsp + %u], %s\n", ts, shadow_space, R10[arg_row]);
							} else {
								sb_appendf(&body, "    mov %s, %s\n", win_regs[arg_row][i], opr_to_nasm(ci.args[i]));
							}
						} break;
					}
				}

				sb_appendf(&body, "    call %s%s\n", (tp == TP_MACOS ? "_" : ""), ci.dst.name);
				sb_appendf(&body, "    add rsp, 32\n");
			} break;

			default: UNREACHABLE;
		}
	}

	total_offset += 48;
	align_up(&total_offset, 16);

	sb_appendf(code, "    sub rsp, %u\n", total_offset);
	sb_appendf(code, "%s", body.items);
	if (da_last(&func.body).op != OP_RETURN) {
		if (strcmp(func.name, "main") == 0)
			sb_appendf(code, "    mov eax, 0\n");
		sb_appendf(code, "    mov rsp, rbp\n");
		sb_appendf(code, "    pop rbp\n");
		sb_appendf(code, "    ret\n");
	}

	sb_appendf(code, "\n");
}

char *nasm_gen_prog(TAC_Program *prog, TargetPlatform tp) {
	StringBuilder code = {0};
	tp = tp;

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
				sb_appendf(&code, "    U%u db ", uniq_data_off);
				for (size_t i = 0; i < g->type.array.length; i++) {
					sb_appendf(&code, "%#x", g->data[i]);
					if (i != g->type.array.length - 1) sb_appendf(&code, ", ");
				}
				sb_appendf(&code, "\n");
			} else sb_appendf(&code, "    U%u times %u db 0\n", uniq_data_off, arr_size);
			sb_appendf(&code, "    align 8\n");
			sb_appendf(&code, "    D%u dq U%u\n", g->index, uniq_data_off++);
		} else sb_appendf(&code, "    D%u times %u db 0\n", g->index, get_type_size(g->type));
	}
	sb_appendf(&code, "    align 8\n");
	sb_appendf(&code, "\n");

	sb_appendf(&code, "section .text\n");
	for (size_t i = 0; i < prog->funcs.count; i++) {
		nasm_gen_func(&code, da_get(&prog->funcs, i));
	}

	return code.items;
}
