#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include <assert.h>

#include "../../include/platform.h"
#include "../../include/tac_ir.h"

typedef enum {
	RAX, RDX, RCX,
	RDI, RBX, RSI,

	R8,  R9,
	R10, R11, R12,
	R13, R14, R15,
} Register;

typedef enum {
	REG, MEM,
	IMM, LBL,
} OprKind;

const char *reg_forms[][4] = {
	[RAX] = {"al",   "ax",   "eax",  "rax"},
	[RDX] = {"dl",   "dx",   "edx",  "rdx"},
	[RCX] = {"cl",   "cx",   "ecx",  "rcx"},
	[RDI] = {"dil",  "di",   "edi",  "rdi"},
	[RBX] = {"bl",   "bx",   "ebx",  "rbx"},
	[RSI] = {"sil",  "si",   "esi",  "rsi"},
	[R8]  = {"r8b",  "r8w",  "r8d",  "r8"},
	[R9]  = {"r9b",  "r9w",  "r9d",  "r9"},
	[R10] = {"r10b", "r10w", "r10d", "r10"},
	[R11] = {"r11b", "r11w", "r11d", "r11"},
	[R12] = {"r12b", "r12w", "r12d", "r12"},
	[R13] = {"r13b", "r13w", "r13d", "r13"},
	[R14] = {"r14b", "r14w", "r14d", "r14"},
	[R15] = {"r15b", "r15w", "r15d", "r15"},
};

Register sysv_regs[] = {RDI, RSI, RDX, RCX, R8, R9};
Register win_regs[] = {RCX, RDX, R8, R9};

typedef struct {
	uint var_id;
	Register reg;
	uint size;
} RegOperand;

HT_DECL(OffTable, uint, uint)
HT_IMPL_NUM(OffTable, uint, uint)

HT_DECL(RegTable, uint, Register)
HT_IMPL_NUM(RegTable, uint, Register)

OffTable stack_table = {0};
OffTable data_table = {0};

static int opt_level;
static TargetPlatform tp;

// Function context
DA(Register) free_regs;
RegTable used_regs;
TAC_VarIntervals var_ints;
StringBuilder body = {0};
uint total_offset;
uint inst_idx;

size_t get_reg_size(Type t) {
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
	case TYPE_FUNCTION:
	case TYPE_ARRAY:
	case TYPE_POINTER:
	case TYPE_IPTR:
	case TYPE_UPTR:
	case TYPE_I64:
	case TYPE_U64:
		return 3;
	default:
		UNREACHABLE;
	}
}

uint get_type_size(Type type) {
	if (type.kind == TYPE_STRUCT) {
		uint total = 0;
		da_foreach (Member, member, &type.as.user->as.ustruct.members) {
			if (member->kind == MBR_FIELD) {
				total += get_type_size(member->as.field.type);
			}
		}
		return total;
	}
	return 1 << get_reg_size(type);
}

uint get_struct_offset(TAC_Operand var) {
	uint total = 0;
	if (var.as.var.fields.count == 0)
		return 0;

	for (size_t i = 0; i < var.as.var.fields.count; i++) {
		char *off = da_get(&var.as.var.fields, i);
		da_foreach (Member, member, &var.as.var.type.as.user->as.ustruct.members) {
			if (member->kind == MBR_FIELD) {
				total += get_type_size(member->as.field.type);
				if (strcmp(member->as.field.id, off) == 0) {
					var.as.var.type = member->as.field.type;
					total -= get_type_size(member->as.field.type);
					break;
				}
			}
		}
	}

	return total;
}

bool reg_allocator_push(uint var_id, Register *reg) {
	TAC_VarInterval vi = *TAC_VarIntervals_get(&var_ints, var_id);
	if (vi.to_spill || free_regs.count == 0)
		return false;

	*reg = da_last(&free_regs);
	free_regs.count--;
	RegTable_add(&used_regs, var_id, *reg);
	return true;
}

void reg_allocator_pop(uint var_id) {
	Register reg = *RegTable_get(&used_regs, var_id);
	RegTable_remove(&used_regs, var_id);
	da_append(&free_regs, reg);
}

void reg_allocator_free() {
	static DA(uint) to_remove = {0};
	da_reset(&to_remove);

	ht_foreach_node (RegTable, n, &used_regs) {
		TAC_VarInterval vi = *TAC_VarIntervals_get(&var_ints, n->key);
		if (vi.end < inst_idx) da_append(&to_remove, n->key);
	}

	da_foreach (uint, var_id, &to_remove) {
		reg_allocator_pop(*var_id);
	}
}

void opr_type_to_stack(TAC_Operand t, char *buf) {
	switch (tac_ir_get_opr_type(t).kind) {
	case TYPE_STRUCT:
		sprintf(buf, "");
		break;
	default:;
		uint reg_size = get_reg_size(tac_ir_get_opr_type(t));
		sprintf(buf, "%s", (char*[]){"byte", "word", "dword", "qword"}[reg_size]);
	}
}

char *opr_to_nasm(TAC_Operand opr, OprKind *opr_kind) {
	static char rbuf[64];
	switch (opr.kind) {
	case OPR_SIZEOF: {
		uint size = get_type_size(opr.as.size_of.vtype);
		if (opr.as.size_of.vtype.kind == TYPE_ARRAY) {
			uint elemSize = get_type_size(*opr.as.size_of.vtype.as.array.elem);
			size = elemSize * opr.as.size_of.vtype.as.array.length;
		}
		if (opr_kind) *opr_kind = IMM;
		sprintf(rbuf, "%u", size);
	} break;

	case OPR_LABEL: {
		if (opr_kind) *opr_kind = LBL;
		sprintf(rbuf, ".L%u", opr.as.label_id);
	} break;

	case OPR_VAR: {
		uint field_off = get_struct_offset(opr);
		char ts[32]; opr_type_to_stack(opr, ts);
		if (opr_kind) *opr_kind = MEM;

		if (opr.as.var.kind == VAR_STACK) {
			uint *off = OffTable_get(&stack_table, opr.as.var.addr_id);
			if (off) {
				sprintf(rbuf, "%s[rbp - %u]", ts, *off - field_off);
			} else {
				size_t row = get_reg_size(opr.as.var.type);
				Register reg = *RegTable_get(&used_regs, opr.as.var.addr_id);
				sprintf(rbuf, "%s", reg_forms[reg][row]);
				if (opr_kind) *opr_kind = REG;
			}
		} else if (opr.as.var.kind == VAR_ADDR) {
			if (opr.as.var.addr_kind == VAR_STACK) {
				uint *off = OffTable_get(&stack_table, opr.as.var.addr_id);
				if (off) {
					sb_appendf(&body, "  mov rax, qword[rbp - %u]\n", *off);
				} else {
					Register reg = *RegTable_get(&used_regs, opr.as.var.addr_id);
					sb_appendf(&body, "  mov rax, %s\n", reg_forms[reg][3]);
				}
				if (field_off) sprintf(rbuf, "%s[rax + %u]", ts, field_off);
				else           sprintf(rbuf, "%s[rax]", ts);
			} else if (opr.as.var.addr_kind == VAR_DATA) {
				if (field_off) sprintf(rbuf, "%s[rel D%u + %lu]", ts, opr.as.var.addr_id, field_off);
				else           sprintf(rbuf, "%s[rel D%u]", ts, opr.as.var.addr_id);
			} else UNREACHABLE;
		} else if (opr.as.var.kind == VAR_DATA) {
			if (field_off) sprintf(rbuf, "%s[rel D%u + %u]", ts, opr.as.var.addr_id, field_off);
			else           sprintf(rbuf, "%s[rel D%u]", ts, opr.as.var.addr_id);
		}
	} break;

	case OPR_LITERAL: {
		if (opr_kind) *opr_kind = IMM;
		switch (opr.as.literal.type.kind) {
		case TYPE_FLOAT:
		case TYPE_F32: {
			float x = (float)opr.as.literal.as.lfloat;
			uint32_t bits;
			memcpy(&bits, &x, 4);
			sb_appendf(&body, "  mov r10d, 0x%08X\n", bits);
			sb_appendf(&body, "  movd xmm0, r10d\n", bits);
			sprintf(rbuf, "xmm0");
		} break;
		case TYPE_I32:
		case TYPE_INT:
			sprintf(rbuf, "%d", (int) opr.as.literal.as.lint);
			break;
		case TYPE_U32:
		case TYPE_UINT:
			sprintf(rbuf, "%u", (uint) opr.as.literal.as.lint);
			break;
		case TYPE_BOOL:
		case TYPE_I8:
			sprintf(rbuf, "%d", (i8) opr.as.literal.as.lint);
			break;
		case TYPE_U8:
			sprintf(rbuf, "%d", (u8) opr.as.literal.as.lint);
			break;
		case TYPE_I16:
			sprintf(rbuf, "%hd", (i16) opr.as.literal.as.lint);
			break;
		case TYPE_U16:
			sprintf(rbuf, "%hu", (u16) opr.as.literal.as.lint);
			break;
		case TYPE_UPTR:
		case TYPE_U64:
			sprintf(rbuf, "%llu", opr.as.literal.as.lint);
			break;
		case TYPE_ARRAY:
		case TYPE_POINTER:
		case TYPE_IPTR:
		case TYPE_I64:
			sprintf(rbuf, "%lli", opr.as.literal.as.lint);
			break;
		default:
			UNREACHABLE;
		}
	} break;

	case OPR_FUNC_RET: {
		if (opr_kind) *opr_kind = REG;
		switch (opr.as.func_ret.type.kind) {
		case TYPE_ARRAY:
		case TYPE_STRUCT:
			assert(!"error: passing arrays or structs isn't supported yet\n");
		default:;
			uint reg_size = get_reg_size(opr.as.func_ret.type);
			sprintf(rbuf, "%s", reg_forms[RAX][reg_size]);
		}
	} break;

	case OPR_FUNC_INP: {
		char ts[32]; opr_type_to_stack(opr, ts);
		uint arg_id = opr.as.func_inp.arg_id;
		size_t arg_size = get_reg_size(opr.as.func_inp.type);
		if (opr_kind) *opr_kind = REG;

		switch (tp) {
		case TP_MACOS:
		case TP_LINUX:
			if (arg_id >= ARR_LEN(sysv_regs)) {
				uint shadow_space = (arg_id - ARR_LEN(sysv_regs)) * 8 + 48;
				sb_appendf(&body, "  mov %s, %s[rbp + %u]\n", reg_forms[R10][arg_size], ts, shadow_space);
				sprintf(rbuf, "%s", reg_forms[R10][arg_size]);
			} else {
				sprintf(rbuf, "%s", reg_forms[sysv_regs[arg_id]][arg_size]);
			} break;
		case TP_WINDOWS:
			if (arg_id >= ARR_LEN(win_regs)) {
				uint shadow_space = (arg_id - ARR_LEN(win_regs)) * 8 + 48;
				sb_appendf(&body, "  mov %s, %s[rbp + %u]\n", reg_forms[R10][arg_size], ts, shadow_space);
				sprintf(rbuf, "%s", reg_forms[R10][arg_size]);
			} else {
				sprintf(rbuf, "%s", reg_forms[win_regs[arg_id]][arg_size]);
			}
		}
	} break;

	default:
		UNREACHABLE;
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
	default:
		sprintf(arg1, "%s", reg_forms[R10][get_reg_size(opr_type)]);
		sprintf(arg2, "%s", reg_forms[R11][get_reg_size(opr_type)]);
	}
}

void load_reserved_regs(TAC_Instruction inst, char *arg1, char *arg2) {
	if (inst.dst.kind == OPR_LABEL) {
		inst.dst.kind = OPR_VAR;
		inst.dst.as.var.type = (Type){.kind = TYPE_BOOL};
		type_to_reg(inst.dst, arg1, arg2);
		return;
	}

	if (inst.dst.as.var.type.kind == TYPE_BOOL) {
		type_to_reg(inst.args[0], arg1, arg2);
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

void nasm_gen_new_var(TAC_Instruction ci, char *dst, OprKind *opr_kind) {
	if (opt_level > 0) {
		reg_allocator_free();
		if (ci.dst.as.var.type.kind != TYPE_STRUCT) {
			Register reg;
			if (reg_allocator_push(ci.dst.as.var.addr_id, &reg)) {
				if (opr_kind) *opr_kind = REG;
				size_t row = get_reg_size(ci.dst.as.var.type);
				sprintf(dst, "%s", reg_forms[reg][row]);
				return;
			}
		}
	}

	if (opr_kind) *opr_kind = MEM;
	char ts[32]; opr_type_to_stack(ci.dst, ts);
	total_offset_add(get_type_size(ci.dst.as.var.type));
	OffTable_add(&stack_table, ci.dst.as.var.addr_id, total_offset);
	sprintf(dst, "%s[rbp - %u]", ts, total_offset);
}

void nasm_gen_func(StringBuilder *code, TAC_Func func) {
	if (func.body.count == 0) return;

	RegTable_free(&used_regs);
	used_regs = (RegTable){0};
	var_ints = func.var_ints;
	da_reset(&free_regs);
	da_append(&free_regs, RBX);
	da_append(&free_regs, R12);
	da_append(&free_regs, R13);
	da_append(&free_regs, R14);
	da_append(&free_regs, R15);

	if (!func.is_static)
		sb_appendf(code, "global %s\n", func.name);
	sb_appendf(code, "%s%s:\n", (tp == TP_MACOS ? "_" : ""), func.name);

	sb_reset(&body);
	total_offset = 0;

	for (size_t i = 0; i < func.body.count; i++) {
		char arg1[64], arg2[64], dst[64];
		TAC_Instruction ci = da_get(&func.body, i);
		inst_idx = i;

#ifdef _CP_RUNTIME_CHECKS
		char res[256];
		tac_ir_dump_inst(ci, res);
		printf("%s\n", res);
		sb_appendf(&body, ";%s\n", res);
#endif

		switch (ci.op) {
		case OP_LESS_EQ: case OP_GREAT_EQ:
		case OP_GREAT:   case OP_LESS:
		case OP_EQ:      case OP_NOT_EQ: {
			nasm_gen_new_var(ci, dst, NULL);
			load_reserved_regs(ci, arg1, arg2);

			OprKind opr1_kind, opr2_kind;
			char opr1[64], opr2[64];

			sprintf(opr1, opr_to_nasm(ci.args[0], &opr1_kind));
			if (opr1_kind != REG) {
				sb_appendf(&body, "  mov %s, %s\n", arg1, opr1);
			} else sprintf(arg1, opr1);

			sprintf(opr2, opr_to_nasm(ci.args[1], &opr2_kind));
			if (opr2_kind != REG) {
				sb_appendf(&body, "  mov %s, %s\n", arg2, opr2);
			} else sprintf(arg2, opr2);

			if (ci.op == OP_EQ) {
				sb_appendf(&body, "  cmp %s, %s\n", arg1, arg2);
				sb_appendf(&body, "  sete al\n", arg1);
			} else if (ci.op == OP_NOT_EQ) {
				sb_appendf(&body, "  cmp %s, %s\n", arg1, arg2);
				sb_appendf(&body, "  setne al\n");
			} else if (ci.op == OP_GREAT) {
				sb_appendf(&body, "  cmp %s, %s\n", arg1, arg2);
				sb_appendf(&body, "  setg al\n");
			} else if (ci.op == OP_LESS) {
				sb_appendf(&body, "  cmp %s, %s\n", arg1, arg2);
				sb_appendf(&body, "  setl al\n");
			} else if (ci.op == OP_GREAT_EQ) {
				sb_appendf(&body, "  cmp %s, %s\n", arg1, arg2);
				sb_appendf(&body, "  setge al\n");
			} else if (ci.op == OP_LESS_EQ) {
				sb_appendf(&body, "  cmp %s, %s\n", arg1, arg2);
				sb_appendf(&body, "  setle al\n");
			}

			sb_appendf(&body, "  mov %s, al\n", dst);
		} break;

		case OP_ADD:    case OP_SUB:
		case OP_MUL:    case OP_DIV:
		case OP_AND:    case OP_OR:
		case OP_BW_AND: case OP_BW_OR:
		case OP_BW_LS:  case OP_BW_RS:
		case OP_BW_XOR: case OP_MOD: {
			OprKind opr1_kind, opr2_kind, dst_kind;
			char opr1[64], opr2[64];
			nasm_gen_new_var(ci, dst, &dst_kind);
			load_reserved_regs(ci, arg1, arg2);

			sprintf(opr1, opr_to_nasm(ci.args[0], &opr1_kind));
			if (dst_kind != REG) {
				sb_appendf(&body, "  mov %s, %s\n", arg1, opr1);
			} else sprintf(arg1, opr1);

			bool is_div = ci.op == OP_DIV || ci.op == OP_MOD;
			sprintf(opr2, opr_to_nasm(ci.args[1], &opr2_kind));
			if (is_div || (opr2_kind != REG && dst_kind != REG)) {
				sb_appendf(&body, "  mov %s, %s\n", arg2, opr2);
			} else sprintf(arg2, opr2);

			if (dst_kind == REG) {
				sprintf(arg1, dst);
				sb_appendf(&body, "  mov %s, %s\n", dst, opr1);
			}

			if      (ci.op == OP_ADD)    sb_appendf(&body, "  add %s, %s\n", arg1, arg2);
			else if (ci.op == OP_SUB)    sb_appendf(&body, "  sub %s, %s\n", arg1, arg2);
			else if (ci.op == OP_BW_AND) sb_appendf(&body, "  and %s, %s\n", arg1, arg2);
			else if (ci.op == OP_BW_OR)  sb_appendf(&body, "  or  %s, %s\n", arg1, arg2);
			else if (ci.op == OP_BW_XOR) sb_appendf(&body, "  xor %s, %s\n", arg1, arg2);

			else if (ci.op == OP_BW_LS || ci.op == OP_BW_RS) {
				const char *rcx = reg_forms[RCX][get_reg_size(ci.dst.as.var.type)];
				sb_appendf(&body, "  mov %s, %s\n", rcx, arg2);
				sb_appendf(&body, "  %s %s, cl\n", ci.op == OP_BW_LS ? "shl" : "shr", arg1);
			}

			else if (ci.op == OP_MUL) {
				switch (ci.dst.as.var.type.kind) {
				case TYPE_ARRAY:
				case TYPE_POINTER:
				case TYPE_UINT: case TYPE_U8:
				case TYPE_U32:  case TYPE_U16:
				case TYPE_U64:  case TYPE_UPTR:
					sb_appendf(&body, "  mul %s, %s\n", arg1, arg2);
					break;
				case TYPE_IPTR:
				case TYPE_BOOL: case TYPE_I8:
				case TYPE_INT:  case TYPE_I32:
				case TYPE_I64:  case TYPE_I16:
					sb_appendf(&body, "  imul %s, %s\n", arg1, arg2);
					break;
				default:
					UNREACHABLE;
				}
			}

			else if (ci.op == OP_DIV || ci.op == OP_MOD) {
				char *SEI[] = {"cbw", "cwd", "cdq", "cqo"};
				uint reg_size = get_reg_size(ci.dst.as.var.type);
				sb_appendf(&body, "  mov %s, %s\n", reg_forms[RAX][reg_size], arg1);

				switch (ci.dst.as.var.type.kind) {
				case TYPE_ARRAY:
				case TYPE_POINTER:
				case TYPE_UINT: case TYPE_U8:
				case TYPE_U32:  case TYPE_U16:
				case TYPE_U64:  case TYPE_UPTR:
					sb_appendf(&body, "  xor rdx, rdx\n");
					sb_appendf(&body, "  div %s\n", arg2);
					break;
				case TYPE_IPTR:
				case TYPE_BOOL: case TYPE_I8:
				case TYPE_INT:  case TYPE_I32:
				case TYPE_I64:  case TYPE_I16:
					sb_appendf(&body, "  %s\n", SEI[reg_size]);
					sb_appendf(&body, "  idiv %s\n", arg2);
					break;
				default:
					UNREACHABLE;
				}

				if (ci.op == OP_DIV) sprintf(arg1, "%s", reg_forms[RAX][reg_size]);
				else                 sprintf(arg1, "%s", reg_forms[RDX][reg_size]);
			}

			else if (ci.op == OP_AND) {
				sb_appendf(&body, "  and %s, %s\n", arg1, arg2);
			} else if (ci.op == OP_OR) {
				sb_appendf(&body, "  or %s, %s\n", arg1, arg2);
			}

			if (dst_kind != REG || is_div) {
				sb_appendf(&body, "  mov %s, %s\n", dst, arg1);
			}
		} break;

		case OP_BW_NOT:
		case OP_NOT: case OP_NEG: {
			nasm_gen_new_var(ci, dst, NULL);
			load_reserved_regs(ci, arg1, arg2);
			sb_appendf(&body, "  mov %s, %s\n", arg1, opr_to_nasm(ci.args[0], NULL));

			if      (ci.op == OP_NEG)    sb_appendf(&body, "  neg %s\n", arg1);
			else if (ci.op == OP_BW_NOT) sb_appendf(&body, "  not %s\n", arg1);
			else if (ci.op == OP_NOT) {
				sb_appendf(&body, "  test %s, %s\n", arg1, arg1);
				sb_appendf(&body, "  setz al\n");
				sprintf(arg1, "al");
			}

			sb_appendf(&body, "  mov %s, %s\n", dst, arg1);
		} break;

		case OP_CAST: {
			Type dst_type = ci.dst.as.var.type;
			Type arg1_type; switch (ci.args[0].kind) {
				case OPR_LITERAL: arg1_type = ci.args[0].as.literal.type; break;
				case OPR_VAR:     arg1_type = ci.args[0].as.var.type;     break;
				case OPR_SIZEOF:  arg1_type = ci.args[0].as.size_of.type; break;
				default: UNREACHABLE;
			}

			nasm_gen_new_var(ci, dst, NULL);
			load_reserved_regs(ci, arg1, arg2);

			if (dst_type.kind == arg1_type.kind) { UNREACHABLE; }

			int dsz = 0;
			int ssz = 0;
			bool ssig = false;

			switch (dst_type.kind) {
				case TYPE_I8:  dsz = 1; break;
				case TYPE_U8:  dsz = 1; break;
				case TYPE_I16: dsz = 2; break;
				case TYPE_U16: dsz = 2; break;
				case TYPE_U64:
				case TYPE_UPTR: case TYPE_POINTER: dsz = 8; break;
				case TYPE_INT:  case TYPE_I32:     dsz = 4; break;
				case TYPE_UINT: case TYPE_U32:     dsz = 4; break;
				case TYPE_I64:  case TYPE_IPTR:    dsz = 8; break;
				default: UNREACHABLE;
			}

			switch (arg1_type.kind) {
				case TYPE_I8:  ssz = 1; ssig = true;  break;
				case TYPE_U8:  ssz = 1; ssig = false; break;
				case TYPE_I16: ssz = 2; ssig = true;  break;
				case TYPE_U16: ssz = 2; ssig = false; break;
				case TYPE_U64:
				case TYPE_UPTR: case TYPE_POINTER: ssz = 8; ssig = false; break;
				case TYPE_INT:  case TYPE_I32:     ssz = 4; ssig = true;  break;
				case TYPE_UINT: case TYPE_U32:     ssz = 4; ssig = false; break;
				case TYPE_I64:  case TYPE_IPTR:    ssz = 8; ssig = true;  break;
				default: UNREACHABLE;
			}

			const char *ext_inst = ssig ? "movsx" : "movzx";
			const char *dst_reg = NULL;
			const char *src_reg = NULL;
			const char *low_reg = NULL;

			switch (dsz) {
				case 1: dst_reg = "al";  low_reg = "al";  break;
				case 2: dst_reg = "ax";  low_reg = "ax";  break;
				case 4: dst_reg = "eax"; low_reg = "eax"; break;
				case 8: dst_reg = "rax"; low_reg = "eax"; break;
				default: UNREACHABLE;
			}

			switch (ssz) {
				case 1: src_reg = "al";  break;
				case 2: src_reg = "ax";  break;
				case 4: src_reg = "eax"; break;
				case 8: src_reg = "rax"; break;
				default: UNREACHABLE;
			}

			if (dsz > ssz) {
				bool is_32_to_64_signed = (ssz == 4 && dsz == 8 && ssig);
				if (is_32_to_64_signed)
					ext_inst = "movsxd";
				sb_appendf(&body, "  %s %s, %s\n", ext_inst, dst_reg, opr_to_nasm(ci.args[0], NULL));
				sb_appendf(&body, "  mov %s, %s\n", opr_to_nasm(ci.dst, NULL), dst_reg);
			} else if (dsz < ssz) {
				sb_appendf(&body, "  mov %s, %s\n", src_reg, opr_to_nasm(ci.args[0], NULL));
				sb_appendf(&body, "  mov %s, %s\n", opr_to_nasm(ci.dst, NULL), low_reg);
			} else {
				sb_appendf(&body, "  mov %s, %s\n", dst_reg, opr_to_nasm(ci.args[0], NULL));
				sb_appendf(&body, "  mov %s, %s\n", opr_to_nasm(ci.dst, NULL), dst_reg);
			}
		} break;

		case OP_ASSIGN: {
			bool fst_asg = false;
			if (ci.dst.as.var.kind == VAR_STACK) {
				uint *off = OffTable_get(&stack_table, ci.dst.as.var.addr_id);
				Register *reg = RegTable_get(&used_regs, ci.dst.as.var.addr_id);
				if (!off && !reg) {
					fst_asg = true;
					nasm_gen_new_var(ci, dst, NULL);
				}
			}

			if (ci.dst.as.var.type.kind == TYPE_ARRAY && fst_asg) {
				load_reserved_regs(ci, arg1, arg2);
				total_offset_add(
					get_type_size(*ci.dst.as.var.type.as.array.elem) *
					ci.dst.as.var.type.as.array.length);

				sb_appendf(&body, "  lea %s, [rbp - %u]\n", arg1, total_offset);
				sb_appendf(&body, "  mov %s, %s\n", opr_to_nasm(ci.dst, NULL), arg1);
			}

			if (ci.args[0].kind != OPR_NULL) {
				if (tac_ir_get_opr_type(ci.dst).kind == TYPE_STRUCT) {
					sb_appendf(&body, "  lea rsi, %s\n", opr_to_nasm(ci.args[0], NULL));
					sb_appendf(&body, "  lea rdi, %s\n", opr_to_nasm(ci.dst, NULL));
					sb_appendf(&body, "  mov rcx, %u\n", get_type_size(tac_ir_get_opr_type(ci.dst)));
					sb_appendf(&body, "  cld\n");
					sb_appendf(&body, "  rep movsb\n");
				} else {
					load_reserved_regs(ci, arg1, arg2);
					OprKind dst_kind;
					sprintf(dst, "%s", opr_to_nasm(ci.dst, &dst_kind));
					if (dst_kind != REG) {
						sb_appendf(&body, "  mov %s, %s\n", arg2, opr_to_nasm(ci.args[0], NULL));
						sb_appendf(&body, "  mov %s, %s\n", dst, arg2);
					} else {
						sb_appendf(&body, "  mov %s, %s\n", dst, opr_to_nasm(ci.args[0], NULL));
					}
				}
			} else {
				if (tac_ir_get_opr_type(ci.dst).kind == TYPE_STRUCT) {
					sb_appendf(&body, "  xor rax, rax\n");
					sb_appendf(&body, "  lea rdi, %s\n", opr_to_nasm(ci.dst, NULL));
					sb_appendf(&body, "  mov rcx, %u\n", get_type_size(tac_ir_get_opr_type(ci.dst)));
					sb_appendf(&body, "  rep stosb\n");
				}
			}
		} break;

		case OP_DEREF: {
			nasm_gen_new_var(ci, dst, NULL);
			char ts[32]; opr_type_to_stack(ci.dst, ts);

			if (ci.dst.as.var.type.kind != TYPE_STRUCT) {
				load_reserved_regs(ci, arg1, arg2);
				sb_appendf(&body, "  mov rax, %s\n",     opr_to_nasm(ci.args[0], NULL));
				sb_appendf(&body, "  mov %s, %s[rax]\n", arg1, ts);
				sb_appendf(&body, "  mov %s, %s\n", opr_to_nasm(ci.dst, NULL), arg1);
			} else {
				sb_appendf(&body, "  mov rsi, %s\n", opr_to_nasm(ci.args[0], NULL));
				sb_appendf(&body, "  lea rdi, %s\n", opr_to_nasm(ci.dst, NULL));
				sb_appendf(&body, "  mov rcx, %u\n", get_type_size(tac_ir_get_opr_type(ci.dst)));
				sb_appendf(&body, "  cld\n");
				sb_appendf(&body, "  rep movsb\n");
			}
		} break;

		case OP_REF: {
			nasm_gen_new_var(ci, dst, NULL);
			size_t field_off = get_struct_offset(ci.args[0]);

			if (ci.args[0].as.var.kind == VAR_ADDR) {
				if (ci.args[0].as.var.addr_kind == VAR_STACK) {
					uint off = *OffTable_get(&stack_table, ci.args[0].as.var.addr_id);
					sb_appendf(&body, "  mov rax, [rbp - %u]\n", off);
					sb_appendf(&body, "  add rax, %zu\n", field_off);
				} else if (ci.args[0].as.var.addr_kind == VAR_DATA) {
					sb_appendf(&body, "  lea rax, [rel D%u]\n", ci.args[0].as.var.addr_id);
					sb_appendf(&body, "  add rax, %zu\n", field_off);
				}
			} else if (ci.args[0].as.var.kind == VAR_STACK) {
				uint off = *OffTable_get(&stack_table, ci.args[0].as.var.addr_id);
				sb_appendf(&body, "  lea rax, [rbp - %u]\n", off - field_off);
			} else if (ci.args[0].as.var.kind == VAR_DATA) {
				sb_appendf(&body, "  lea rax, [rel D%u]\n", ci.args[0].as.var.addr_id);
				sb_appendf(&body, "  add rax, %zu\n", field_off);
			}

			sb_appendf(&body, "  mov %s, rax\n", opr_to_nasm(ci.dst, NULL));
		} break;

		case OP_JUMP_IF_NOT: {
			load_reserved_regs(ci, arg1, arg2);
			sb_appendf(&body, "  mov %s, %s\n", arg1, opr_to_nasm(ci.args[0], NULL));
			sb_appendf(&body, "  cmp %s, 0\n", arg1);
			sb_appendf(&body, "  je %s\n", opr_to_nasm(ci.dst, NULL));
		} break;

		case OP_LABEL: {
			sb_appendf(&body, "%s:\n", opr_to_nasm(ci.args[0], NULL));
		} break;

		case OP_JUMP: {
			sb_appendf(&body, "  jmp %s\n", opr_to_nasm(ci.dst, NULL));
		} break;

		case OP_RETURN: {
			if (ci.args[0].kind != OPR_NULL) {
				switch (func.ret_type.kind) {
				case TYPE_STRUCT:
				case TYPE_ARRAY:
					assert(!"error: returning arrays/structs isn't supported yet\n");
				default:;
					uint reg_size = get_reg_size(func.ret_type);
					sb_appendf(&body, "  mov %s, %s\n", reg_forms[RAX][reg_size], opr_to_nasm(ci.args[0], NULL));
				}
			}

			sb_appendf(&body, "  mov rsp, rbp\n");
			sb_appendf(&body, "  pop rbp\n");
			sb_appendf(&body, "  ret\n");
		} break;

		case OP_FUNC_CALL: {
			StringBuilder fc = {0};
			bool shadow_space = false;

			for (size_t i = 0; ci.args[i].kind != OPR_NULL; i++) {
				char ts[32]; opr_type_to_stack(ci.args[i], ts);
				size_t arg_size = get_reg_size(tac_ir_get_opr_type(ci.args[i]));

				switch (tp) {
				case TP_MACOS:
				case TP_LINUX:
					if (i >= ARR_LEN(sysv_regs)) {
						shadow_space = true;
						sb_appendf(&fc, "  mov %s, %s\n", reg_forms[R10][arg_size], opr_to_nasm(ci.args[i], NULL));
						uint shadow_space = (i - ARR_LEN(sysv_regs)) * 8 + 32;
						sb_appendf(&fc, "  mov %s[rsp + %u], %s\n", ts, shadow_space, reg_forms[R10][arg_size]);
					} else {
						sb_appendf(&fc, "  mov %s, %s\n", reg_forms[sysv_regs[i]][arg_size], opr_to_nasm(ci.args[i], NULL));
					} break;
				case TP_WINDOWS:
					if (i >= ARR_LEN(win_regs)) {
						shadow_space = true;
						sb_appendf(&fc, "  mov %s, %s\n", reg_forms[R10][arg_size], opr_to_nasm(ci.args[i], NULL));
						uint shadow_space = (i - ARR_LEN(win_regs)) * 8 + 32;
						sb_appendf(&fc, "  mov %s[rsp + %u], %s\n", ts, shadow_space, reg_forms[R10][arg_size]);
					} else {
						sb_appendf(&fc, "  mov %s, %s\n", reg_forms[win_regs[i]][arg_size], opr_to_nasm(ci.args[i], NULL));
					}
				}
			}

			if (shadow_space) sb_appendf(&body, "  sub rsp, 32\n");
			sb_appendf(&fc, "  call %s%s\n", (tp == TP_MACOS ? "_" : ""), ci.dst.as.name);
			sb_appendf(&body, fc.items);
			if (shadow_space) sb_appendf(&body, "  add rsp, 32\n");
			sb_free(&fc);
		} break;

		default:
			UNREACHABLE;
		}
	}

	bool is_stack_used = total_offset != 0;
	total_offset += 48;
	align_up(&total_offset, 16);

	sb_appendf(code, "  push rbp\n");
	sb_appendf(code, "  mov rbp, rsp\n");
	sb_appendf(code, "  sub rsp, %u\n", total_offset);

	sb_appendf(code, "%s", body.items);

	if (da_last(&func.body).op != OP_RETURN) {
		if (strcmp(func.name, "main") == 0)
			sb_appendf(code, "  mov eax, 0\n");
		sb_appendf(code, "  mov rsp, rbp\n");
		sb_appendf(code, "  pop rbp\n");
		sb_appendf(code, "  ret\n");
	}

	sb_appendf(code, "\n");
}

char *nasm_gen_prog(TAC_Program *prog, TargetPlatform _tp, int _opt_level) {
	StringBuilder code = {0};
	opt_level = _opt_level;
	tp = _tp;

	da_foreach(TAC_Extern, ext, &prog->externs)
		sb_appendf(&code, "extern %s\n", ext->name);
	sb_appendf(&code, "\n");

	sb_appendf(&code, "section .data\n");
	uint uniq_data_off = 0;

	da_foreach (TAC_GlobalVar, g, &prog->globals) {
		if (g->type.kind == TYPE_ARRAY && g->is_none) {
			uint arr_size = get_type_size(*g->type.as.array.elem) * g->type.as.array.length;
			sb_appendf(&code, "  U%u times %u db 0\n", uniq_data_off, arr_size);
			sb_appendf(&code, "  align 8\n");
			sb_appendf(&code, "  D%u dq U%u\n", g->index, uniq_data_off++);
		} else {
			if (g->is_none) {
				sb_appendf(&code, "  D%u times %u db 0\n", g->index, get_type_size(g->type));
			} else {
				if (g->data.kind == LIT_ARR) {
					sb_appendf(&code, "  U%u db ", uniq_data_off);

					size_t lit_size = g->data.as.array.count;
					for (size_t i = 0; i < lit_size; i++) {
						size_t type_size = get_type_size(g->data.as.array.items[i].type);
						for (size_t j = 0; j < type_size; j++) {
							sb_appendf(&code, "%#x", (u8) g->data.as.array.items[i].as.bytes[j]);
							if (j != type_size - 1) sb_appendf(&code, ", ");
						}
						if (i != lit_size - 1) sb_appendf(&code, ", ");
					}

					sb_appendf(&code, "\n");
					sb_appendf(&code, "  align 8\n");
					sb_appendf(&code, "  D%u dq U%u\n", g->index, uniq_data_off++);
				} else if (g->data.kind == LIT_STR) {
					sb_appendf(&code, "  U%u db ", uniq_data_off);

					size_t lit_size = strlen(g->data.as.str) + 1;
					for (size_t i = 0; i < lit_size; i++) {
						sb_appendf(&code, "%#x", (u8) g->data.as.str[i]);
						if (i != lit_size - 1) sb_appendf(&code, ", ");
					}

					sb_appendf(&code, "\n");
					sb_appendf(&code, "  align 8\n");
					sb_appendf(&code, "  D%u dq U%u\n", g->index, uniq_data_off++);
				} else {
					sb_appendf(&code,
						"  D%u %s ", g->index,
						(char*[]){"db", "dw", "dd", "dq"}
						[get_reg_size(g->type)]);

					switch (g->data.kind) {
					case LIT_INT:   sb_appendf(&code, "%lli", g->data.as.lint);   break;
					case LIT_FLOAT: sb_appendf(&code, "%lf",  g->data.as.lfloat); break;
					case LIT_BOOL:  sb_appendf(&code, "%d",   g->data.as.lbool);  break;
					case LIT_CHAR:  sb_appendf(&code, "%lli", g->data.as.lint);   break;
					default: UNREACHABLE; }

					sb_appendf(&code, "\n", g->index);
				}
			}
		}
	}
	sb_appendf(&code, "  align 8\n");
	sb_appendf(&code, "\n");

	sb_appendf(&code, "section .text\n");
	for (size_t i = 0; i < prog->funcs.count; i++) {
		nasm_gen_func(&code, da_get(&prog->funcs, i));
	}

	return code.items;
}
