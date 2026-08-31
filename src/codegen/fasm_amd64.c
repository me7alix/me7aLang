#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include <assert.h>
#include <platform.h>
#include <tac_ir.h>

#include "amd64.h"
#include "reg_allocator.h"

HT_DECL(OffTable, uint, uint)
static OffTable stack_table = {0};
static int opt_level;
static TargetPlatform tp;

// Function context
static RegAllocator regal = {0};
static StringBuilder body = {0};
static bool is_there_return;
static uint stack_offset;
static uint inst_idx;

static void opr_type_to_stack(TAC_Operand t, char *buf) {
	static char *types[] = {"byte", "word", "dword", "qword"};
	switch (tac_ir_get_opr_type(t).kind) {
	case TYPE_STRUCT:
		sprintf(buf, "");
		break;
	default:;
		uint reg_size = get_reg_size(tac_ir_get_opr_type(t));
		sprintf(buf, "%s", types[reg_size]);
	}
}

typedef enum { REG, MEM, IMM, LBL } OprKind;
char *opr_to_fasm(TAC_Operand opr, OprKind *opr_kind) {
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
		uint fo = get_struct_offset(opr);
		char ts[32]; opr_type_to_stack(opr, ts);
		if (opr_kind) *opr_kind = MEM;
		if (opr.as.var.kind == VAR_LOCAL) {
			uint *off = OffTable_get(&stack_table, opr.as.var.addr_id);
			if (off) {
				sprintf(rbuf, "%s[rbp - %u]", ts, *off - fo);
			} else {
				size_t row = get_reg_size(opr.as.var.type);
				Register reg = *RegTable_get(&regal.allocated_regs, opr.as.var.addr_id);
				sprintf(rbuf, "%s", reg_forms[reg][row]);
				if (opr_kind) *opr_kind = REG;
			}
		} else if (opr.as.var.kind == VAR_ADDR) {
			if (opr.as.var.addr_kind == VAR_LOCAL) {
				uint *off = OffTable_get(&stack_table, opr.as.var.addr_id);
				if (off) {
					sb_appendf(&body, "  mov rax, qword[rbp - %u]\n", *off);
					if (fo) sprintf(rbuf, "%s[rax + %u]", ts, fo);
					else    sprintf(rbuf, "%s[rax]", ts);
				} else {
					Register reg = *RegTable_get(&regal.allocated_regs, opr.as.var.addr_id);
					if (fo) sprintf(rbuf, "%s[%s + %u]", ts, reg_forms[reg][3], fo);
					else    sprintf(rbuf, "%s[%s]", ts, reg_forms[reg][3]);
				}
			} else if (opr.as.var.addr_kind == VAR_GLOBAL) {
				if (fo) sprintf(rbuf, "%s[D%u + %u]", ts, opr.as.var.addr_id, fo);
				else    sprintf(rbuf, "%s[D%u]", ts, opr.as.var.addr_id);
			} else UNREACHABLE;
		} else if (opr.as.var.kind == VAR_GLOBAL) {
			if (fo) sprintf(rbuf, "%s[D%u + %u]", ts, opr.as.var.addr_id, fo);
			else    sprintf(rbuf, "%s[D%u]", ts, opr.as.var.addr_id);
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
			if (arg_id >= ARR_LEN(sysv_gn_fa)) {
				uint shadow_space = (arg_id - ARR_LEN(sysv_gn_fa)) * 8 + 48;
				sb_appendf(&body, "  mov %s, %s[rbp + %u]\n", reg_forms[R10][arg_size], ts, shadow_space);
				sprintf(rbuf, "%s", reg_forms[R10][arg_size]);
			} else {
				sprintf(rbuf, "%s", reg_forms[sysv_gn_fa[arg_id]][arg_size]);
			} break;
		case TP_WINDOWS:
			if (arg_id >= ARR_LEN(win_gn_fa)) {
				uint shadow_space = (arg_id - ARR_LEN(win_gn_fa)) * 8 + 48;
				sb_appendf(&body, "  mov %s, %s[rbp + %u]\n", reg_forms[R10][arg_size], ts, shadow_space);
				sprintf(rbuf, "%s", reg_forms[R10][arg_size]);
			} else {
				sprintf(rbuf, "%s", reg_forms[win_gn_fa[arg_id]][arg_size]);
			}
		}
	} break;

	default:
		UNREACHABLE;
	}

	return rbuf;
}

static void type_to_reg(TAC_Operand opr, char *arg1, char *arg2) {
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

static void load_reserved_regs(TAC_Instruction inst, char *arg1, char *arg2) {
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

static void stack_offset_add(uint off) {
	stack_offset += off;
	align_up(&stack_offset, 8);
}

void fasm_gen_new_var(TAC_Instruction ci, char *dst, OprKind *opr_kind) {
	if (opt_level > 0) {
		reg_allocator_free(&regal, inst_idx);
		if (ci.dst.as.var.type.kind != TYPE_STRUCT) {
			Register reg;
			if (reg_allocator_push(&regal, ci.dst.as.var.addr_id, (int*)&reg)) {
				if (opr_kind) *opr_kind = REG;
				size_t row = get_reg_size(ci.dst.as.var.type);
				sprintf(dst, "%s", reg_forms[reg][row]);
				return;
			}
		}
	}
	if (opr_kind) *opr_kind = MEM;
	char ts[32]; opr_type_to_stack(ci.dst, ts);
	stack_offset_add(get_type_size(ci.dst.as.var.type));
	OffTable_add(&stack_table, ci.dst.as.var.addr_id, stack_offset);
	sprintf(dst, "%s[rbp - %u]", ts, stack_offset);
}

void fasm_gen_func(StringBuilder *code, TAC_Func func) {
	if (!func.is_static)
		sb_appendf(code, "public %s\n", func.name);
	sb_appendf(code, "%s%s:\n", (tp == TP_MACOS ? "_" : ""), func.name);

	if (func.body.count == 0) {
		sb_appendf(code, "  ret\n\n");
		return;
	}

	is_there_return = false;
	RegTable_free(&regal.allocated_regs);
	regal.allocated_regs = (RegTable){0};
	regal.life_intervals = &func.var_ints;
	da_reset(&regal.available_regs);
	da_reset(&regal.callee_saved_regs);
	for (size_t i = 0; i < ARR_LEN(callee_saved); i++) {
		da_append(&regal.available_regs, callee_saved[i]);
	}

	sb_reset(&body);
	stack_offset = 0;

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
			fasm_gen_new_var(ci, dst, NULL);
			load_reserved_regs(ci, arg1, arg2);

			OprKind opr1_kind, opr2_kind;
			char opr1[64], opr2[64];

			sprintf(opr1, opr_to_fasm(ci.args[0], &opr1_kind));
			if (opr1_kind != REG) {
				sb_appendf(&body, "  mov %s, %s\n", arg1, opr1);
			} else sprintf(arg1, opr1);

			sprintf(opr2, opr_to_fasm(ci.args[1], &opr2_kind));
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
			fasm_gen_new_var(ci, dst, &dst_kind);
			load_reserved_regs(ci, arg1, arg2);

			sprintf(opr1, opr_to_fasm(ci.args[0], &opr1_kind));
			if (dst_kind != REG) {
				sb_appendf(&body, "  mov %s, %s\n", arg1, opr1);
			} else sprintf(arg1, opr1);

			bool is_div = ci.op == OP_DIV || ci.op == OP_MOD;
			sprintf(opr2, opr_to_fasm(ci.args[1], &opr2_kind));
			if (is_div || (opr2_kind != REG && dst_kind != REG)) {
				sb_appendf(&body, "  mov %s, %s\n", arg2, opr2);
			} else sprintf(arg2, opr2);

			if (dst_kind == REG) {
				sprintf(arg1, dst);
				sb_appendf(&body, "  mov %s, %s\n", dst, opr1);
			}

			if      (ci.op == OP_ADD)    sb_appendf(&body, "  add %s, %s\n",  arg1, arg2);
			else if (ci.op == OP_SUB)    sb_appendf(&body, "  sub %s, %s\n",  arg1, arg2);
			else if (ci.op == OP_BW_AND) sb_appendf(&body, "  and %s, %s\n",  arg1, arg2);
			else if (ci.op == OP_BW_OR)  sb_appendf(&body, "  or  %s, %s\n",  arg1, arg2);
			else if (ci.op == OP_BW_XOR) sb_appendf(&body, "  xor %s, %s\n",  arg1, arg2);
			else if (ci.op == OP_MUL)    sb_appendf(&body, "  imul %s, %s\n", arg1, arg2);

			else if (ci.op == OP_BW_LS || ci.op == OP_BW_RS) {
				const char *rcx = reg_forms[RCX][get_reg_size(ci.dst.as.var.type)];
				sb_appendf(&body, "  mov %s, %s\n", rcx, arg2);
				sb_appendf(&body, "  %s %s, cl\n", ci.op == OP_BW_LS ? "shl" : "shr", arg1);
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
			fasm_gen_new_var(ci, dst, NULL);
			load_reserved_regs(ci, arg1, arg2);
			sb_appendf(&body, "  mov %s, %s\n", arg1, opr_to_fasm(ci.args[0], NULL));
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

			fasm_gen_new_var(ci, dst, NULL);
			load_reserved_regs(ci, arg1, arg2);

			if (dst_type.kind == arg1_type.kind)
				UNREACHABLE;

			int dsz = 0;
			int ssz = 0;
			bool ssig = false;

			switch (dst_type.kind) {
				case TYPE_U64:
				case TYPE_UPTR: case TYPE_POINTER: dsz = 8; break;
				case TYPE_INT:  case TYPE_I32:     dsz = 4; break;
				case TYPE_UINT: case TYPE_U32:     dsz = 4; break;
				case TYPE_I64:  case TYPE_IPTR:    dsz = 8; break;
				case TYPE_I8:                      dsz = 1; break;
				case TYPE_U8:                      dsz = 1; break;
				case TYPE_I16:                     dsz = 2; break;
				case TYPE_U16:                     dsz = 2; break;
				default: UNREACHABLE;
			}

			switch (arg1_type.kind) {
				case TYPE_U64:
				case TYPE_UPTR: case TYPE_POINTER: ssz = 8; ssig = false; break;
				case TYPE_INT:  case TYPE_I32:     ssz = 4; ssig = true;  break;
				case TYPE_UINT: case TYPE_U32:     ssz = 4; ssig = false; break;
				case TYPE_I64:  case TYPE_IPTR:    ssz = 8; ssig = true;  break;
				case TYPE_I8:                      ssz = 1; ssig = true;  break;
				case TYPE_U8:                      ssz = 1; ssig = false; break;
				case TYPE_I16:                     ssz = 2; ssig = true;  break;
				case TYPE_U16:                     ssz = 2; ssig = false; break;
				default: UNREACHABLE;
			}

			const char *ext_inst = ssig ? "movsx" : "movzx"; // ext inst
			const char *DR = NULL; // dst
			const char *SR = NULL; // src
			const char *LR = NULL; // low

			switch (dsz) {
				case 1: DR = "al";  LR = "al";  break;
				case 2: DR = "ax";  LR = "ax";  break;
				case 4: DR = "eax"; LR = "eax"; break;
				case 8: DR = "rax"; LR = "eax"; break;
				default: UNREACHABLE;
			}

			switch (ssz) {
				case 1: SR = "al";  break;
				case 2: SR = "ax";  break;
				case 4: SR = "eax"; break;
				case 8: SR = "rax"; break;
				default: UNREACHABLE;
			}

			if (dsz > ssz) {
				if (ssz == 4 && dsz == 8) {
					if (ssig) {
						sb_appendf(&body, "  movsxd %s, %s\n", DR, opr_to_fasm(ci.args[0], NULL));
					} else {
						sb_appendf(&body, "  mov %s, %s\n", LR, opr_to_fasm(ci.args[0], NULL));
					}
				} else {
					sb_appendf(&body, "  %s %s, %s\n", ext_inst, DR, opr_to_fasm(ci.args[0], NULL));
				}
				sb_appendf(&body, "  mov %s, %s\n", opr_to_fasm(ci.dst, NULL), DR);
			} else if (dsz < ssz) {
				sb_appendf(&body, "  mov %s, %s\n", SR, opr_to_fasm(ci.args[0], NULL));
				sb_appendf(&body, "  mov %s, %s\n", opr_to_fasm(ci.dst, NULL), LR);
			} else {
				sb_appendf(&body, "  mov %s, %s\n", DR, opr_to_fasm(ci.args[0], NULL));
				sb_appendf(&body, "  mov %s, %s\n", opr_to_fasm(ci.dst, NULL), DR);
			}
		} break;

		case OP_ASSIGN: {
			bool fst_asg = false;
			if (ci.dst.as.var.kind == VAR_LOCAL) {
				uint *off = OffTable_get(&stack_table, ci.dst.as.var.addr_id);
				Register *reg = (Register*)RegTable_get(&regal.allocated_regs, ci.dst.as.var.addr_id);
				if (!off && !reg) {
					fst_asg = true;
					fasm_gen_new_var(ci, dst, NULL);
				}
			}

			if (ci.dst.as.var.type.kind == TYPE_ARRAY && fst_asg) {
				load_reserved_regs(ci, arg1, arg2);
				stack_offset_add(
					get_type_size(*ci.dst.as.var.type.as.array.elem) *
					ci.dst.as.var.type.as.array.length);

				sb_appendf(&body, "  lea %s, [rbp - %u]\n", arg1, stack_offset);
				sb_appendf(&body, "  mov %s, %s\n", opr_to_fasm(ci.dst, NULL), arg1);
			}

			if (ci.args[0].kind != OPR_NULL) {
				if (tac_ir_get_opr_type(ci.dst).kind == TYPE_STRUCT) {
					sb_appendf(&body, "  lea rsi, %s\n", opr_to_fasm(ci.args[0], NULL));
					sb_appendf(&body, "  lea rdi, %s\n", opr_to_fasm(ci.dst, NULL));
					sb_appendf(&body, "  mov rcx, %u\n", get_type_size(tac_ir_get_opr_type(ci.dst)));
					sb_appendf(&body, "  cld\n");
					sb_appendf(&body, "  rep movsb\n");
				} else {
					load_reserved_regs(ci, arg1, arg2);
					OprKind dst_kind;
					sprintf(dst, "%s", opr_to_fasm(ci.dst, &dst_kind));
					if (dst_kind != REG) {
						sb_appendf(&body, "  mov %s, %s\n", arg2, opr_to_fasm(ci.args[0], NULL));
						sb_appendf(&body, "  mov %s, %s\n", dst, arg2);
					} else {
						sb_appendf(&body, "  mov %s, %s\n", dst, opr_to_fasm(ci.args[0], NULL));
					}
				}
			} else {
				if (tac_ir_get_opr_type(ci.dst).kind == TYPE_STRUCT) {
					sb_appendf(&body, "  xor rax, rax\n");
					sb_appendf(&body, "  lea rdi, %s\n", opr_to_fasm(ci.dst, NULL));
					sb_appendf(&body, "  mov rcx, %u\n", get_type_size(tac_ir_get_opr_type(ci.dst)));
					sb_appendf(&body, "  rep stosb\n");
				}
			}
		} break;

		case OP_DEREF: {
			fasm_gen_new_var(ci, dst, NULL);
			char ts[32]; opr_type_to_stack(ci.dst, ts);

			if (ci.dst.as.var.type.kind != TYPE_STRUCT) {
				load_reserved_regs(ci, arg1, arg2);
				sb_appendf(&body, "  mov rax, %s\n",     opr_to_fasm(ci.args[0], NULL));
				sb_appendf(&body, "  mov %s, %s[rax]\n", arg1, ts);
				sb_appendf(&body, "  mov %s, %s\n", opr_to_fasm(ci.dst, NULL), arg1);
			} else {
				sb_appendf(&body, "  mov rsi, %s\n", opr_to_fasm(ci.args[0], NULL));
				sb_appendf(&body, "  lea rdi, %s\n", opr_to_fasm(ci.dst, NULL));
				sb_appendf(&body, "  mov rcx, %u\n", get_type_size(tac_ir_get_opr_type(ci.dst)));
				sb_appendf(&body, "  cld\n");
				sb_appendf(&body, "  rep movsb\n");
			}
		} break;

		case OP_REF: {
			fasm_gen_new_var(ci, dst, NULL);
			size_t fo = get_struct_offset(ci.args[0]);

			if (ci.args[0].as.var.kind == VAR_ADDR) {
				if (ci.args[0].as.var.addr_kind == VAR_LOCAL) {
					uint off = *OffTable_get(&stack_table, ci.args[0].as.var.addr_id);
					sb_appendf(&body, "  mov rax, [rbp - %u]\n", off);
					sb_appendf(&body, "  add rax, %zu\n", fo);
				} else if (ci.args[0].as.var.addr_kind == VAR_GLOBAL) {
					sb_appendf(&body, "  lea rax, [D%u]\n", ci.args[0].as.var.addr_id);
					sb_appendf(&body, "  add rax, %zu\n", fo);
				}
			} else if (ci.args[0].as.var.kind == VAR_LOCAL) {
				uint off = *OffTable_get(&stack_table, ci.args[0].as.var.addr_id);
				sb_appendf(&body, "  lea rax, [rbp - %u]\n", off - fo);
			} else if (ci.args[0].as.var.kind == VAR_GLOBAL) {
				sb_appendf(&body, "  lea rax, [D%u]\n", ci.args[0].as.var.addr_id);
				sb_appendf(&body, "  add rax, %zu\n", fo);
			}

			sb_appendf(&body, "  mov %s, rax\n", opr_to_fasm(ci.dst, NULL));
		} break;

		case OP_JUMP_IF_NOT: {
			load_reserved_regs(ci, arg1, arg2);
			sb_appendf(&body, "  mov %s, %s\n", arg1, opr_to_fasm(ci.args[0], NULL));
			sb_appendf(&body, "  cmp %s, 0\n", arg1);
			sb_appendf(&body, "  je %s\n", opr_to_fasm(ci.dst, NULL));
		} break;

		case OP_LABEL: {
			sb_appendf(&body, "%s:\n", opr_to_fasm(ci.args[0], NULL));
		} break;

		case OP_JUMP: {
			sb_appendf(&body, "  jmp %s\n", opr_to_fasm(ci.dst, NULL));
		} break;

		case OP_RETURN: {
			if (ci.args[0].kind != OPR_NULL) {
				switch (func.type.kind) {
				case TYPE_STRUCT:
				case TYPE_ARRAY:
					assert(!"error: returning arrays/structs isn't supported yet\n");
				default:;
					uint reg_size = get_reg_size(func.type);
					sb_appendf(&body, "  mov %s, %s\n", reg_forms[RAX][reg_size], opr_to_fasm(ci.args[0], NULL));
				}
			}

			is_there_return = true;
			sb_appendf(&body, "  jmp .FE\n");
		} break;

		case OP_FUNC_CALL: {
			bool is_shadow_space_used = false;
			for (size_t i = 0; ci.args[i].kind != OPR_NULL; i++) {
				if (i >= ARR_LEN(sysv_gn_fa)) {
					is_shadow_space_used = true;
					sb_appendf(&body, "  sub rsp, 32\n");
					break;
				}
			}
			for (size_t i = 0; ci.args[i].kind != OPR_NULL; i++) {
				char ts[32]; opr_type_to_stack(ci.args[i], ts);
				size_t arg_size = get_reg_size(tac_ir_get_opr_type(ci.args[i]));
				switch (tp) {
				case TP_MACOS:
				case TP_LINUX:
					if (i >= ARR_LEN(sysv_gn_fa)) {
						sb_appendf(&body, "  mov %s, %s\n", reg_forms[R10][arg_size], opr_to_fasm(ci.args[i], NULL));
						uint shadow_space = (i - ARR_LEN(sysv_gn_fa)) * 8 + 32;
						sb_appendf(&body, "  mov %s[rsp + %u], %s\n", ts, shadow_space, reg_forms[R10][arg_size]);
					} else {
						sb_appendf(&body, "  mov %s, %s\n", reg_forms[sysv_gn_fa[i]][arg_size], opr_to_fasm(ci.args[i], NULL));
					} break;
				case TP_WINDOWS:
					if (i >= ARR_LEN(win_gn_fa)) {
						sb_appendf(&body, "  mov %s, %s\n", reg_forms[R10][arg_size], opr_to_fasm(ci.args[i], NULL));
						uint shadow_space = (i - ARR_LEN(win_gn_fa)) * 8 + 32;
						sb_appendf(&body, "  mov %s[rsp + %u], %s\n", ts, shadow_space, reg_forms[R10][arg_size]);
					} else {
						sb_appendf(&body, "  mov %s, %s\n", reg_forms[win_gn_fa[i]][arg_size], opr_to_fasm(ci.args[i], NULL));
					}
				}
			}
			sb_appendf(&body, "  call %s%s\n", (tp == TP_MACOS ? "_" : ""), ci.dst.as.name);
			if (is_shadow_space_used) sb_appendf(&body, "  add rsp, 32\n");
		} break;

		default:
			UNREACHABLE;
		}
	}

	bool is_stack_used = stack_offset != 0;
	stack_offset += 48;
	align_up(&stack_offset, 16);

	if (opt_level > 0) {
		stack_offset += ((regal.callee_saved_regs.count + is_stack_used) * 8 % 16 == 0) * 8;
		for (size_t i = 0; i < regal.callee_saved_regs.count; i++) {
			sb_appendf(code, "  push %s\n", reg_forms[regal.callee_saved_regs.items[i]][3]);
		}
	}

	if (is_stack_used) {
		sb_appendf(code, "  push rbp\n");
		sb_appendf(code, "  mov rbp, rsp\n");
		sb_appendf(code, "  sub rsp, %u\n", stack_offset);
	} else {
		if (regal.callee_saved_regs.count * 8 % 16 == 0) {
			sb_appendf(code, "  sub rsp, 8\n");
		}
	}

	sb_appendf(code, "%s", body.items);

	if (strcmp(func.name, "main") == 0)
		sb_appendf(code, "  mov eax, 0\n");
	if (is_there_return)
		sb_appendf(code, ".FE:\n");
	if (is_stack_used)
		sb_appendf(code, "  leave\n");
	else if (regal.callee_saved_regs.count * 8 % 16 == 0)
		sb_appendf(code, "  add rsp, 8\n");
	if (opt_level > 0) {
		for (long i = (long)regal.callee_saved_regs.count - 1; i >= 0; i--) {
			sb_appendf(code, "  pop %s\n", reg_forms[regal.callee_saved_regs.items[i]][3]);
		}
	}
	sb_appendf(code, "  ret\n\n");
}

char *fasm_gen_prog(TAC_Program *prog, TargetPlatform _tp, int _opt_level) {
	StringBuilder code = {0};
	opt_level = _opt_level;
	tp = _tp;

	const char *format = (match(tp),
		when(TP_LINUX, "ELF64")
		when(TP_WINDOWS, "PE")
		when(TP_MACOS, "MACH64") NULL);
	sb_appendf(&code, "format %s\n", format);

	sb_appendf(&code, "\n");
	da_foreach(TAC_Extern, ext, &prog->externs)
		sb_appendf(&code, "extrn %s\n", ext->name);
	sb_appendf(&code, "\n");

	sb_appendf(&code, "section '.data'\n");
	uint uniq_data_off = 0;

	da_foreach (TAC_GlobalVar, g, &prog->globals) {
		if (g->type.kind == TYPE_ARRAY && g->is_none) {
			uint arr_size = get_type_size(*g->type.as.array.elem) * g->type.as.array.length;
			sb_appendf(&code, "  U%u db %u dup (0)\n", uniq_data_off, arr_size);
			sb_appendf(&code, "  align 8\n");
			sb_appendf(&code, "  D%u dq U%u\n", g->index, uniq_data_off++);
		} else {
			if (g->is_none) {
				sb_appendf(&code, "  D%u db %u dup (0)\n", g->index, get_type_size(g->type));
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

	sb_appendf(&code, "section '.text'\n");
	for (size_t i = 0; i < prog->funcs.count; i++) {
		fasm_gen_func(&code, da_get(&prog->funcs, i));
	}

	return code.items;
}
