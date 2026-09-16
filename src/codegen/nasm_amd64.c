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
static TAC_Func ir_func;
static RegAllocator regal = {0};
static StringBuilder body = {0};
static bool is_there_return;
static uint stack_offset;
static uint inst_idx;

typedef struct {
	Type type;
	double value;
} LitFloat;

static DA(LitFloat) floats = {0};

typedef struct {
	enum {
		REG, MEM,
		IMM, LBL,
	} kind;
	Type type;
	char text[64];
} NasmOpr;

NasmOpr nasm_oprt(int kind, Type type, char *text) {
	NasmOpr res = {kind, type};
	sprintf(res.text, "%s", text);
	return res;
}

NasmOpr nasm_opr(int kind, char *text) {
	NasmOpr res = {kind, (Type){TYPE_NULL}};
	sprintf(res.text, "%s", text);
	return res;
}

void nasm_mov(NasmOpr dst, NasmOpr src) {
	size_t s = get_reg_size(dst.type);
	char *reg = "xmm14";
	char *pf = "";

	switch (dst.type.kind) {
	case TYPE_FLOAT:
	case TYPE_F32:
		pf = "ss";
		break;
	case TYPE_F64:
		pf = "sd";
		break;
	default:
		switch (tp) {
		case TP_MACOS:
		case TP_LINUX:
			reg = RF[sysv_scr[0]][s];
			break;
		case TP_WINDOWS:
			reg = RF[win_scr[0]][s];
		}
	}

	if (
		dst.kind == REG && src.kind == REG ||
		dst.kind == MEM && src.kind == REG ||
		dst.kind == REG && src.kind == MEM ||
		dst.kind == REG && src.kind == IMM
	) {
		sb_appendf(&body, "  mov%s %s, %s\n", pf, dst.text, src.text);
	} else if (
		dst.kind == MEM && src.kind == MEM ||
		dst.kind == MEM && src.kind == IMM
	) {
		sb_appendf(&body, "  mov%s %s, %s\n", pf, reg, src.text);
		sb_appendf(&body, "  mov%s %s, %s\n", pf, dst.text, reg);
	} else UNREACHABLE;
}

static Register *reg_allocator_get(uint vid) {
	int *cer = RegTable_get(&regal.allocated_ce_regs, vid);
	if (cer) return (Register*)cer;
	int *crr = RegTable_get(&regal.allocated_cr_regs, vid);
	return (Register*)crr;
}

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

NasmOpr opr_to_nasm(TAC_Operand opr) {
	int kind;
	char buf[64];

	switch (opr.kind) {
	case OPR_SIZEOF: {
		uint size = get_type_size(opr.as.size_of.vtype);
		if (opr.as.size_of.vtype.kind == TYPE_ARRAY) {
			uint elemSize = get_type_size(*opr.as.size_of.vtype.as.array.elem);
			size = elemSize * opr.as.size_of.vtype.as.array.length;
		}
		kind = IMM;
		sprintf(buf, "%u", size);
	} break;

	case OPR_LABEL: {
		kind = LBL;
		sprintf(buf, ".L%u", opr.as.label_id);
	} break;

	case OPR_VAR: {
		uint fo = get_struct_offset(opr);
		char ts[32]; opr_type_to_stack(opr, ts);
		kind = MEM;
		if (opr.as.var.kind == VAR_LOCAL) {
			uint *off = OffTable_get(&stack_table, opr.as.var.addr_id);
			if (off) {
				sprintf(buf, "%s[rbp - %u]", ts, *off - fo);
			} else {
				kind = REG;
				size_t row = get_reg_size(opr.as.var.type);
				Register reg = *reg_allocator_get(opr.as.var.addr_id);
				if (reg < XMM0) {
					sprintf(buf, "%s", RF[reg][row]);
				} else {
					sprintf(buf, "%s", RFf[reg]);
				}
			}
		} else if (opr.as.var.kind == VAR_ADDR) {
			if (opr.as.var.addr_kind == VAR_LOCAL) {
				uint *off = OffTable_get(&stack_table, opr.as.var.addr_id);
				if (off) {
					sb_appendf(&body, "  mov rax, qword[rbp - %u]\n", *off);
					if (fo) sprintf(buf, "%s[rax + %u]", ts, fo);
					else    sprintf(buf, "%s[rax]", ts);
				} else {
					Register reg = *reg_allocator_get(opr.as.var.addr_id);
					if (fo) sprintf(buf, "%s[%s + %u]", ts, RF[reg][3], fo);
					else    sprintf(buf, "%s[%s]", ts, RF[reg][3]);
				}
			} else if (opr.as.var.addr_kind == VAR_GLOBAL) {
				if (fo) sprintf(buf, "%s[D%u + %u]", ts, opr.as.var.addr_id, fo);
				else    sprintf(buf, "%s[D%u]", ts, opr.as.var.addr_id);
			} else UNREACHABLE;
		} else if (opr.as.var.kind == VAR_GLOBAL) {
			if (fo) sprintf(buf, "%s[D%u + %u]", ts, opr.as.var.addr_id, fo);
			else    sprintf(buf, "%s[D%u]", ts, opr.as.var.addr_id);
		}
	} break;

	case OPR_LITERAL: {
		kind = IMM;
		long long val = opr.as.literal.as.lint;
		switch (opr.as.literal.type.kind) {
		case TYPE_FLOAT:
		case TYPE_F64:
		case TYPE_F32: {
			kind = MEM;
			size_t idx = floats.count;
			LitFloat lit = {
				.type = opr.as.literal.type,
				.value = opr.as.literal.as.lfloat,
			};
			da_append(&floats, lit);
			sprintf(buf, "[F%zu]", idx);
		} break;
		case TYPE_I32:
		case TYPE_INT:
			sprintf(buf, "%d", (int)val);
			break;
		case TYPE_U32:
		case TYPE_UINT:
			sprintf(buf, "%u", (uint)val);
			break;
		case TYPE_BOOL:
		case TYPE_I8:
			sprintf(buf, "%d", (i8)val);
			break;
		case TYPE_U8:
			sprintf(buf, "%d", (u8)val);
			break;
		case TYPE_I16:
			sprintf(buf, "%hd", (i16)val);
			break;
		case TYPE_U16:
			sprintf(buf, "%hu", (u16)val);
			break;
		case TYPE_UPTR:
		case TYPE_U64:
			sprintf(buf, "%llu", val);
			break;
		case TYPE_ARRAY:
		case TYPE_POINTER:
		case TYPE_IPTR:
		case TYPE_I64:
			sprintf(buf, "%lli", val);
			break;
		default:
			UNREACHABLE;
		}
	} break;

	case OPR_FUNC_RET: {
		kind = REG;
		Type type = opr.as.func_ret.type;
		switch (type.kind) {
		case TYPE_ARRAY:
		case TYPE_STRUCT:
			assert(!"passing arrays or structs isn't supported yet");
			break;
		case TYPE_FLOAT:
		case TYPE_F32:
		case TYPE_F64:
			sprintf(buf, "%s", RFf[XMM0]);
			break;
		default:
			sprintf(buf, "%s", RF[RAX][get_reg_size(type)]);
		}
	} break;

	case OPR_FUNC_INP: {
		char ts[32]; opr_type_to_stack(opr, ts);
		uint arg_id = opr.as.func_inp.arg_id;
		size_t as = get_reg_size(opr.as.func_inp.type);

		size_t fl_idx = 0;
		size_t gn_idx = 0;
		size_t sh_idx = 0;

		for (size_t i = 0; i < ir_func.args.count; i++) {
			bool is_float = is_type_float(ir_func.args.items[i].type);

			switch (tp) {
			case TP_MACOS:
			case TP_LINUX:
				if (
					gn_idx >= ARR_LEN(sysv_gn_fa) && !is_float ||
					fl_idx >= ARR_LEN(sysv_fl_fa) &&  is_float
				) {
					kind = MEM;
					if (arg_id == i) {
						uint shadow_space = sh_idx * 8 + 48;
						sprintf(buf, "%s[rbp + %u]", ts, shadow_space);
						break;
					}
					sh_idx++;
				} else {
					kind = REG;
					if (is_float) {
						if (arg_id == i) {
							sprintf(buf, "%s", RFf[sysv_fl_fa[fl_idx]]);
							break;
						}
						fl_idx++;
					} else {
						if (arg_id == i) {
							sprintf(buf, "%s", RF[sysv_gn_fa[gn_idx]][as]);
							break;
						}
						gn_idx++;
					}
				} break;
			case TP_WINDOWS:
				if (
					gn_idx >= ARR_LEN(win_gn_fa) && !is_float ||
					fl_idx >= ARR_LEN(win_fl_fa) &&  is_float
				) {
					kind = MEM;
					if (arg_id == i) {
						uint shadow_space = sh_idx * 8 + 48;
						sprintf(buf, "%s[rbp + %u]", ts, shadow_space);
						break;
					}
					sh_idx++;
				} else {
					kind = REG;
					if (is_float) {
						if (arg_id == i) {
							sprintf(buf, "%s", RFf[win_fl_fa[fl_idx]]);
							break;
						}
						fl_idx++;
					} else {
						if (arg_id == i) {
							sprintf(buf, "%s", RF[win_gn_fa[gn_idx]][as]);
							break;
						}
						gn_idx++;
					}
				} break;
			}
		}
	} break;

	default:
		UNREACHABLE;
	}

	Type type;
	if (opr.kind != OPR_LABEL && opr.kind != OPR_FIELD) {
		type = tac_ir_get_opr_type(opr);
	}

	return nasm_oprt(kind, type, buf);
}

static void type_to_reg(TAC_Operand opr, char *arg1, char *arg2) {
	Type opr_type = tac_ir_get_opr_type(opr);
	switch (opr_type.kind) {
	case TYPE_F64:
	case TYPE_F32:
	case TYPE_FLOAT:
		sprintf(arg1, "xmm14");
		sprintf(arg2, "xmm15");
		break;
	default:
		switch (tp) {
		case TP_MACOS:
		case TP_LINUX:
			sprintf(arg1, "%s", RF[sysv_scr[0]][get_reg_size(opr_type)]);
			sprintf(arg2, "%s", RF[sysv_scr[1]][get_reg_size(opr_type)]);
			break;
		case TP_WINDOWS:
			sprintf(arg1, "%s", RF[win_scr[0]][get_reg_size(opr_type)]);
			sprintf(arg2, "%s", RF[win_scr[1]][get_reg_size(opr_type)]);
		}
	}
}

static void load_reserved_regs(TAC_Instruction inst, char *arg1, char *arg2) {
	if (inst.dst.kind == OPR_LABEL) {
		inst.dst.kind = OPR_VAR;
		inst.dst.as.var.type = (Type){TYPE_BOOL};
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

NasmOpr nasm_gen_new_var(TAC_Instruction ci) {
	Type type = ci.dst.as.var.type;

	if (opt_level > 0) {
		Register reg;
		reg_allocator_free(&regal, inst_idx);
		if (is_type_integer(type)) {
			size_t row = get_reg_size(type);
			if (reg_allocator_push_ce(&regal, ci.dst.as.var.addr_id, (int*)&reg)) {
				return nasm_oprt(REG, type, RF[reg][row]);
			}
		} else if (is_type_float(type)) {
			if (reg_allocator_push_cr(&regal, ci.dst.as.var.addr_id, (int*)&reg)) {
				return nasm_oprt(REG, type, RFf[reg]);
			}
		}
	}

	char ts[32]; opr_type_to_stack(ci.dst, ts);
	stack_offset_add(get_type_size(type));
	OffTable_add(&stack_table, ci.dst.as.var.addr_id, stack_offset);
	char buf[64]; sprintf(buf, "%s[rbp - %u]", ts, stack_offset);
	return nasm_oprt(MEM, ci.dst.as.var.type, buf);
}

void nasm_gen_func(StringBuilder *code, TAC_Func func) {
	if (!func.is_static)
		sb_appendf(code, "global %s\n", func.name);
	sb_appendf(code, "%s%s:\n", (tp == TP_MACOS ? "_" : ""), func.name);

	if (func.body.count == 0) {
		sb_appendf(code, "  ret\n\n");
		return;
	}

	is_there_return = false;
	ir_func = func;

	RegTable_free(&regal.allocated_ce_regs);
	RegTable_free(&regal.allocated_cr_regs);

	regal.allocated_ce_regs = (RegTable){0};
	regal.allocated_cr_regs = (RegTable){0};
	regal.life_intervals = &func.var_ints;

	da_reset(&regal.callee_saved_regs);
	da_reset(&regal.available_ce_regs);
	da_reset(&regal.available_cr_regs);

	switch (tp) {
	case TP_MACOS:
	case TP_LINUX:
		for (size_t i = 0; i < ARR_LEN(sysv_fl_cr); i++)
			da_append(&regal.available_cr_regs, sysv_fl_cr[i]);
		for (size_t i = 0; i < ARR_LEN(sysv_gn_ce); i++)
			da_append(&regal.available_ce_regs, sysv_gn_ce[i]);
		break;
	case TP_WINDOWS:
		for (size_t i = 0; i < ARR_LEN(win_fl_cr); i++)
			da_append(&regal.available_cr_regs, win_fl_cr[i]);
		for (size_t i = 0; i < ARR_LEN(win_gn_ce); i++)
			da_append(&regal.available_ce_regs, win_gn_ce[i]);
		break;
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
			NasmOpr oprd = nasm_gen_new_var(ci);
			load_reserved_regs(ci, arg1, arg2);

			NasmOpr opr1 = opr_to_nasm(ci.args[0]);
			if (opr1.kind != REG) {
				sb_appendf(&body, "  mov %s, %s\n", arg1, opr1.text);
			} else sprintf(arg1, "%s", opr1.text);

			NasmOpr opr2 = opr_to_nasm(ci.args[1]);
			if (opr2.kind != REG) {
				sb_appendf(&body, "  mov %s, %s\n", arg2, opr2.text);
			} else sprintf(arg2, "%s", opr2.text);

			if (oprd.kind != REG) {
				sprintf(dst, "al");
			} else sprintf(dst, "%s", oprd.text);

			if (ci.op == OP_EQ) {
				sb_appendf(&body, "  cmp %s, %s\n", arg1, arg2);
				sb_appendf(&body, "  sete %s\n", dst);
			} else if (ci.op == OP_NOT_EQ) {
				sb_appendf(&body, "  cmp %s, %s\n", arg1, arg2);
				sb_appendf(&body, "  setne %s\n", dst);
			} else if (ci.op == OP_GREAT) {
				sb_appendf(&body, "  cmp %s, %s\n", arg1, arg2);
				sb_appendf(&body, "  setg %s\n", dst);
			} else if (ci.op == OP_LESS) {
				sb_appendf(&body, "  cmp %s, %s\n", arg1, arg2);
				sb_appendf(&body, "  setl %s\n", dst);
			} else if (ci.op == OP_GREAT_EQ) {
				sb_appendf(&body, "  cmp %s, %s\n", arg1, arg2);
				sb_appendf(&body, "  setge %s\n", dst);
			} else if (ci.op == OP_LESS_EQ) {
				sb_appendf(&body, "  cmp %s, %s\n", arg1, arg2);
				sb_appendf(&body, "  setle %s\n", dst);
			}

			if (oprd.kind != REG) {
				sb_appendf(&body, "  mov %s, al\n", oprd.text);
			}
		} break;

		case OP_ADD:    case OP_SUB:
		case OP_MUL:    case OP_DIV:
		case OP_AND:    case OP_OR:
		case OP_BW_AND: case OP_BW_OR:
		case OP_BW_LS:  case OP_BW_RS:
		case OP_BW_XOR: case OP_MOD: {
			char *pf = "";
			bool flt = false;
			switch (ci.dst.as.var.type.kind) {
			case TYPE_FLOAT:
			case TYPE_F32:
				pf = "ss";
				flt = true;
				break;
			case TYPE_F64:
				pf = "sd";
				flt = true;
			}

			NasmOpr oprd = nasm_gen_new_var(ci);
			sprintf(dst, "%s", oprd.text);
			load_reserved_regs(ci, arg1, arg2);

			NasmOpr opr1 = opr_to_nasm(ci.args[0]);
			if (oprd.kind != REG) {
				sb_appendf(&body, "  mov%s %s, %s\n", pf, arg1, opr1.text);
			} else sprintf(arg1, "%s", opr1.text);

			bool is_div = ci.op == OP_DIV || ci.op == OP_MOD;
			NasmOpr opr2 = opr_to_nasm(ci.args[1]);
			if (is_div || (opr2.kind != REG && oprd.kind != REG)) {
				sb_appendf(&body, "  mov%s %s, %s\n", pf, arg2, opr2.text);
			} else sprintf(arg2, "%s", opr2.text);

			if (oprd.kind == REG) {
				sprintf(arg1, "%s", dst);
				sb_appendf(&body, "  mov%s %s, %s\n", pf, dst, opr1.text);
			}

			if      (ci.op == OP_ADD)    sb_appendf(&body, "  add%s %s, %s\n", pf,  arg1, arg2);
			else if (ci.op == OP_SUB)    sb_appendf(&body, "  sub%s %s, %s\n", pf, arg1, arg2);
			else if (ci.op == OP_BW_AND) sb_appendf(&body, "  and %s, %s\n", arg1, arg2);
			else if (ci.op == OP_BW_OR)  sb_appendf(&body, "  or  %s, %s\n", arg1, arg2);
			else if (ci.op == OP_BW_XOR) sb_appendf(&body, "  xor %s, %s\n", arg1, arg2);

			else if (ci.op == OP_MUL && !flt) sb_appendf(&body, "  imul %s, %s\n", arg1, arg2);
			else if (ci.op == OP_MUL && flt)  sb_appendf(&body, "  mul%s %s, %s\n", pf, arg1, arg2);
			else if (ci.op == OP_DIV && flt)  sb_appendf(&body, "  div%s %s, %s\n", pf, arg1, arg2);

			else if (ci.op == OP_BW_LS || ci.op == OP_BW_RS) {
				const char *rcx = RF[RCX][get_reg_size(ci.dst.as.var.type)];
				sb_appendf(&body, "  mov %s, %s\n", rcx, arg2);
				sb_appendf(&body, "  %s %s, cl\n", ci.op == OP_BW_LS ? "shl" : "shr", arg1);
			}

			else if ((ci.op == OP_DIV || ci.op == OP_MOD) && !flt) {
				char *SEI[] = {"cbw", "cwd", "cdq", "cqo"};
				uint reg_size = get_reg_size(ci.dst.as.var.type);
				sb_appendf(&body, "  mov %s, %s\n", RF[RAX][reg_size], arg1);

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

				if (ci.op == OP_DIV) sprintf(arg1, "%s", RF[RAX][reg_size]);
				else                 sprintf(arg1, "%s", RF[RDX][reg_size]);
			}

			else if (ci.op == OP_AND) {
				sb_appendf(&body, "  and %s, %s\n", arg1, arg2);
			} else if (ci.op == OP_OR) {
				sb_appendf(&body, "  or %s, %s\n", arg1, arg2);
			}

			if (oprd.kind != REG || (is_div && !flt)) {
				sb_appendf(&body, "  mov%s %s, %s\n", pf, dst, arg1);
			}
		} break;

		case OP_BW_NOT:
		case OP_NOT: case OP_NEG: {
			NasmOpr oprd = nasm_gen_new_var(ci);
			sprintf(dst, "%s", oprd.text);

			load_reserved_regs(ci, arg1, arg2);
			sb_appendf(&body, "  mov %s, %s\n", arg1, opr_to_nasm(ci.args[0]).text);

			if (ci.op == OP_NEG)
				sb_appendf(&body, "  neg %s\n", arg1);
			else if (ci.op == OP_BW_NOT)
				sb_appendf(&body, "  not %s\n", arg1);
			else if (ci.op == OP_NOT) {
				sb_appendf(&body, "  test %s, %s\n", arg1, arg1);
				sb_appendf(&body, "  setz al\n");
				sprintf(arg1, "al");
			}

			sb_appendf(&body, "  mov %s, %s\n", dst, arg1);
		} break;

		case OP_CAST: {
			Type dt = ci.dst.as.var.type;
			Type st = tac_ir_get_opr_type(ci.args[0]);

			NasmOpr oprd = nasm_gen_new_var(ci);
			sprintf(dst, "%s", oprd.text);
			load_reserved_regs(ci, arg1, arg2);

			if (dt.kind == st.kind)
				UNREACHABLE;

			if (
				dt.kind == TYPE_F32 && st.kind == TYPE_FLOAT ||
				dt.kind == TYPE_FLOAT && st.kind == TYPE_F32
			) {
				nasm_mov(oprd, opr_to_nasm(ci.args[0]));
				break;
			}

			int dsz = 0;
			int ssz = 0;
			bool ssig = false;

			// New flags to detect floating point casts
			bool dst_is_float = false;
			bool src_is_float = false;

			switch (dt.kind) {
				case TYPE_U64:
				case TYPE_UPTR: case TYPE_POINTER: dsz = 8; break;
				case TYPE_INT:  case TYPE_I32:     dsz = 4; break;
				case TYPE_UINT: case TYPE_U32:     dsz = 4; break;
				case TYPE_I64:  case TYPE_IPTR:    dsz = 8; break;
				case TYPE_I8:                      dsz = 1; break;
				case TYPE_U8:                      dsz = 1; break;
				case TYPE_I16:                     dsz = 2; break;
				case TYPE_U16:                     dsz = 2; break;
				case TYPE_F32: case TYPE_FLOAT:    dsz = 4; dst_is_float = true; break;
				case TYPE_F64:                     dsz = 8; dst_is_float = true; break;
				default: UNREACHABLE;
			}

			switch (st.kind) {
				case TYPE_U64:
				case TYPE_UPTR: case TYPE_POINTER: ssz = 8; ssig = false; break;
				case TYPE_INT:  case TYPE_I32:     ssz = 4; ssig = true;  break;
				case TYPE_UINT: case TYPE_U32:     ssz = 4; ssig = false; break;
				case TYPE_I64:  case TYPE_IPTR:    ssz = 8; ssig = true;  break;
				case TYPE_I8:                      ssz = 1; ssig = true;  break;
				case TYPE_U8:                      ssz = 1; ssig = false; break;
				case TYPE_I16:                     ssz = 2; ssig = true;  break;
				case TYPE_U16:                     ssz = 2; ssig = false; break;
				case TYPE_F32: case TYPE_FLOAT:    ssz = 4; src_is_float = true; break;
				case TYPE_F64:                     ssz = 8; src_is_float = true; break;
				default: UNREACHABLE;
			}

			if (dst_is_float || src_is_float) {
				const char *dst_text = opr_to_nasm(ci.dst).text;
				const char *src_text = opr_to_nasm(ci.args[0]).text;

				if (dst_is_float && src_is_float) {
					// Float to Float
					if (ssz == 4 && dsz == 8) { // F32 -> F64
						sb_appendf(&body, "  cvtss2sd xmm0, %s\n", src_text);
						sb_appendf(&body, "  movsd %s, xmm0\n", dst_text);
					} else if (ssz == 8 && dsz == 4) { // F64 -> F32
						sb_appendf(&body, "  cvtsd2ss xmm0, %s\n", src_text);
						sb_appendf(&body, "  movss %s, xmm0\n", dst_text);
					}
				} else if (dst_is_float && !src_is_float) {
					// Integer to Float
					if (ssz < 4) {
						sb_appendf(&body, "  %s eax, %s\n", ssig ? "movsx" : "movzx", src_text);
						src_text = "eax";
					} else if (ssz == 4) {
						sb_appendf(&body, "  mov eax, %s\n", src_text);
						src_text = "eax";
					} else if (ssz == 8) {
						sb_appendf(&body, "  mov rax, %s\n", src_text);
						src_text = "rax";
					}

					const char *inst = (dsz == 8) ? "cvtsi2sd" : "cvtsi2ss";
					sb_appendf(&body, "  %s xmm0, %s\n", inst, src_text);

					const char *mov_inst = (dsz == 8) ? "movsd" : "movss";
					sb_appendf(&body, "  %s %s, xmm0\n", mov_inst, dst_text);

				} else if (!dst_is_float && src_is_float) {
					// Float to Integer
					const char *inst = (ssz == 8) ? "cvttsd2si" : "cvttss2si";
					const char *int_reg = (dsz == 8) ? "rax" : "eax";

					sb_appendf(&body, "  %s %s, %s\n", inst, int_reg, src_text);

					// Move result to the properly sized destination
					const char *sub_reg = NULL;
					switch (dsz) {
						case 1: sub_reg = "al";  break;
						case 2: sub_reg = "ax";  break;
						case 4: sub_reg = "eax"; break;
						case 8: sub_reg = "rax"; break;
					}
					sb_appendf(&body, "  mov %s, %s\n", dst_text, sub_reg);
				}
			} else {
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
							sb_appendf(&body, "  movsxd %s, %s\n", DR, opr_to_nasm(ci.args[0]).text);
						} else {
							sb_appendf(&body, "  mov %s, %s\n", LR, opr_to_nasm(ci.args[0]).text);
						}
					} else {
						sb_appendf(&body, "  %s %s, %s\n", ext_inst, DR, opr_to_nasm(ci.args[0]).text);
					}
					sb_appendf(&body, "  mov %s, %s\n", opr_to_nasm(ci.dst).text, DR);
				} else if (dsz < ssz) {
					sb_appendf(&body, "  mov %s, %s\n", SR, opr_to_nasm(ci.args[0]).text);
					sb_appendf(&body, "  mov %s, %s\n", opr_to_nasm(ci.dst).text, LR);
				} else {
					sb_appendf(&body, "  mov %s, %s\n", DR, opr_to_nasm(ci.args[0]).text);
					sb_appendf(&body, "  mov %s, %s\n", opr_to_nasm(ci.dst).text, DR);
				}
			}
		} break;

		case OP_ASSIGN: {
			NasmOpr oprd;
			bool fst_asg = false;
			if (ci.dst.as.var.kind == VAR_LOCAL) {
				uint *off = OffTable_get(&stack_table, ci.dst.as.var.addr_id);
				Register *reg = reg_allocator_get(ci.dst.as.var.addr_id);
				if (!off && !reg) {
					fst_asg = true;
					oprd = nasm_gen_new_var(ci);
				} else oprd = opr_to_nasm(ci.dst);
			} else oprd = opr_to_nasm(ci.dst);

			if (ci.dst.as.var.type.kind == TYPE_ARRAY && fst_asg) {
				load_reserved_regs(ci, arg1, arg2);
				stack_offset_add(
					get_type_size(*ci.dst.as.var.type.as.array.elem) *
					ci.dst.as.var.type.as.array.length);

				sb_appendf(&body, "  lea %s, [rbp - %u]\n", arg1, stack_offset);
				sb_appendf(&body, "  mov %s, %s\n", opr_to_nasm(ci.dst).text, arg1);
			}

			if (ci.args[0].kind != OPR_NULL) {
				if (tac_ir_get_opr_type(ci.dst).kind == TYPE_STRUCT) {
					sb_appendf(&body, "  lea rsi, %s\n", opr_to_nasm(ci.args[0]).text);
					sb_appendf(&body, "  lea rdi, %s\n", opr_to_nasm(ci.dst).text);
					sb_appendf(&body, "  mov rcx, %u\n", get_type_size(tac_ir_get_opr_type(ci.dst)));
					sb_appendf(&body, "  rep movsb\n");
				} else {
					nasm_mov(oprd, opr_to_nasm(ci.args[0]));
				}
			} else {
				if (tac_ir_get_opr_type(ci.dst).kind == TYPE_STRUCT) {
					sb_appendf(&body, "  xor rax, rax\n");
					sb_appendf(&body, "  lea rdi, %s\n", opr_to_nasm(ci.dst).text);
					sb_appendf(&body, "  mov rcx, %u\n", get_type_size(tac_ir_get_opr_type(ci.dst)));
					sb_appendf(&body, "  rep stosb\n");
				}
			}
		} break;

		case OP_DEREF: {
			NasmOpr oprd = nasm_gen_new_var(ci);
			sprintf(dst, "%s", oprd.text);
			char ts[32]; opr_type_to_stack(ci.dst, ts);

			if (ci.dst.as.var.type.kind != TYPE_STRUCT) {
				load_reserved_regs(ci, arg1, arg2);
				sb_appendf(&body, "  mov rax, %s\n", opr_to_nasm(ci.args[0]).text);
				sb_appendf(&body, "  mov %s, %s[rax]\n", arg1, ts);
				sb_appendf(&body, "  mov %s, %s\n", opr_to_nasm(ci.dst).text, arg1);
			} else {
				sb_appendf(&body, "  mov rsi, %s\n", opr_to_nasm(ci.args[0]).text);
				sb_appendf(&body, "  lea rdi, %s\n", opr_to_nasm(ci.dst).text);
				sb_appendf(&body, "  mov rcx, %u\n", get_type_size(tac_ir_get_opr_type(ci.dst)));
				sb_appendf(&body, "  rep movsb\n");
			}
		} break;

		case OP_REF: {
			NasmOpr oprd = nasm_gen_new_var(ci);
			sprintf(dst, "%s", oprd.text);
			size_t field_off = get_struct_offset(ci.args[0]);

			if (ci.args[0].as.var.kind == VAR_ADDR) {
				if (ci.args[0].as.var.addr_kind == VAR_LOCAL) {
					uint off = *OffTable_get(&stack_table, ci.args[0].as.var.addr_id);
					sb_appendf(&body, "  mov rax, [rbp - %u]\n", off);
					sb_appendf(&body, "  add rax, %zu\n", field_off);
				} else if (ci.args[0].as.var.addr_kind == VAR_GLOBAL) {
					sb_appendf(&body, "  lea rax, [rel D%u]\n", ci.args[0].as.var.addr_id);
					sb_appendf(&body, "  add rax, %zu\n", field_off);
				}
			} else if (ci.args[0].as.var.kind == VAR_LOCAL) {
				uint off = *OffTable_get(&stack_table, ci.args[0].as.var.addr_id);
				sb_appendf(&body, "  lea rax, [rbp - %u]\n", off - field_off);
			} else if (ci.args[0].as.var.kind == VAR_GLOBAL) {
				sb_appendf(&body, "  lea rax, [rel D%u]\n", ci.args[0].as.var.addr_id);
				sb_appendf(&body, "  add rax, %zu\n", field_off);
			}

			sb_appendf(&body, "  mov %s, rax\n", opr_to_nasm(ci.dst).text);
		} break;

		case OP_JUMP_IF_NOT: {
			load_reserved_regs(ci, arg1, arg2);
			sb_appendf(&body, "  mov %s, %s\n", arg1, opr_to_nasm(ci.args[0]).text);
			sb_appendf(&body, "  cmp %s, 0\n", arg1);
			sb_appendf(&body, "  je %s\n", opr_to_nasm(ci.dst).text);
		} break;

		case OP_LABEL: {
			sb_appendf(&body, "%s:\n", opr_to_nasm(ci.args[0]).text);
		} break;

		case OP_JUMP: {
			sb_appendf(&body, "  jmp %s\n", opr_to_nasm(ci.dst).text);
		} break;

		case OP_RETURN: {
			if (ci.args[0].kind != OPR_NULL) {
				switch (func.type.kind) {
				case TYPE_ARRAY:
				case TYPE_STRUCT:
					assert(!"returning arrays/structs isn't supported yet");
					break;
				case TYPE_FLOAT:
				case TYPE_F32:
				case TYPE_F64:
					nasm_mov(nasm_oprt(REG, func.type, RFf[XMM0]), opr_to_nasm(ci.args[0]));
					break;
				default:;
					size_t row = get_reg_size(func.type);
					nasm_mov(nasm_oprt(REG, func.type, RF[RAX][row]), opr_to_nasm(ci.args[0]));
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

			size_t gn_idx = 0;
			size_t fl_idx = 0;
			size_t sh_idx = 0;

			for (size_t i = 0; ci.args[i].kind != OPR_NULL; i++) {
				char ts[32]; opr_type_to_stack(ci.args[i], ts);
				size_t as = get_reg_size(tac_ir_get_opr_type(ci.args[i]));
				Type at = tac_ir_get_opr_type(ci.args[i]);
				bool is_float = is_type_float(at);

				switch (tp) {
				case TP_MACOS:
				case TP_LINUX:
					if (
						gn_idx >= ARR_LEN(sysv_gn_fa) && !is_float ||
						fl_idx >= ARR_LEN(sysv_fl_fa) &&  is_float
					) {
						uint shadow_space = sh_idx++ * 8 + 32;
						char *addr = tsprintf("%s[rsp + %u]", ts, shadow_space);
						NasmOpr dst = nasm_oprt(MEM, at, addr);
						nasm_mov(dst, opr_to_nasm(ci.args[i]));
					} else {
						if (is_float) {
							NasmOpr dst = nasm_oprt(REG, at, RFf[sysv_fl_fa[fl_idx++]]);
							nasm_mov(dst, opr_to_nasm(ci.args[i]));
						} else {
							NasmOpr dst = nasm_oprt(REG, at, RF[sysv_gn_fa[gn_idx++]][as]);
							nasm_mov(dst, opr_to_nasm(ci.args[i]));
						}
					} break;
				case TP_WINDOWS:
					if (
						gn_idx >= ARR_LEN(win_gn_fa) && !is_float ||
						fl_idx >= ARR_LEN(win_fl_fa) &&  is_float
					) {
						uint shadow_space = sh_idx++ * 8 + 32;
						char *addr = tsprintf("%s[rsp + %u]", ts, shadow_space);
						NasmOpr dst = nasm_oprt(MEM, at, addr);
						nasm_mov(dst, opr_to_nasm(ci.args[i]));
					} else {
						if (is_float) {
							NasmOpr dst = nasm_oprt(REG, at, RFf[win_fl_fa[fl_idx++]]);
							nasm_mov(dst, opr_to_nasm(ci.args[i]));
						} else {
							NasmOpr dst = nasm_oprt(REG, at, RF[win_gn_fa[gn_idx++]][as]);
							nasm_mov(dst, opr_to_nasm(ci.args[i]));
						}
					} break;
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
			sb_appendf(code, "  push %s\n", RF[regal.callee_saved_regs.items[i]][3]);
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
			sb_appendf(code, "  pop %s\n", RF[regal.callee_saved_regs.items[i]][3]);
		}
	}
	sb_appendf(code, "  ret\n\n");
}

char *nasm_gen_prog(TAC_Program *prog, TargetPlatform _tp, int _opt_level) {
	StringBuilder code = {0};
	opt_level = _opt_level;
	tp = _tp;

	sb_appendf(&code, "DEFAULT REL\n\n");

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
						default: UNREACHABLE;
					}
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

	if (floats.count > 0) {
		sb_appendf(&code, "section .data\n");
		for (size_t i = 0; i < floats.count; i++) {
			size_t s = get_reg_size(floats.items[i].type);
			sb_appendf(&code, "  F%zu %s %lf\n", i,
				(char*[]){"db", "dw", "dd", "dq"}[s],
				floats.items[i].value
			);
		}
	}

	return code.items;
}
