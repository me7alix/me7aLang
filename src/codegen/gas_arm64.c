#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include <assert.h>
#include <platform.h>
#include <tac_ir.h>

#include "arm64.h"
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

static int CSR = 0;
#define nSR scratch[CSR=(CSR+1)%ARR_LEN(scratch)]
#define SR scratch[CSR]
#define nSRs RF[nSR][3]
#define SRs RF[SR][3]

typedef struct {
	enum {
		REG, MEM,
		IMM, LBL,
	} kind;
	Type type;
	char text[64];
} GasOpr;

GasOpr gas_oprt(int kind, Type type, char *text) {
	GasOpr res = {kind, type};
	sprintf(res.text, "%s", text);
	return res;
}

GasOpr gas_opr(int kind, char *text) {
	GasOpr res = {kind, (Type){TYPE_NULL}};
	sprintf(res.text, "%s", text);
	return res;
}

static void gas_load_imm(const char *reg, bool is64, const char *imm_text) {
	long long signed_val = strtoll(imm_text[0] == '#' ? imm_text + 1 : imm_text, NULL, 0);
	unsigned long long val = is64
		? (unsigned long long) signed_val
		: (unsigned long long)(uint32_t) signed_val;

	int top = is64 ? 64 : 32;
	bool started = false;
	for (int shift = 0; shift < top; shift += 16) {
		unsigned long long chunk = (val >> shift) & 0xffff;
		if (chunk != 0) {
			if (!started) {
				sb_appendf(&body, "  movz %s, #%llu, lsl #%d\n", reg, chunk, shift);
				started = true;
			} else {
				sb_appendf(&body, "  movk %s, #%llu, lsl #%d\n", reg, chunk, shift);
			}
		}
	}
	if (!started) {
		sb_appendf(&body, "  movz %s, #0\n", reg);
	}
}

void gas_mov(GasOpr dst, GasOpr src) {
	char pfx[16] = {0};
	bool is64 = false;
	switch (dst.type.kind) {
	case TYPE_I16:
	case TYPE_U16:
		sprintf(pfx, "h");
		break;
	case TYPE_I8:
	case TYPE_U8:
	case TYPE_BOOL:
		sprintf(pfx, "b");
		break;
	case TYPE_UPTR:
	case TYPE_IPTR:
	case TYPE_POINTER:
		is64 = true;
	}
	char reg[64];
	Type type = dst.type;
	if (dst.type.kind == TYPE_NULL)
		type = src.type;
	uint size = get_reg_size(type);
	sprintf(reg, "%s", RF[nSR][size]);
	if (dst.kind == MEM && src.kind == MEM) {
		sb_appendf(&body, "  ldr%s %s, %s\n", pfx, reg, src.text);
		sb_appendf(&body, "  str%s %s, %s\n", pfx, reg, dst.text);
	} else if (dst.kind == MEM && src.kind == REG) {
		sb_appendf(&body, "  str%s %s, %s\n", pfx, src.text, dst.text);
	} else if (dst.kind == REG && src.kind == REG) {
		sb_appendf(&body, "  mov %s, %s\n", dst.text, src.text);
	} else if (dst.kind == REG && src.kind == MEM) {
		sb_appendf(&body, "  ldr%s %s, %s\n", pfx, dst.text, src.text);
	} else if (dst.kind == REG && src.kind == IMM) {
		gas_load_imm(dst.text, is64, src.text);
	} else if (dst.kind == MEM && src.kind == IMM) {
		gas_load_imm(reg, is64, src.text);
		sb_appendf(&body, "  str%s %s, %s\n", pfx, reg, dst.text);
	} else UNREACHABLE;
}

GasOpr opr_to_gas(TAC_Operand opr) {
	GasOpr res = {0};
	if (opr.kind != OPR_LABEL && opr.kind != OPR_FIELD)
		res.type = tac_ir_get_opr_type(opr);
	switch (opr.kind) {
	case OPR_SIZEOF: {
		uint size = get_type_size(opr.as.size_of.vtype);
		if (opr.as.size_of.vtype.kind == TYPE_ARRAY) {
			uint elemSize = get_type_size(*opr.as.size_of.vtype.as.array.elem);
			size = elemSize * opr.as.size_of.vtype.as.array.length;
		}
		res.kind = IMM;
		sprintf(res.text, "#%u", size);
	} break;

	case OPR_LABEL: {
		res.kind = LBL;
		sprintf(res.text, ".L%u", opr.as.label_id);
	} break;

	case OPR_VAR: {
		uint fo = get_struct_offset(opr);
		res.kind = MEM;
		if (opr.as.var.kind == VAR_LOCAL) {
			uint *off = OffTable_get(&stack_table, opr.as.var.addr_id);
			if (off) {
				sprintf(res.text, "[x29, #-%u]", *off - fo);
			} else {
				res.kind = REG;
				size_t row = get_reg_size(opr.as.var.type);
				Register reg = *RegTable_get(&regal.allocated_regs, opr.as.var.addr_id);
				sprintf(res.text, "%s", RF[reg][row]);
			}
		} else if (opr.as.var.kind == VAR_ADDR) {
			if (opr.as.var.addr_kind == VAR_LOCAL) {
				uint *off = OffTable_get(&stack_table, opr.as.var.addr_id);
				if (off) {
					sb_appendf(&body, "  ldr %s, [x29, #-%u]\n", nSRs, *off);
					if (fo) sprintf(res.text, "[%s, #%u]", SRs, fo);
					else    sprintf(res.text, "[%s]", SRs);
				} else {
					Register reg = *RegTable_get(&regal.allocated_regs, opr.as.var.addr_id);
					if (fo) sprintf(res.text, "[%s, #%u]", RF[reg][3], fo);
					else    sprintf(res.text, "[%s]", RF[reg][3]);
				}
			} else if (opr.as.var.addr_kind == VAR_GLOBAL) {
				sb_appendf(&body, "  adrp %s, D%u\n", nSRs, opr.as.var.addr_id);
				sb_appendf(&body, "  add %s, %s, :lo12:D%u\n", SRs, SRs, opr.as.var.addr_id);
				if (fo) sb_appendf(&body, "  add %S, %s, #%u\n", SRs, SRs, fo);
				sprintf(res.text, "[%s]", SRs);
			} else UNREACHABLE;
		} else if (opr.as.var.kind == VAR_GLOBAL) {
			sb_appendf(&body, "  adrp %s, D%u\n", nSRs, opr.as.var.addr_id);
			sb_appendf(&body, "  add %s, %s, :lo12:D%u\n", SRs, SRs, opr.as.var.addr_id);
			if (fo) sb_appendf(&body, "  add %s, %s, #%u\n", SRs, SRs, fo);
			sprintf(res.text, "[%s]", SRs);
		}
	} break;

	case OPR_LITERAL: {
		res.kind = IMM;
		switch (opr.as.literal.type.kind) {
		case TYPE_I32:
		case TYPE_INT:
			sprintf(res.text, "#%d", (int) opr.as.literal.as.lint);
			break;
		case TYPE_U32:
		case TYPE_UINT:
			sprintf(res.text, "#%u", (uint) opr.as.literal.as.lint);
			break;
		case TYPE_BOOL:
		case TYPE_I8:
			sprintf(res.text, "#%d", (i8) opr.as.literal.as.lint);
			break;
		case TYPE_U8:
			sprintf(res.text, "#%d", (u8) opr.as.literal.as.lint);
			break;
		case TYPE_I16:
			sprintf(res.text, "#%hd", (i16) opr.as.literal.as.lint);
			break;
		case TYPE_U16:
			sprintf(res.text, "#%hu", (u16) opr.as.literal.as.lint);
			break;
		case TYPE_ARRAY:
		case TYPE_POINTER:
		case TYPE_UPTR:
		case TYPE_U64:
			sprintf(res.text, "#%llu", opr.as.literal.as.lint);
			break;
		case TYPE_IPTR:
		case TYPE_I64:
			sprintf(res.text, "#%lli", opr.as.literal.as.lint);
			break;
		default:
			UNREACHABLE;
		}
	} break;

	case OPR_FUNC_RET: {
		res.kind = REG;
		switch (opr.as.func_ret.type.kind) {
		case TYPE_ARRAY:
		case TYPE_STRUCT:
			assert(!"passing arrays or structs is not supported yet");
		default:;
			uint reg_size = get_reg_size(opr.as.func_ret.type);
			sprintf(res.text, "%s", RF[X0][reg_size]);
		}
	} break;

	case OPR_FUNC_INP: {
		uint arg_id = opr.as.func_inp.arg_id;
		size_t arg_size = get_reg_size(opr.as.func_inp.type);
		switch (tp) {
		case TP_MACOS:
		case TP_LINUX:
			if (arg_id >= ARR_LEN(sysv_gn_fa)) {
				uint shadow_space = (arg_id - ARR_LEN(sysv_gn_fa)) * 8 + 48;
				res.kind = MEM;
				sprintf(res.text, "[sp, #%u]", shadow_space);
			} else {
				res.kind = REG;
				sprintf(res.text, "%s", RF[sysv_gn_fa[arg_id]][arg_size]);
			} break;
		case TP_WINDOWS:
			if (arg_id >= ARR_LEN(win_gn_fa)) {
				uint shadow_space = (arg_id - ARR_LEN(win_gn_fa)) * 8 + 48;
				res.kind = MEM;
				sprintf(res.text, "[sp, #%u]", shadow_space);
			} else {
				res.kind = REG;
				sprintf(res.text, "%s", RF[win_gn_fa[arg_id]][arg_size]);
			}
		}
	} break;

	default:
		UNREACHABLE;
	}

	return res;
}

static void type_to_reg(TAC_Operand opr, char *a, char *b, char *c) {
	Type type = tac_ir_get_opr_type(opr);
	switch (type.kind) {
	case TYPE_F32:
	case TYPE_FLOAT:
		//sprintf(a, "%s", RF[V8][get_reg_size(type)]);
		//sprintf(b, "%s", RF[V9][get_reg_size(type)]);
		//sprintf(c, "%s", RF[V10][get_reg_size(type)]);
		UNREACHABLE;
		break;
	default:
		sprintf(a, "%s", RF[X9][get_reg_size(type)]);
		sprintf(b, "%s", RF[X10][get_reg_size(type)]);
		sprintf(c, "%s", RF[X11][get_reg_size(type)]);
	}
}

static void load_reserved_regs(TAC_Instruction inst, char *arg1, char *arg2, char *dst) {
	if (inst.dst.kind == OPR_LABEL) {
		inst.dst.kind = OPR_VAR;
		inst.dst.as.var.type = (Type){.kind = TYPE_BOOL};
		type_to_reg(inst.dst, arg1, arg2, dst);
		return;
	}
	if (inst.dst.as.var.type.kind == TYPE_BOOL) {
		type_to_reg(inst.args[0], arg1, arg2, dst);
		return;
	}
	type_to_reg(inst.dst, arg1, arg2, dst);
}

static void stack_offset_add(uint off) {
	stack_offset += off;
	align_up(&stack_offset, 8);
}

static void load_struct_ptr(const char *reg, TAC_Operand opr) {
	uint fo = get_struct_offset(opr);
	if (opr.as.var.kind == VAR_LOCAL) {
		uint *off = OffTable_get(&stack_table, opr.as.var.addr_id);
		assert(off);
		sb_appendf(&body, "  sub %s, x29, #%u\n", reg, *off - fo);
	} else if (opr.as.var.kind == VAR_ADDR) {
		if (opr.as.var.addr_kind == VAR_LOCAL) {
			uint *off = OffTable_get(&stack_table, opr.as.var.addr_id);
			if (off) {
				sb_appendf(&body, "  ldr %s, [x29, #-%u]\n", reg, *off);
			} else {
				Register r = *RegTable_get(&regal.allocated_regs, opr.as.var.addr_id);
				sb_appendf(&body, "  mov %s, %s\n", reg, RF[r][3]);
			}
			if (fo) sb_appendf(&body, "  add %s, %s, %u\n", reg, reg, fo);
		} else if (opr.as.var.addr_kind == VAR_GLOBAL) {
			sb_appendf(&body, "  adrp %s, D%u\n", nSRs, opr.as.var.addr_id);
			sb_appendf(&body, "  add %s, %s, :lo12:D%u\n", SRs, opr.as.var.addr_id);
			if (fo) sb_appendf(&body, "  add %s, %s, %u", SRs, SRs, fo);
			sb_appendf(&body, "  ldr %s, [%s]\n", reg, SRs);
		}
	} else if (opr.as.var.kind == VAR_GLOBAL) {
		sb_appendf(&body, "  adrp %s, D%u\n", reg, opr.as.var.addr_id);
		sb_appendf(&body, "  add %s, %s, :lo12:D%u\n", reg, reg, opr.as.var.addr_id);
		if (fo) sb_appendf(&body, "  add %s, %s, %u", reg, reg, fo);
	}
}

static void copy_struct(TAC_Operand dst, TAC_Operand src) {
	load_struct_ptr("x0", dst);
	load_struct_ptr("x1", src);
	uint size = get_type_size(tac_ir_get_opr_type(dst));
	sb_appendf(&body, "  mov x2, #%u\n", size);
	sb_appendf(&body, "  bl memcpy\n");
}

GasOpr gas_gen_new_var(TAC_Instruction ci) {
	if (opt_level > 0) {
		reg_allocator_free(&regal, inst_idx);
		if (ci.dst.as.var.type.kind != TYPE_STRUCT) {
			Register reg;
			if (reg_allocator_push(&regal, ci.dst.as.var.addr_id, (int*)&reg)) {
				size_t row = get_reg_size(ci.dst.as.var.type);
				return gas_oprt(REG, ci.dst.as.var.type, RF[reg][row]);
			}
		}
	}
	stack_offset_add(get_type_size(ci.dst.as.var.type));
	OffTable_add(&stack_table, ci.dst.as.var.addr_id, stack_offset);
	char buf[256]; sprintf(buf, "[x29, #-%u]", stack_offset);
	return gas_oprt(MEM, ci.dst.as.var.type, buf);
}

static bool is_signed(Type type) {
	switch (type.kind) {
	case TYPE_ARRAY:
	case TYPE_POINTER:
	case TYPE_UINT: case TYPE_U8:
	case TYPE_U32:  case TYPE_U16:
	case TYPE_U64:  case TYPE_UPTR:
		return false;
	case TYPE_IPTR:
	case TYPE_BOOL: case TYPE_I8:
	case TYPE_INT:  case TYPE_I32:
	case TYPE_I64:  case TYPE_I16:
		return true;
	default:
		UNREACHABLE;
	}
}

void gas_gen_func(StringBuilder *code, TAC_Func func) {
	if (!func.is_static)
		sb_appendf(code, ".global %s\n", func.name);
	sb_appendf(code, "%s%s:\n", (tp == TP_MACOS ? "_" : ""), func.name);

	if (func.body.count == 0) {
		sb_appendf(code, "  ret\n\n");
		return;
	}

	is_there_return = false;
	RegTable_free(&regal.allocated_regs);
	regal.allocated_regs = (RegTable){0};
	regal.life_intervals = &func.var_ints;
	da_reset(&regal.callee_saved_regs);
	da_reset(&regal.available_regs);
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
		res[0] = '/'; res[1] = '/';
		sb_appendf(&body, "%s\n", res);
#endif

		switch (ci.op) {
		case OP_LESS_EQ: case OP_GREAT_EQ:
		case OP_GREAT:   case OP_LESS:
		case OP_EQ:      case OP_NOT_EQ: {
			GasOpr oprd = gas_gen_new_var(ci);
			load_reserved_regs(ci, arg1, arg2, dst);

			GasOpr opr1 = opr_to_gas(ci.args[0]);
			if (opr1.kind != REG) {
				gas_mov(gas_opr(REG, arg1), opr1);
			} else sprintf(arg1, "%s", opr1.text);

			GasOpr opr2 = opr_to_gas(ci.args[1]);
			if (opr2.kind == MEM) {
				gas_mov(gas_opr(REG, arg2), opr2);
			} else sprintf(arg2, "%s", opr2.text);

			if (oprd.kind != REG) {
				gas_mov(gas_opr(REG, dst), oprd);
			} else sprintf(dst,  "%s", oprd.text);

			bool us = !is_signed(tac_ir_get_opr_type(ci.args[0]));
			const char *cond = NULL;
			switch (ci.op) {
				case OP_EQ:       cond = "eq";             break;
				case OP_NOT_EQ:   cond = "ne";             break;
				case OP_GREAT:    cond = us ? "hi" : "gt"; break;
				case OP_LESS:     cond = us ? "lo" : "lt"; break;
				case OP_GREAT_EQ: cond = us ? "hs" : "ge"; break;
				case OP_LESS_EQ:  cond = us ? "ls" : "le"; break;
			}

			if (cond != NULL) {
				sb_appendf(&body, "  cmp %s, %s\n", arg1, arg2);
				sb_appendf(&body, "  cset %s, %s\n", dst, cond);
			}

			if (oprd.kind != REG) {
				gas_mov(oprd, gas_opr(REG, dst));
			}
		} break;

		case OP_ADD:    case OP_SUB:
		case OP_MUL:    case OP_DIV:
		case OP_AND:    case OP_OR:
		case OP_BW_AND: case OP_BW_OR:
		case OP_BW_LS:  case OP_BW_RS:
		case OP_BW_XOR: case OP_MOD: {
			GasOpr oprd = gas_gen_new_var(ci);
			load_reserved_regs(ci, arg1, arg2, dst);

			GasOpr opr1 = opr_to_gas(ci.args[0]);
			if (opr1.kind != REG) {
				gas_mov(gas_opr(REG, arg1), opr1);
			} else sprintf(arg1, "%s", opr1.text);

			GasOpr opr2;
			if (
				ci.op == OP_ADD || ci.op == OP_SUB ||
				ci.op == OP_AND || ci.op == OP_OR  ||
				ci.op == OP_BW_LS || ci.op == OP_BW_RS
			) {
				opr2 = opr_to_gas(ci.args[1]);
				if (opr2.kind == MEM) {
					gas_mov(gas_opr(REG, arg2), opr2);
				} else sprintf(arg2, "%s", opr2.text);
			} else {
				opr2 = opr_to_gas(ci.args[1]);
				if (opr2.kind != REG) {
					gas_mov(gas_opr(REG, arg2), opr2);
				} else sprintf(arg2, "%s", opr2.text);
			}

			if (oprd.kind != REG) {
				gas_mov(gas_opr(REG, dst), oprd);
			} else sprintf(dst,  "%s", oprd.text);

			if      (ci.op == OP_ADD)    sb_appendf(&body, "  add %s, %s, %s\n", dst, arg1, arg2);
			else if (ci.op == OP_SUB)    sb_appendf(&body, "  sub %s, %s, %s\n", dst, arg1, arg2);
			else if (ci.op == OP_AND)    sb_appendf(&body, "  and %s, %s, %s\n", dst, arg1, arg2);
			else if (ci.op == OP_BW_AND) sb_appendf(&body, "  and %s, %s, %s\n", dst, arg1, arg2);
			else if (ci.op == OP_OR)     sb_appendf(&body, "  orr %s, %s, %s\n", dst, arg1, arg2);
			else if (ci.op == OP_BW_OR)  sb_appendf(&body, "  orr %s, %s, %s\n", dst, arg1, arg2);
			else if (ci.op == OP_BW_XOR) sb_appendf(&body, "  eor %s, %s, %s\n", dst, arg1, arg2);
			else if (ci.op == OP_MUL)    sb_appendf(&body, "  mul %s, %s, %s\n", dst, arg1, arg2);
			else if (ci.op == OP_BW_LS)  sb_appendf(&body, "  lsl %s, %s, %s\n", dst, arg1, arg2);
			else if (ci.op == OP_BW_RS)  sb_appendf(&body, "  lsr %s, %s, %s\n", dst, arg1, arg2);

			else if (ci.op == OP_DIV) {
				sb_appendf(&body, "  %sdiv %s, %s, %s\n",
					is_signed(ci.dst.as.var.type) ? "s" : "u",
					dst, arg1, arg2
				);
			}

			else if (ci.op == OP_MOD) {
				uint size = get_reg_size(ci.dst.as.var.type);
				char *reg = RF[nSR][size];
				sb_appendf(&body, "  %sdiv %s, %s, %s\n",
					is_signed(ci.dst.as.var.type) ? "s" : "u",
					reg, arg1, arg2);
				sb_appendf(&body, "  msub %s, %s, %s, %s\n", dst, reg, arg2, arg1);
			}

			if (oprd.kind != REG) {
				gas_mov(oprd, gas_opr(REG, dst));
			}
		} break;

		case OP_NOT:
		case OP_NEG:
		case OP_BW_NOT: {
			GasOpr oprd = gas_gen_new_var(ci);
			load_reserved_regs(ci, arg1, arg2, dst);

			GasOpr opr1 = opr_to_gas(ci.args[0]);
			if (opr1.kind != REG) {
				gas_mov(gas_opr(REG, arg1), opr1);
			} else sprintf(arg1, "%s", opr1.text);

			if (oprd.kind != REG) {
				gas_mov(gas_opr(REG, dst), oprd);
			} else sprintf(dst,  "%s", oprd.text);

			if (ci.op == OP_NEG)
				sb_appendf(&body, "  neg %s, %s\n", dst, arg1);
			else if (ci.op == OP_BW_NOT)
				sb_appendf(&body, "  mnv %s, %s\n", dst, arg1);
			else if (ci.op == OP_NOT) {
				sb_appendf(&body, "  cmp %s, #0\n", arg1);
				sb_appendf(&body, "  cset %s, eq\n", dst);
			}

			if (oprd.kind != REG) {
				gas_mov(oprd, gas_opr(REG, dst));
			}
		} break;

		case OP_ASSIGN: {
			bool fst_asg = false;
			GasOpr oprd;
			if (ci.dst.as.var.kind == VAR_LOCAL) {
				uint *off = OffTable_get(&stack_table, ci.dst.as.var.addr_id);
				Register *reg = (Register*)RegTable_get(&regal.allocated_regs, ci.dst.as.var.addr_id);
				if (!off && !reg) {
					fst_asg = true;
					oprd = gas_gen_new_var(ci);
				} else oprd = opr_to_gas(ci.dst);
			} else oprd = opr_to_gas(ci.dst);

			if (ci.dst.as.var.type.kind == TYPE_ARRAY && fst_asg) {
				load_reserved_regs(ci, arg1, arg2, dst);
				stack_offset_add(
					get_type_size(*ci.dst.as.var.type.as.array.elem) *
					ci.dst.as.var.type.as.array.length);
				if (oprd.kind == REG) sprintf(arg1, "%s", oprd.text);
				sb_appendf(&body, "  add %s, x29, #-%u\n", arg1, stack_offset);
				if (oprd.kind != REG) gas_mov(oprd, gas_opr(REG, arg1));
			}

			if (ci.args[0].kind != OPR_NULL) {
				if (tac_ir_get_opr_type(ci.dst).kind == TYPE_STRUCT) {
					//sb_appendf(&body, "  ldr x0, %s\n", oprd.text);
					//sb_appendf(&body, "  ldr x1, %s\n", opr_to_gas(ci.args[0]).text);
					//sb_appendf(&body, "  mov x2, #%u\n", get_type_size(tac_ir_get_opr_type(ci.dst)));
					//sb_appendf(&body, "  bl memcpy\n");
					copy_struct(ci.dst, ci.args[0]);
				} else gas_mov(oprd, opr_to_gas(ci.args[0]));
			} else {
				if (tac_ir_get_opr_type(ci.dst).kind == TYPE_STRUCT) {
					uint size = get_type_size(tac_ir_get_opr_type(ci.dst));
					load_struct_ptr("x0", ci.dst);
					sb_appendf(&body, "  mov x1, #0\n");
					sb_appendf(&body, "  mov x2, #%u\n", size);
					sb_appendf(&body, "  bl memset\n");
				}
			}
		} break;

		case OP_DEREF: {
			GasOpr oprd = gas_gen_new_var(ci);
			if (ci.dst.as.var.type.kind != TYPE_STRUCT) {
				gas_mov(gas_opr(REG, "x13"), opr_to_gas(ci.args[0]));
				gas_mov(opr_to_gas(ci.dst), gas_opr(MEM, "[x13]"));
			} else {
				copy_struct(ci.dst, ci.args[0]);
			}
		} break;

		case OP_REF: {
			GasOpr oprd = gas_gen_new_var(ci);
			uint fo = get_struct_offset(ci.args[0]);
			if (ci.args[0].as.var.kind == VAR_ADDR) {
				if (ci.args[0].as.var.addr_kind == VAR_LOCAL) {
					uint off = *OffTable_get(&stack_table, ci.args[0].as.var.addr_id);
					sb_appendf(&body, "  ldr %s, [x29, #-%u]\n", nSRs, off);
					sb_appendf(&body, "  add %s, %s, #%u\n", SRs, SRs, fo);
				} else if (ci.args[0].as.var.addr_kind == VAR_GLOBAL) {
					sb_appendf(&body, "  adrp %s, D%u\n", nSRs, ci.args[0].as.var.addr_id);
					sb_appendf(&body, "  add %s, %s, :lo12:D%u\n", SRs, SRs, ci.args[0].as.var.addr_id);
					if (fo) sb_appendf(&body, "  add %s, %s, #%u\n", SRs, SRs, fo);
				} else UNREACHABLE;
			} else if (ci.args[0].as.var.kind == VAR_LOCAL) {
				uint off = *OffTable_get(&stack_table, ci.args[0].as.var.addr_id);
				sb_appendf(&body, "  sub %s, x29, #%u\n", nSRs, off - fo);
			} else if (ci.args[0].as.var.kind == VAR_GLOBAL) {
				sb_appendf(&body, "  adrp %s, D%u\n", nSRs, ci.args[0].as.var.addr_id);
				sb_appendf(&body, "  add %s, %s, :lo12:D%u\n", SRs, SRs, ci.args[0].as.var.addr_id);
				if (fo) sb_appendf(&body, "  add %s, %s, #%u\n", SRs, SRs, fo);
			}
			gas_mov(opr_to_gas(ci.dst), gas_opr(REG, SRs));
		} break;

		case OP_CAST: {
			Type dst_type = ci.dst.as.var.type;
			Type arg1_type = tac_ir_get_opr_type(ci.args[0]);
			GasOpr oprd = gas_gen_new_var(ci);
			load_reserved_regs(ci, arg1, arg2, dst);
			if (oprd.kind == REG) sprintf(dst, "%s", oprd.text);

			if (dst_type.kind == arg1_type.kind)
				UNREACHABLE;

			int dsz = 0;
			int ssz = 0;
			bool ssig = false;

			switch (dst_type.kind) {
				case TYPE_U64:
				case TYPE_UPTR:
				case TYPE_POINTER:
				case TYPE_I64:
				case TYPE_IPTR:
					dsz = 8;
					break;
				case TYPE_INT:
				case TYPE_I32:
				case TYPE_UINT:
				case TYPE_U32:
					dsz = 4;
					break;
				case TYPE_I8:
				case TYPE_U8:
					dsz = 1;
					break;
				case TYPE_I16:
				case TYPE_U16:
					dsz = 2;
					break;
				default:
					UNREACHABLE;
			}

			switch (arg1_type.kind) {
				case TYPE_U64: case TYPE_UPTR: case TYPE_POINTER:
					ssz = 8; ssig = false; break;
				case TYPE_INT: case TYPE_I32:
					ssz = 4; ssig = true; break;
				case TYPE_UINT: case TYPE_U32:
					ssz = 4; ssig = false; break;
				case TYPE_I64: case TYPE_IPTR:
					ssz = 8; ssig = true; break;
				case TYPE_I8:
					ssz = 1; ssig = true; break;
				case TYPE_U8:
					ssz = 1; ssig = false; break;
				case TYPE_I16:
					ssz = 2; ssig = true; break;
				case TYPE_U16:
					ssz = 2; ssig = false; break;
				default:
					UNREACHABLE;
			}

			const char *TR = nSRs;
			const char *TW = RF[SR][2];

			GasOpr src_opr = opr_to_gas(ci.args[0]);
			int actual_ssz = ssz;
			if (src_opr.kind == REG)
				actual_ssz = (src_opr.text[0] == 'x') ? 8 : 4;

			const char *ld_reg = (actual_ssz == 8) ? TR : TW;

			if (src_opr.kind == MEM) {
				const char *ld_op = (actual_ssz == 1) ? "ldrb" : (actual_ssz == 2) ? "ldrh" : "ldr";
				sb_appendf(&body, "  %s %s, %s\n", ld_op, ld_reg, src_opr.text);
			} else {
				sb_appendf(&body, "  mov %s, %s\n", ld_reg, src_opr.text);
			}

			if (dsz > actual_ssz) {
				if (actual_ssz == 1) {
					sb_appendf(&body, ssig ? "  sxtb %s, %s\n" : "  uxtb %s, %s\n", TW, TW);
				} else if (actual_ssz == 2) {
					sb_appendf(&body, ssig ? "  sxth %s, %s\n" : "  uxth %s, %s\n", TW, TW);
				} else if (actual_ssz == 4 && dsz == 8) {
					if (ssig) sb_appendf(&body, "  sxtw %s, %s\n", TR, TW);
				} else UNREACHABLE;

				if (dsz == 8) {
					sb_appendf(&body, "  mov %s, %s\n", dst, TR);
				} else if (dsz == 4) {
					sb_appendf(&body, "  mov %s, %s\n", dst, TW);
				} else if (dsz == 2) {
					sb_appendf(&body, "  and %s, %s, #0xffff\n", TW, TW);
					sb_appendf(&body, "  mov %s, %s\n", dst, TW);
				} else if (dsz == 1) {
					sb_appendf(&body, "  and %s, %s, #0xff\n", TW, TW);
					sb_appendf(&body, "  mov %s, %s\n", dst, TW);
				} else UNREACHABLE;
			} else if (dsz < actual_ssz) {
				if (dsz == 4) {
					sb_appendf(&body, "  mov %s, %s\n", dst, TW);
				} else if (dsz == 2) {
					sb_appendf(&body, "  and %s, %s, #0xffff\n", TW, TW);
					sb_appendf(&body, "  mov %s, %s\n", dst, TW);
				} else if (dsz == 1) {
					sb_appendf(&body, "  and %s, %s, #0xff\n", TW, TW);
					sb_appendf(&body, "  mov %s, %s\n", dst, TW);
				} else {
					UNREACHABLE;
				}
			} else {
				if (dsz == 8) {
					sb_appendf(&body, "  mov %s, %s\n", dst, TR);
				} else {
					sb_appendf(&body, "  mov %s, %s\n", dst, TW);
				}
			}

			if (oprd.kind != REG) {
				gas_mov(oprd, gas_opr(REG, dst));
			}
		} break;

		case OP_JUMP_IF_NOT: {
			load_reserved_regs(ci, arg1, arg2, dst);
			GasOpr opr1 = opr_to_gas(ci.args[0]);
			if (opr1.kind != REG) {
				gas_mov(gas_opr(REG, arg1), opr_to_gas(ci.args[0]));
			} else sprintf(arg1, "%s", opr1.text);
			sb_appendf(&body, "  cmp %s, 0\n", arg1);
			sb_appendf(&body, "  beq %s\n", opr_to_gas(ci.dst).text);
		} break;

		case OP_LABEL: {
			sb_appendf(&body, "%s:\n", opr_to_gas(ci.args[0]).text);
		} break;

		case OP_JUMP: {
			sb_appendf(&body, "  b %s\n", opr_to_gas(ci.dst).text);
		} break;

		case OP_RETURN: {
			if (ci.args[0].kind != OPR_NULL) {
				switch (func.type.kind) {
				case TYPE_STRUCT:
				case TYPE_ARRAY:
					assert(!"returning arrays/structs is not supported yet");
				default:
					gas_mov(gas_opr(REG, RF[X0][get_reg_size(func.type)]), opr_to_gas(ci.args[0]));
				}
			}
			is_there_return = true;
			sb_appendf(&body, "  b 1f\n");
		} break;

		case OP_FUNC_CALL: {
			bool is_shadow_space_used = false;
			for (size_t i = 0; ci.args[i].kind != OPR_NULL; i++) {
				if (i >= ARR_LEN(sysv_gn_fa)) {
					is_shadow_space_used = true;
					sb_appendf(&body, "  sub sp, sp, 32\n");
					break;
				}
			}
			for (size_t i = 0; ci.args[i].kind != OPR_NULL; i++) {
				size_t arg_size = get_reg_size(tac_ir_get_opr_type(ci.args[i]));
				switch (tp) {
				case TP_MACOS:
				case TP_LINUX:
					if (i >= ARR_LEN(sysv_gn_fa)) {
						sb_appendf(&body, "  mov %s, %s\n", RF[X10][arg_size], opr_to_gas(ci.args[i]));
						uint shadow_space = (i - ARR_LEN(sysv_gn_fa)) * 8 + 32;
						sb_appendf(&body, "  mov [sp, %u], %s\n", shadow_space, RF[X10][arg_size]);
					} else {
						gas_mov(gas_opr(REG, RF[sysv_gn_fa[i]][arg_size]), opr_to_gas(ci.args[i]));
					} break;
				case TP_WINDOWS:
					if (i >= ARR_LEN(win_gn_fa)) {
						sb_appendf(&body, "  mov %s, %s\n", RF[X10][arg_size], opr_to_gas(ci.args[i]));
						uint shadow_space = (i - ARR_LEN(win_gn_fa)) * 8 + 32;
						sb_appendf(&body, "  mov [sp, %u], %s\n", shadow_space, RF[X10][arg_size]);
					} else {
						gas_mov(gas_opr(REG, RF[win_gn_fa[i]][arg_size]), opr_to_gas(ci.args[i]));
					}
				}
			}
			sb_appendf(&body, "  bl %s%s\n", (tp == TP_MACOS ? "_" : ""), ci.dst.as.name);
			if (is_shadow_space_used) sb_appendf(&body, "  add sp, sp, 32\n");
		} break;

		default:
			UNREACHABLE;
		}
	}

	bool is_stack_used = stack_offset != 0;
	stack_offset += 48;
	align_up(&stack_offset, 16);

	if (opt_level > 0) {
		for (size_t i = 0; i < regal.callee_saved_regs.count; i++) {
			sb_appendf(code, "  str %s, [sp, #-16]!\n", RF[regal.callee_saved_regs.items[i]][3]);
		}
	}

	sb_appendf(code, "  stp x29, x30, [sp, #-16]!\n");
	if (is_stack_used) {
		sb_appendf(code, "  mov x29, sp\n");
		sb_appendf(code, "  sub sp, sp, #%u\n", stack_offset);
	}

	sb_appendf(code, "%s", body.items);

	if (is_there_return)
		sb_appendf(code, "1:\n");
	if (strcmp(func.name, "main") == 0)
		sb_appendf(code, "  mov w0, 0\n");
	if (is_stack_used) {
		sb_appendf(code, "  add sp, sp, %u\n", stack_offset);
	}
	sb_appendf(code, "  ldp x29, x30, [sp], #16\n");
	if (opt_level > 0) {
		for (long i = (long)regal.callee_saved_regs.count - 1; i >= 0; i--) {
			sb_appendf(code, "  ldr %s, [sp], #16\n", RF[regal.callee_saved_regs.items[i]][3]);
		}
	}
	sb_appendf(code, "  ret\n\n");
}

char *gas_gen_prog(TAC_Program *prog, TargetPlatform _tp, int _opt_level) {
	StringBuilder code = {0};
	opt_level = _opt_level;
	tp = _tp;

	da_foreach(TAC_Extern, ext, &prog->externs)
		sb_appendf(&code, ".extern %s\n", ext->name);
	sb_appendf(&code, "\n");

	sb_appendf(&code, ".section .data\n");
	uint uniq_data_off = 0;

	da_foreach (TAC_GlobalVar, g, &prog->globals) {
		if (g->type.kind == TYPE_ARRAY && g->is_none) {
			uint arr_size = get_type_size(*g->type.as.array.elem) * g->type.as.array.length;
			sb_appendf(&code, "  U%u: .zero %u\n", uniq_data_off, arr_size);
			sb_appendf(&code, "  .align 8\n");
			sb_appendf(&code, "  D%u: .quad U%u\n", g->index, uniq_data_off++);
		} else {
			if (g->is_none) {
				sb_appendf(&code, "  D%u: .zero %u\n", g->index, get_type_size(g->type));
			} else {
				if (g->data.kind == LIT_ARR) {
					sb_appendf(&code, "  U%u: .byte ", uniq_data_off);
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
					sb_appendf(&code, "  .align 8\n");
					sb_appendf(&code, "  D%u: .quad U%u\n", g->index, uniq_data_off++);
				} else if (g->data.kind == LIT_STR) {
					sb_appendf(&code, "  U%u: .byte ", uniq_data_off);
					size_t lit_size = strlen(g->data.as.str) + 1;
					for (size_t i = 0; i < lit_size; i++) {
						sb_appendf(&code, "%#x", (u8) g->data.as.str[i]);
						if (i != lit_size - 1) sb_appendf(&code, ", ");
					}
					sb_appendf(&code, "\n");
					sb_appendf(&code, "  .align 8\n");
					sb_appendf(&code, "  D%u: .quad U%u\n", g->index, uniq_data_off++);
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
	sb_appendf(&code, "  .align 8\n");
	sb_appendf(&code, "\n");
	sb_appendf(&code, ".section .text\n");
	for (size_t i = 0; i < prog->funcs.count; i++) {
		gas_gen_func(&code, da_get(&prog->funcs, i));
	}
	return code.items;
}

