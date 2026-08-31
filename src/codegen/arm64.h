#ifndef ARM64_H
#define ARM64_H

#include <tac_ir.h>
#include <cplus.h>

typedef enum {
	X0, X1, X2, X3,
	X4, X5, X6, X7,
	X8, X9, X10, X11,
	X12, X13, X14, X15,
	X16, X17, X18, X19,
	X20, X21, X22, X23,
	X24, X25, X26, X27,
	X28, X29, SP,
} Register;

static char *RF[][4] = {
	[X0]  = {"w0",  "w0",  "w0",  "x0"  },
	[X1]  = {"w1",  "w1",  "w1",  "x1"  },
	[X2]  = {"w2",  "w2",  "w2",  "x2"  },
	[X3]  = {"w3",  "w3",  "w3",  "x3"  },
	[X4]  = {"w4",  "w4",  "w4",  "x4"  },
	[X5]  = {"w5",  "w5",  "w5",  "x5"  },
	[X6]  = {"w6",  "w6",  "w6",  "x6"  },
	[X7]  = {"w7",  "w7",  "w7",  "x7"  },
	[X8]  = {"w8",  "w8",  "w8",  "x8"  },
	[X9]  = {"w9",  "w9",  "w9",  "x9"  },
	[X10] = {"w10", "w10", "w10", "x10" },
	[X11] = {"w11", "w11", "w11", "x11" },
	[X12] = {"w12", "w12", "w12", "x12" },
	[X13] = {"w13", "w13", "w13", "x13" },
	[X14] = {"w14", "w14", "w14", "x14" },
	[X15] = {"w15", "w15", "w15", "x15" },
	[X16] = {"w16", "w16", "w16", "x16" },
	[X17] = {"w17", "w17", "w17", "x17" },
	[X18] = {"w18", "w18", "w18", "x18" },
	[X19] = {"w19", "w19", "w19", "x19" },
	[X20] = {"w20", "w20", "w20", "x20" },
	[X21] = {"w21", "w21", "w21", "x21" },
	[X22] = {"w22", "w22", "w22", "x22" },
	[X23] = {"w23", "w23", "w23", "x23" },
	[X24] = {"w24", "w24", "w24", "x24" },
	[X25] = {"w25", "w25", "w25", "x25" },
	[X26] = {"w26", "w26", "w26", "x26" },
	[X27] = {"w27", "w27", "w27", "x27" },
	[X28] = {"w28", "w28", "w28", "x28" },
	[X29] = {"w29", "w29", "w29", "x29" },
};

static Register scratch      [] = {X12, X13, X14, X15};
static Register callee_saved [] = {X19, X20, X21, X22, X23, X24, X25, X26, X27, X28};
static Register sysv_gn_fa   [] = {X0, X1, X2, X3, X4, X5, X6, X7};
static Register win_gn_fa    [] = {X0, X1, X2, X3, X4, X5, X6, X7};

static size_t get_reg_size(Type t) {
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

static void align_up(uint *x, uint a) {
	if (*x % a != 0) *x += a - *x % a;
}

static uint get_type_alignment(Type type) {
	if (type.kind == TYPE_STRUCT) {
		uint max_al = 0;
		da_foreach (Member, member, &type.as.user->as.ustruct.members) {
			if (member->kind == MBR_FIELD) {
				uint al = get_type_alignment(member->as.field.type);
				if (al > max_al) max_al = al;
			}
		}
		return max_al;
	}
	return 1 << get_reg_size(type);
}

static uint get_type_size(Type type) {
	if (type.kind == TYPE_STRUCT) {
		uint total = 0;
		uint max_align = 1;
		da_foreach (Member, member, &type.as.user->as.ustruct.members) {
			if (member->kind != MBR_FIELD) continue;
			uint align = get_type_alignment(member->as.field.type);
			uint size  = get_type_size(member->as.field.type);
			if (align > max_align) max_align = align;
			align_up(&total, align);
			total += size;
		}
		align_up(&total, max_align);
        return total;
	}
	return 1 << get_reg_size(type);
}

static uint get_struct_offset(TAC_Operand var) {
	uint total = 0;
	if (var.as.var.fields.count == 0)
		return 0;
	for (size_t i = 0; i < var.as.var.fields.count; i++) {
		char *off = da_get(&var.as.var.fields, i);
		da_foreach (Member, member, &var.as.var.type.as.user->as.ustruct.members) {
			if (member->kind != MBR_FIELD) continue;
			uint size  = get_type_size(member->as.field.type);
			uint align = get_type_alignment(member->as.field.type);
			align_up(&total, align);
			if (strcmp(member->as.field.id, off) == 0) {
				var.as.var.type = member->as.field.type;
				break;
			}
			total += size;
		}
	}
	return total;
}

#endif
