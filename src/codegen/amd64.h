#ifndef AMD64_H
#define AMD64_H

#include <tac_ir.h>
#include <cplus.h>

typedef enum {
	RAX, RDX, RCX,
	RDI, RBX, RSI,
	R8,  R9,
	R10, R11, R12,
	R13, R14, R15,

	XMM0, XMM1, XMM2,
	XMM3, XMM4, XMM5,
	XMM6, XMM7, XMM8,
	XMM9, XMM10, XMM11,
	XMM12, XMM13, XMM14,
	XMM15,
} Register;

static char *RF[][4] = {
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

static Register callee_saved [] = {R15, R14, R13, R12, RBX};
static Register sysv_gn_fa   [] = {RDI, RSI, RDX, RCX, R8, R9};
static Register win_gn_fa    [] = {RCX, RDX, R8, R9};
static Register sysv_fl_fa   [] = {XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7};
static Register win_fl_fa    [] = {XMM0, XMM1, XMM2, XMM3};

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
