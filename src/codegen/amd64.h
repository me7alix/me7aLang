#ifndef AMD64_H
#define AMD64_H

#include "../../include/tac_ir.h"
#include "../../thirdparty/cplus.h"

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

static const char *reg_forms[][4] = {
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

HT_DECL(OffTable, uint, uint)

size_t get_reg_size(Type t);

#endif
