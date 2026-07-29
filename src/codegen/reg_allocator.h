#ifndef REG_ALLOCATOR_H
#define REG_ALLOCATOR_H

#include <stdbool.h>
#include <tac_ir.h>
#include <cplus.h>

HT_DECL(RegTable, uint, int)
typedef DA(int) Registers;

typedef struct {
	TAC_VarIntervals *life_intervals;
	RegTable allocated_regs;
	Registers available_regs;
	Registers callee_saved_regs;
} RegAllocator;

void reg_allocator_free(RegAllocator *a, uint inst_idx);
bool reg_allocator_push(RegAllocator *a, uint vid, int *reg);

#endif
