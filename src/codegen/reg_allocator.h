#ifndef REG_ALLOCATOR_H
#define REG_ALLOCATOR_H

#include <stdbool.h>
#include "../../include/tac_ir.h"
#include "../../thirdparty/cplus.h"

HT_DECL(RegTable, uint, int)

typedef DA(int) Registers;

void reg_allocator_free(
	TAC_VarIntervals *var_ints,
	Registers *free_regs,
	RegTable *used_regs,
	uint inst_idx);

bool reg_allocator_push(
	TAC_VarIntervals *var_ints,
	Registers *free_regs,
	RegTable *used_regs,
	Registers *regs_to_save,
	uint vid, int *reg);

#endif
