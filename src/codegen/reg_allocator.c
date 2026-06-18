#include "reg_allocator.h"

HT_IMPL_NUM(RegTable, uint, int)

bool reg_allocator_push(
	TAC_VarIntervals *var_ints,
	Registers *free_regs,
	RegTable *used_regs,
	Registers *regs_to_save,
	uint vid, int *reg
) {
	TAC_VarInterval vi = *TAC_VarIntervals_get(var_ints, vid);
	if (vi.to_spill || free_regs->count == 0) {
		return false;
	}
	*reg = da_last(free_regs);
	free_regs->count--;
	RegTable_add(used_regs, vid, *reg);
	bool to_save = true;
	da_foreach (int, saved, regs_to_save) {
		if (*saved == *reg) {
			to_save = false;
			break;
		}
	}
	if (to_save) {
		da_append(regs_to_save, *reg);
	}
	return true;
}

void reg_allocator_pop(
	Registers *free_regs,
	RegTable *used_regs,
	uint vid
) {
	int reg = *RegTable_get(used_regs, vid);
	RegTable_remove(used_regs, vid);
	da_append(free_regs, reg);
}

void reg_allocator_free(
	TAC_VarIntervals *var_ints,
	Registers *free_regs,
	RegTable *used_regs,
	uint inst_idx
) {
	static DA(uint) to_remove = {0};
	da_reset(&to_remove);
	ht_foreach_node (RegTable, n, used_regs) {
		TAC_VarInterval vi = *TAC_VarIntervals_get(var_ints, n->key);
		if (vi.end < inst_idx) da_append(&to_remove, n->key);
	}
	da_foreach (uint, vid, &to_remove) {
		reg_allocator_pop(free_regs, used_regs, *vid);
	}
}
