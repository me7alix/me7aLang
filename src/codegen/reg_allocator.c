#include "reg_allocator.h"

HT_IMPL_NUM(RegTable, uint, int)

bool reg_allocator_push(RegAllocator *a, uint vid, int *reg) {
	TAC_VarInterval vi = *TAC_VarIntervals_get(a->life_intervals, vid);
	if (vi.to_spill || a->available_regs.count == 0)
		return false;
	*reg = da_last(&a->available_regs);
	a->available_regs.count--;
	RegTable_add(&a->allocated_regs, vid, *reg);
	da_foreach (int, saved, &a->callee_saved_regs)
		if (*saved == *reg) return true;
	da_append(&a->callee_saved_regs, *reg);
	return true;
}

void reg_allocator_pop(RegAllocator *a, uint vid) {
	int reg = *RegTable_get(&a->allocated_regs, vid);
	RegTable_remove(&a->allocated_regs, vid);
	da_append(&a->available_regs, reg);
}

void reg_allocator_free(RegAllocator *a, uint inst_idx) {
	static DA(uint) to_remove = {0};
	da_reset(&to_remove);
	ht_foreach_node (RegTable, n, &a->allocated_regs) {
		TAC_VarInterval vi = *TAC_VarIntervals_get(a->life_intervals, n->key);
		if (vi.end < inst_idx) da_append(&to_remove, n->key);
	}
	da_foreach (uint, vid, &to_remove) {
		reg_allocator_pop(a, *vid);
	}
}
