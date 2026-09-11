#include "reg_allocator.h"

HT_IMPL_NUM(RegTable, uint, int)

bool reg_allocator_push_ce(RegAllocator *a, uint vid, int *reg) {
	TAC_VarInterval vi = *TAC_VarIntervals_get(a->life_intervals, vid);
	if (vi.to_spill || a->available_ce_regs.count == 0)
		return false;
	*reg = da_pop(&a->available_ce_regs);
	RegTable_add(&a->allocated_ce_regs, vid, *reg);
	da_foreach (int, saved, &a->callee_saved_regs)
		if (*saved == *reg) return true;
	da_append(&a->callee_saved_regs, *reg);
	return true;
}


bool reg_allocator_push_cr(RegAllocator *a, uint vid, int *reg) {
	TAC_VarInterval vi = *TAC_VarIntervals_get(a->life_intervals, vid);
	if (vi.to_spill || a->available_cr_regs.count == 0 || !vi.no_func_calls)
		return false;
	*reg = da_pop(&a->available_cr_regs);
	RegTable_add(&a->allocated_cr_regs, vid, *reg);
	return true;
}

void reg_allocator_pop_ce(RegAllocator *a, uint vid) {
	int reg = *RegTable_get(&a->allocated_ce_regs, vid);
	RegTable_remove(&a->allocated_ce_regs, vid);
	da_append(&a->available_ce_regs, reg);
}

void reg_allocator_pop_cr(RegAllocator *a, uint vid) {
	int reg = *RegTable_get(&a->allocated_cr_regs, vid);
	RegTable_remove(&a->allocated_cr_regs, vid);
	da_append(&a->available_cr_regs, reg);
}

void reg_allocator_free(RegAllocator *a, uint inst_idx) {
	static DA(uint) to_remove_ce = {0};
	da_reset(&to_remove_ce);
	static DA(uint) to_remove_cr = {0};
	da_reset(&to_remove_cr);

	ht_foreach_node (RegTable, n, &a->allocated_ce_regs) {
		TAC_VarInterval vi = *TAC_VarIntervals_get(a->life_intervals, n->key);
		if (vi.end < inst_idx) da_append(&to_remove_ce, n->key);
	}
	ht_foreach_node (RegTable, n, &a->allocated_cr_regs) {
		TAC_VarInterval vi = *TAC_VarIntervals_get(a->life_intervals, n->key);
		if (vi.end < inst_idx) da_append(&to_remove_cr, n->key);
	}

	da_foreach (uint, vid, &to_remove_ce) {
		reg_allocator_pop_ce(a, *vid);
	}
	da_foreach (uint, vid, &to_remove_cr) {
		reg_allocator_pop_cr(a, *vid);
	}
}
