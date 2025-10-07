#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <threads.h>

#include "../include/ir.h"
#include "../include/parser.h"
#include "irgen_calc.c"

int64_t var_index = 0;
size_t label_index = 0;

typedef struct {
	char *name;
	Type type;
	bool is_mem_addr;
	int64_t index;
} Variable;

da(Variable) vt = {0};

void vt_add(Variable var) {
	da_append(&vt, var);
}

Variable vt_get(char *name) {
	for (int i = vt.count - 1; i >= 0; i--) {
		Variable cv = da_get(&vt, i);
		if (strcmp(cv.name, name) == 0) {
			return cv;
		}
	}

	return (Variable) {0};
}

void vt_set(char *name, size_t index) {
	for (int i = vt.count - 1; i >= 0; i--) {
		if (strcmp(da_get(&vt, i).name, name) == 0) {
			da_get(&vt, i).index = index;
		}
	}
}

Type iptr = {.kind = TYPE_IPTR};
Type *ir_get_opr_type(Operand *op) {
	switch (op->type) {
		case OPR_VAR:      return &op->var.type;      break;
		case OPR_LITERAL:  return &op->literal.type;  break;
		case OPR_FUNC_INP: return &op->func_inp.type; break;
		case OPR_FUNC_RET: return &op->func_ret.type; break;
		case OPR_SIZEOF:   return &iptr; break;;
		default: unreachable; return NULL;
	}
}

void ir_gen_func_call(Func *func, AST_Node *cn);

bool before_eq;
int64_t last_var;
Operand ir_gen_expr(Func *func, AST_Node *en) {
	if (!en) {
		last_var = -1;
		return (Operand) {.type = OPR_NULL};
	}

	switch (en->kind) {
		case AST_LITERAL: {
			last_var = -1;
			return (Operand) {
				.type = OPR_LITERAL,
				.literal = en->literal,
			};
		};

		case AST_VAR: {
			Variable v = vt_get(en->var_id);
			last_var = -1;
			return (Operand) {
				.type = OPR_VAR,
				.var.type = v.type,
				.var.index = v.index,
			};
		} break;

		case AST_FUNC_CALL: {
			ir_gen_func_call(func, en);

			Operand res = {
				.type = OPR_FUNC_RET,
				.func_ret.type = en->func_call.type,
			};

			Instruction inst = {
				.op = OP_ASSIGN,
				.arg1 = res,
				.dst = (Operand) {
					.type = OPR_VAR,
					.var.type = en->func_call.type,
					.var.index = var_index++,
				},
			};

			last_var = inst.dst.var.index;
			da_append(&func->body, inst);
			return inst.dst;
		} break;

		case AST_BIN_EXP: {
			Operand l = ir_gen_expr(func, en->exp_binary.l);
			Operand r = ir_gen_expr(func, en->exp_binary.r);

			bool ret; Operand calc = ir_opr_calc(en, l, r, &ret);
			if (ret) return calc;

			Instruction inst = {
				.arg1 = l,
				.arg2 = r,
				.dst = (Operand){
					.type = OPR_VAR,
					.var.type = en->exp_binary.type,
					.var.index = var_index++,
				},
			};

			switch (en->exp_binary.op) {
				case AST_OP_ADD:      inst.op = OP_ADD;      break;
				case AST_OP_SUB:      inst.op = OP_SUB;      break;
				case AST_OP_MUL:      inst.op = OP_MUL;      break;
				case AST_OP_DIV:      inst.op = OP_DIV;      break;
				case AST_OP_LESS:     inst.op = OP_LESS;     break;
				case AST_OP_LESS_EQ:  inst.op = OP_LESS_EQ;  break;
				case AST_OP_GREAT:    inst.op = OP_GREAT;    break;
				case AST_OP_GREAT_EQ: inst.op = OP_GREAT_EQ; break;
				case AST_OP_EQ:       inst.op = OP_EQ;       break;
				case AST_OP_NOT_EQ:   inst.op = OP_NOT_EQ;   break;
				case AST_OP_AND:      inst.op = OP_AND;      break;
				case AST_OP_OR:       inst.op = OP_OR;       break;
				default: unreachable;
			}

			// pointers arithmetic
			if (inst.op == OP_ADD || inst.op == OP_SUB) {
				Type lt = *ir_get_opr_type(&l);
				Type rt = *ir_get_opr_type(&r);

				if (is_pointer(lt) || is_pointer(rt)) {
					Type ptr_base;
					Operand tm;
					if (is_pointer(lt)) {
						ptr_base = *lt.pointer.base;
						tm = r;
					} else if (is_pointer(rt)) {
						ptr_base = *rt.pointer.base;
						tm = l;
					}

					Operand dst = (Operand){
						.type = OPR_VAR,
						.var.type = en->exp_binary.type,
						.var.index = var_index++,
					};

					Instruction mult = {
						.op = OP_MUL,
						.arg1 = tm,
						.arg2 = (Operand) {
							.type = OPR_SIZEOF,
							.size_of.type = (Type) {.kind = TYPE_IPTR},
							.size_of.v_type = ptr_base,
						},
						.dst = dst,
					};

					da_append(&func->body, mult);
					if (is_pointer(lt)) inst.arg2 = dst;
					if (is_pointer(rt)) inst.arg1 = dst;
				}
			}

			last_var = inst.dst.var.index;
			da_append(&func->body, inst);
			return inst.dst;
		} break;

		case AST_UN_EXP: {
			if (en->exp_unary.op == AST_OP_SIZEOF) {
				Operand arg1 = ir_gen_expr(func, en->exp_unary.v);
				last_var = -1;
				return (Operand) {
					.type = OPR_SIZEOF,
					.size_of.type = en->exp_unary.type,
					.size_of.v_type = *ir_get_opr_type(&arg1),
				};
			} else if (en->exp_unary.op == AST_OP_DEREF) {
				Operand arg1 = ir_gen_expr(func, en->exp_unary.v);
				Operand ret = (Operand) {
					.type = OPR_VAR,
					.var.type = en->exp_unary.type,
					.var.is_mem_addr = true,
					.var.index = arg1.var.index,
				};

				if (!before_eq) {
					last_var = -1;
					return ret;
				} else {
					da_append(&func->body, ((Instruction){
						.op = OP_ASSIGN,
						.arg1 = ret,
						.dst = (Operand) {
							.type = OPR_VAR,
							.var.type = en->exp_unary.type,
							.var.index = var_index++,
						}
					}));
					return da_last(&func->body).dst;
				}
			}

			Operand arg = ir_gen_expr(func, en->exp_unary.v);
			bool ret; Operand calc = ir_opr_calc(en, arg, (Operand){}, &ret);
			if (ret) return calc;

			Instruction inst = {
				.arg1 = arg,
				.dst = (Operand){
					.type = OPR_VAR,
					.var.type = en->exp_unary.type,
					.var.index = var_index++,
				},
			};

			switch (en->exp_unary.op) {
				case AST_OP_CAST:  inst.op = OP_CAST;  break;
				case AST_OP_NOT:   inst.op = OP_NOT;   break;
				case AST_OP_NEG:   inst.op = OP_NEG;   break;
				case AST_OP_REF:   inst.op = OP_REF;   break;
				default: unreachable;
			}

			if (inst.op == OP_CAST) {
				Type *lt = ir_get_opr_type(&inst.arg1);
				Type *rt = ir_get_opr_type(&inst.dst);
				if (lt->kind == rt->kind || (is_pointer(*lt) && is_pointer(*rt))) {
					inst.op = OP_ASSIGN;
				}
			}

			last_var = inst.dst.var.index;
			da_append(&func->body, inst);
			return inst.dst;
		} break;

		default: unreachable;
	}

	return (Operand){0};
}

void ir_dump_opr(Operand opr, char *buf);

void ir_gen_var_def(Func *func, AST_Node *cn) {
	Operand res = ir_gen_expr(func, cn->var_def.exp);
	if (last_var == -1) {
		da_append(&func->body, ((Instruction){
			.op = OP_ASSIGN,
			.arg1 = res,
			.dst = (Operand) {
				.type = OPR_VAR,
				.var.type = cn->var_def.type,
				.var.index = var_index,
			},
		}));

		vt_add((Variable){
			.name = cn->var_def.id,
			.type = cn->var_def.type,
			.index = var_index++,
		});
	} else {
		vt_add((Variable){
			.name = cn->var_def.id,
			.type = cn->var_def.type,
			.index = last_var,
		});
	}
}

void ir_gen_var_mut(Func *func, AST_Node *cn) {
	before_eq = false;
	Operand dst = ir_gen_expr(func, cn->var_mut.exp->exp_binary.l);
	before_eq = true;
	Operand res = ir_gen_expr(func, cn->var_mut.exp->exp_binary.r);
	before_eq = false;

	OpCode op_eq;
	bool is_op_eq;
	switch (cn->var_mut.exp->exp_binary.op) {
		case AST_OP_ADD_EQ: op_eq = OP_ADD; is_op_eq = true; break;
		case AST_OP_SUB_EQ: op_eq = OP_SUB; is_op_eq = true; break;
		case AST_OP_MUL_EQ: op_eq = OP_MUL; is_op_eq = true; break;
		case AST_OP_DIV_EQ: op_eq = OP_DIV; is_op_eq = true; break;
		default: is_op_eq = false;
	}

	if (is_op_eq) {
		Instruction op_eq_res = {
			.op = op_eq,
			.arg1 = dst,
			.arg2 = res,
			.dst = (Operand) {
				.type = OPR_VAR,
				.var.type = cn->var_mut.type,
				.var.index = var_index++,
			},
		};

		da_append(&func->body, op_eq_res);
		da_append(&func->body, ((Instruction){
			.op = OP_ASSIGN,
			.arg1 = op_eq_res.dst,
			.dst = dst,
		}));
	} else {
		da_append(&func->body, ((Instruction){
			.op = OP_ASSIGN,
			.arg1 = res,
			.dst = dst,
		}));
	}
}

void ir_gen_func_call(Func *func, AST_Node *cn) {
	Instruction func_call = {
		.op = OP_FUNC_CALL,
		.dst = (Operand) {
			.type = OPR_NAME,
			.name = cn->func_call.id,
		},
	};

	assert(cn->func_call.args.count < 7);
	for (size_t i = 0; i < cn->func_call.args.count; i++) {
		AST_Node *arg = da_get(&cn->func_call.args, i);
		func_call.args[i] = ir_gen_expr(func, arg);
	}

	func_call.args[cn->func_call.args.count] = (Operand) {.type = OPR_NULL};
	da_append(&func->body,  func_call);
}

size_t lbl_st, lbl_ex;
AST_Node *for_var_mut;
int loop_gen;

void ir_gen_body(Func *func, AST_Node *fn);

void ir_gen_if_chain(Func *func, AST_Node *cn) {
	Operand res = ir_gen_expr(func, cn->stmt_if.exp);
	size_t lbl = label_index++;
	size_t lbl_ex = label_index++;

	da_append(&func->body, ((Instruction){
		.op = OP_JUMP_IF_NOT,
		.arg1 = res,
		.dst = (Operand) {
			.type = OPR_LABEL,
			.label_index = lbl,
		},
	}));

	ir_gen_body(func, cn->stmt_if.body);

	if (cn->stmt_if.next) {
		da_append(&func->body, ((Instruction){
			.op = OP_JUMP,
			.dst = (Operand) {
				.type = OPR_LABEL,
				.label_index = lbl_ex,
			},
		}));
	}

	da_append(&func->body, ((Instruction){
		.op = OP_LABEL,
		.arg1 = (Operand) {
			.type = OPR_LABEL,
			.label_index = lbl,
		},
	}));

	if (cn->stmt_if.next) {
		if (cn->stmt_if.next->kind == AST_IF_STMT) {
			ir_gen_if_chain(func, cn->stmt_if.next);
		} else if (cn->stmt_if.next->kind == AST_ELSE_STMT) {
			ir_gen_body(func, cn->stmt_if.next->stmt_else.body);
		} else unreachable;

		da_append(&func->body, ((Instruction){
			.op = OP_LABEL,
			.arg1 = (Operand) {
				.type = OPR_LABEL,
				.label_index = lbl_ex,
			},
		}));
	}
}

void ir_gen_body(Func *func, AST_Node *fn) {
	for (size_t i = 0; i < fn->body.stmts.count; i++) {
		AST_Node *cn = da_get(&fn->body.stmts, i);
		switch (cn->kind) {
			case AST_VAR_DEF: ir_gen_var_def(func, cn); break;
			case AST_FUNC_CALL: ir_gen_func_call(func, cn); break;

			case AST_FUNC_RET: {
				Operand res = ir_gen_expr(func, cn->func_ret.exp);
				switch (res.type) {
					case OPR_LITERAL: {
						da_append(&func->body, ((Instruction){
							.op = OP_RETURN,
							.arg1 = res,
						}));
					} break;

					case OPR_VAR: {
						da_append(&func->body, ((Instruction){
							.op = OP_RETURN,
							.arg1 = (Operand) {
								.type = OPR_VAR,
								.var.type = cn->func_ret.type,
								.var.index = res.var.index,
							},
						}));
					} break;

					default: unreachable;
				}
			} break;

			case AST_VAR_MUT: {
				ir_gen_var_mut(func, cn);
			} break;

			case AST_IF_STMT: {
				ir_gen_if_chain(func, cn);
			} break;

			case AST_LOOP_BREAK:
				if (loop_gen <= 0) lexer_error(cn->loc, "irgen error: break outside of a loop");
				da_append(&func->body, ((Instruction){
					.op = OP_JUMP,
					.dst = (Operand) {
						.type = OPR_LABEL,
						.label_index = lbl_ex,
					},
				}));
				break;

			case AST_LOOP_CONTINUE:
				if (loop_gen <= 0) lexer_error(cn->loc, "irgen error: continue outside of a loop");
				if (for_var_mut) ir_gen_var_mut(func, for_var_mut);
				da_append(&func->body, ((Instruction){
					.op = OP_JUMP,
					.dst = (Operand) {
						.type = OPR_LABEL,
						.label_index = lbl_st,
					},
				}));
				break;

			case AST_WHILE_STMT: {
				loop_gen++;
				lbl_st = label_index++;

				da_append(&func->body, ((Instruction){
					.op = OP_LABEL,
					.arg1 = (Operand) {
						.type = OPR_LABEL,
						.label_index = lbl_st,
					},
				}));

				Operand res = ir_gen_expr(func, cn->stmt_while.exp);
				lbl_ex = label_index++;

				da_append(&func->body, ((Instruction){
					.op = OP_JUMP_IF_NOT,
					.arg1 = res,
					.dst = (Operand) {
						.type = OPR_LABEL,
						.label_index = lbl_ex,
					},
				}));

				ir_gen_body(func, cn->stmt_while.body);

				da_append(&func->body, ((Instruction){
					.op = OP_JUMP,
					.dst = (Operand) {
						.type = OPR_LABEL,
						.label_index = lbl_st,
					},
				}));

				da_append(&func->body, ((Instruction){
					.op = OP_LABEL,
					.arg1 = (Operand) {
						.type = OPR_LABEL,
						.label_index = lbl_ex,
					},
				}));
				loop_gen--;
			} break;

			case AST_FOR_STMT: {
				loop_gen++;
				switch (cn->stmt_for.var->kind) {
					case AST_VAR_MUT: ir_gen_var_mut(func, cn->stmt_for.var); break;
					case AST_VAR_DEF: ir_gen_var_def(func, cn->stmt_for.var); break;
					default: unreachable;
				}

				size_t lbl_st = label_index++;

				da_append(&func->body, ((Instruction){
					.op = OP_LABEL,
					.arg1 = (Operand) {
						.type = OPR_LABEL,
						.label_index = lbl_st,
					},
				}));

				Operand res = ir_gen_expr(func, cn->stmt_for.exp);
				size_t lbl_ex = label_index++;

				da_append(&func->body, ((Instruction){
					.op = OP_JUMP_IF_NOT,
					.arg1 = res,
					.dst = (Operand) {
						.type = OPR_LABEL,
						.label_index = lbl_ex,
					},
				}));

				for_var_mut = cn->stmt_for.mut;
				ir_gen_body(func, cn->stmt_for.body);
				ir_gen_var_mut(func, cn->stmt_for.mut);

				da_append(&func->body, ((Instruction){
					.op = OP_JUMP,
					.dst = (Operand) {
						.type = OPR_LABEL,
						.label_index = lbl_st,
					},
				}));

				da_append(&func->body, ((Instruction){
					.op = OP_LABEL,
					.arg1 = (Operand) {
						.type = OPR_LABEL,
						.label_index = lbl_ex,
					},
				}));
				loop_gen--;
				for_var_mut = NULL;
			} break;

			default: unreachable;
		}
	}
}

void ir_gen_func(Program *prog, AST_Node *fn) {
	da_resize(&vt, 0);
	Func func = {.name = fn->func_def.id, .ret_type = fn->func_def.type};

	for (size_t i = 0; i < fn->func_def.args.count; i++) {
		AST_Node *cn = da_get(&fn->func_def.args, i);
		da_append(&func.body, ((Instruction) {
			.op = OP_ASSIGN,
			.arg1 = (Operand) {
				.type = OPR_FUNC_INP,
				.func_inp.type = cn->func_def_arg.type,
				.func_inp.arg_ind = i,
			},
			.dst = (Operand) {
				.type = OPR_VAR,
				.var.type = cn->func_def_arg.type,
				.var.index = var_index,
			},
		}));
		vt_add((Variable) {
			.type = cn->func_def_arg.type,
			.index = var_index++,
			.name = cn->func_def_arg.id,
		});
	}

	ir_gen_body(&func, fn->func_def.body);
	da_append(&prog->funcs, func);
}

Program ir_gen_prog(Parser *p) {
	Program prog = {0};
	label_index = 0;

	for (size_t i = 0; i < p->symbols.count; i++) {
		Symbol cs = da_get(&p->symbols, i);
		if (cs.type == SBL_FUNC_EXTERN) {
			da_append(&prog.externs, ((Extern){
				.name = cs.func_extern.extern_smb,
				.ret_type = cs.func_extern.type,
			}));
		}
	}

	AST_Node *pn = p->program;
	for (size_t i = 0; i < pn->program.stmts.count; i++) {
		AST_Node *cn = da_get(&pn->program.stmts, i);
		switch (cn->kind) {
			case AST_FUNC_DEF:
				ir_gen_func(&prog, cn);
				break;

			default: unreachable;
		}
	}

	return prog;
}
