#include <stdlib.h>
#include <assert.h>

#include "../include/ir.h"
#include "../include/parser.h"

size_t var_index, label_index;

typedef struct {
	char *name;
	Type type;
	size_t index;
} Var;

da(Var) vt = {0};

void vt_add(Var var) {
	da_append(&vt, var);
}

Var vt_get(char *name) {
	for (int i = vt.count - 1; i >= 0; i--) {
		Var cv = da_get(&vt, i);
		if (strcmp(cv.name, name) == 0) {
			return cv;
		}
	}

	return (Var) {0};
}

void vt_set(char *name, size_t index) {
	for (int i = vt.count - 1; i >= 0; i--) {
		if (strcmp(da_get(&vt, i).name, name) == 0) {
			da_get(&vt, i).index = index;
		}
	}
}

Operand ir_gen_expr(Func *func, AST_Node *en) {
	switch (en->type) {
		case AST_INT:
			return (Operand) {
				.type = OPR_IMM_INT,
				.imm_int = en->num_int,
			};

		case AST_FLOAT:
			return (Operand) {
				.type = OPR_IMM_FLOAT,
				.imm_float = en->num_float,
			};

		case AST_VAR: {
			Var v = vt_get(en->var_id);

			return (Operand) {
				.type = OPR_VAR,
				.var.type = v.type,
				.var.index = v.index,
			};

		} break;

		case AST_BIN_EXP: {
			Operand l = ir_gen_expr(func, en->exp_binary.l);
			Operand r = ir_gen_expr(func, en->exp_binary.r);

			// to make this work correctly, i need to have imm for each TYPE
			if (l.type == OPR_IMM_INT && r.type == OPR_IMM_INT) {
				switch (en->exp_binary.op) {
					case TOK_PLUS:
						return (Operand) {.type = OPR_IMM_INT, .imm_int = l.imm_int + r.imm_int };
					case TOK_MINUS:
						return (Operand) {.type = OPR_IMM_INT, .imm_int = l.imm_int - r.imm_int };
					case TOK_STAR:
						return (Operand) {.type = OPR_IMM_INT, .imm_int = l.imm_int * r.imm_int };
					case TOK_SLASH:
						return (Operand) {.type = OPR_IMM_INT, .imm_int = l.imm_int / r.imm_int };
					default: break;
				}
			}

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
				case TOK_PLUS:     inst.op = OP_ADD;      break;
				case TOK_MINUS:    inst.op = OP_SUB;      break;
				case TOK_STAR:     inst.op = OP_MUL;      break;
				case TOK_SLASH:    inst.op = OP_DIV;      break;
				case TOK_LESS:     inst.op = OP_LESS;     break;
				case TOK_LESS_EQ:  inst.op = OP_LESS_EQ;  break;
				case TOK_GREAT:    inst.op = OP_GREAT;    break;
				case TOK_GREAT_EQ: inst.op = OP_GREAT_EQ; break;
				case TOK_EQ_EQ:    inst.op = OP_EQ;       break;
				case TOK_NOT_EQ:   inst.op = OP_NOT_EQ;   break;
				case TOK_AND:      inst.op = OP_AND;      break;
				case TOK_OR:       inst.op = OP_OR;       break;
				default:           assert(!"unreachable");
			}

			da_append(&func->body, inst);
			return inst.dst;
		} break;

		case AST_UN_EXP: break;

		default: assert(!"unreachable");
	}

	return (Operand){0};
}

void ir_gen_body(Func *func, AST_Node *fn) {
	for (size_t i = 0; i < fn->body.stmts.count; i++) {
		AST_Node *cn = da_get(&fn->body.stmts, i);
		switch (cn->type) {
			case AST_VAR_DEF: {
				Operand res = ir_gen_expr(func, cn->var_def.exp);
				switch (res.type) {
					case OPR_IMM_FLOAT: case OPR_IMM_INT: case OPR_VAR: {
						da_append(&func->body, ((Instruction){
							.op = OP_ASSIGN,
							.arg1 = res,
							.dst = (Operand) {
								.type = OPR_VAR,
								.var.type = cn->var_def.type,
								.var.index = var_index,
							},
						}));

						vt_add((Var){
							.name = cn->var_def.id,
							.type = cn->var_def.type,
							.index = var_index++,
						});
					} break;

					default: assert(!"unreachable");
				}
			} break;

			case AST_FUNC_RET: {
				Operand res = ir_gen_expr(func, cn->func_ret.exp);
				switch (res.type) {
					case OPR_IMM_FLOAT: case OPR_IMM_INT: {
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

					default: assert(!"unreachable");
				}
			} break;

			case AST_VAR_MUT: {
				Var v = vt_get(cn->var_mut.id);
				Operand res = ir_gen_expr(func, cn->var_mut.exp);

				switch (res.type) {
					case OPR_IMM_FLOAT: case OPR_IMM_INT: case OPR_VAR: {
						da_append(&func->body, ((Instruction){
							.op = OP_ASSIGN,
							.arg1 = res,
							.dst = (Operand) {
								.type = OPR_VAR,
								.var.type = cn->var_mut.type,
								.var.index = v.index,
							},
						}));
					} break;

					default: assert(!"unreachable");
				}
			} break;


			case AST_IF_STMT: {
				Operand res = ir_gen_expr(func, cn->stmt_if.exp);
				size_t lbl = label_index++;

				da_append(&func->body, ((Instruction){
					.op = OP_JUMP_IF_NOT,
					.arg1 = res,
					.dst = (Operand) {
						.type = OPR_LABEL,
						.label_index = lbl,
					},
				}));

				ir_gen_body(func, cn->stmt_if.body);

				da_append(&func->body, ((Instruction){
					.op = OP_LABEL,
					.arg1 = (Operand) {
						.type = OPR_LABEL,
						.label_index = lbl,
					},
				}));
			} break;

			case AST_WHILE_STMT: {
				size_t lbl_st = label_index++;

				da_append(&func->body, ((Instruction){
					.op = OP_LABEL,
					.arg1 = (Operand) {
						.type = OPR_LABEL,
						.label_index = lbl_st,
					},
				}));

				Operand res = ir_gen_expr(func, cn->stmt_while.exp);

				size_t lbl_ex = label_index++;

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
					.arg1 = (Operand) {
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
			} break;

			default: assert(!"unreachable");
		}
	}
}

void ir_gen_func(Program *prog, AST_Node *fn) {
	var_index = 0;
	da_resize(&vt, 0);
	Func func = {.name = fn->func_def.id, .ret_type = fn->func_def.type};
	ir_gen_body(&func, fn->func_def.body);
	da_append(&prog->funcs, func);
}

Program *ir_gen_prog(AST_Node *pn) {
	Program *prog = calloc(1, sizeof(Program));
	label_index = 0;

	for (size_t i = 0; i < pn->program.stmts.count; i++) {
		AST_Node *cn = da_get(&pn->program.stmts, i);
		switch (cn->type) {
			case AST_FUNC_DEF:
				ir_gen_func(prog, cn);
				break;

			default: assert(!"unreachable");
		}
	}

	return prog;
}
