#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <threads.h>

#include "../include/ir.h"
#include "../include/parser.h"

size_t var_index = 0, label_index;

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

Operand ir_opr_calc(AST_Node *en, Operand l, Operand r, bool *ret) {
	*ret = true;
	if (l.type == OPR_LITERAL && r.type == OPR_LITERAL && en->type == AST_BIN_EXP) {
		if (l.literal.type.kind == r.literal.type.kind) {
			switch (l.literal.type.kind) {
				case TYPE_INT: {
					int32_t lv = (int32_t) l.literal.lint;
					int32_t rv = (int32_t) r.literal.lint;
					int32_t res;

					switch (en->exp_binary.op) {
						case TOK_PLUS:  res = lv + rv; break;
						case TOK_MINUS: res = lv - rv; break;
						case TOK_STAR:  res = lv * rv; break;
						case TOK_SLASH: res = lv / rv; break;
						default: *ret = false; return (Operand) {};
					}

					return (Operand) {
						.type = OPR_LITERAL,
						.literal.kind = LIT_INT,
						.literal.lint = res,
						.literal.type = (Type) {
							.kind = TYPE_INT,
							.size = 4,
						},
					};
				} break;

				case TYPE_I64: {
					int64_t lv = l.literal.lint;
					int64_t rv = r.literal.lint;
					int64_t res;

					switch (en->exp_binary.op) {
						case TOK_PLUS:  res = lv + rv; break;
						case TOK_MINUS: res = lv - rv; break;
						case TOK_STAR:  res = lv * rv; break;
						case TOK_SLASH: res = lv / rv; break;
						default: *ret = false; return (Operand) {};
					}

					return (Operand) {
						.type = OPR_LITERAL,
						.literal.kind = LIT_INT,
						.literal.lint = res,
						.literal.type = (Type) {
							.kind = TYPE_I64,
							.size = 8,
						},
					};
				} break;

				case TYPE_I8: {
					int8_t lv = (int8_t) l.literal.lint;
					int8_t rv = (int8_t) r.literal.lint;
					int8_t res;

					switch (en->exp_binary.op) {
						case TOK_PLUS:  res = lv + rv; break;
						case TOK_MINUS: res = lv - rv; break;
						case TOK_STAR:  res = lv * rv; break;
						case TOK_SLASH: res = lv / rv; break;
						default: *ret = false; return (Operand) {};
					}

					return (Operand) {
						.type = OPR_LITERAL,
						.literal.kind = LIT_INT,
						.literal.lint = res,
						.literal.type = (Type) {
							.kind = TYPE_I8,
							.size = 1,
						},
					};
				} break;

				default: unreachable;
			}
		}
	}

	*ret = false;
	return (Operand) {0};
}

void ir_gen_func_call(Func *func, AST_Node *cn);

Operand ir_gen_expr(Func *func, AST_Node *en) {
	switch (en->type) {
		case AST_LITERAL: {
			return (Operand) {
				.type = OPR_LITERAL,
				.literal = en->literal,
			};
		};

		case AST_VAR: {
			Var v = vt_get(en->var_id);

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

			da_append(&func->body, inst);

			return inst.dst;
		} break;

		case AST_BIN_EXP: {
			Operand l = ir_gen_expr(func, en->exp_binary.l);
			Operand r = ir_gen_expr(func, en->exp_binary.r);

			bool ret;
			Operand calc = ir_opr_calc(en, l, r, &ret);
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

		case AST_UN_EXP: {
			Instruction inst = {
				.arg1 = ir_gen_expr(func, en->exp_unary.v),
				.dst = (Operand){
					.type = OPR_VAR,
					.var.type = en->exp_unary.type,
					.var.index = var_index++,
				},
			};

			switch (en->exp_unary.op) {
				case TOK_COL: inst.op = OP_CAST; break;
				default: unreachable;
			}

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
}

void ir_gen_var_mut(Func *func, AST_Node *cn) {
	Var v = vt_get(cn->var_mut.id);
	Operand res = ir_gen_expr(func, cn->var_mut.exp);

	switch (res.type) {
		case OPR_LITERAL: case OPR_VAR: {
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

		default: unreachable;
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

void ir_gen_body(Func *func, AST_Node *fn) {
	for (size_t i = 0; i < fn->body.stmts.count; i++) {
		AST_Node *cn = da_get(&fn->body.stmts, i);
		switch (cn->type) {
			case AST_VAR_DEF:
				ir_gen_var_def(func, cn);
				break;

			case AST_FUNC_CALL:
				ir_gen_func_call(func, cn);
				break;

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
			} break;

			case AST_FOR_STMT: {
				switch (cn->stmt_for.var->type) {
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
			} break;

			default: unreachable;
		}
	}
}

void ir_gen_func(Program *prog, AST_Node *fn) {
	da_resize(&vt, 0);
	Func func = {.name = fn->func_def.id, .ret_type = fn->func_def.type};
	ir_gen_body(&func, fn->func_def.body);
	da_append(&prog->funcs, func);
}

Program ir_gen_prog(Parser *parser) {
	Program prog = {0};
	label_index = 0;

	for (size_t i = 0; i < parser->st.count; i++) {
		Symbol cs = da_get(&parser->st, i);
		if (cs.type == SBL_FUNC_EXTERN) {
			da_append(&prog.externs, ((Extern){
				.name = cs.id,
				.ret_type = cs.func_extern.type,
			}));
		}
	}

	AST_Node *pn = parser->program;
	for (size_t i = 0; i < pn->program.stmts.count; i++) {
		AST_Node *cn = da_get(&pn->program.stmts, i);
		switch (cn->type) {
			case AST_FUNC_DEF:
				ir_gen_func(&prog, cn);
				break;

			default: unreachable;
		}
	}


	return prog;
}
