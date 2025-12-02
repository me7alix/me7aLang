#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "../include/tac_ir.h"
#include "../include/parser.h"
#include "tac_ir_gen_calc.c"

uint data_id  = 1;
uint var_id   = 1;
uint label_id = 1;

HT_STR(ASTVarTable, TAC_Operand)
ASTVarTable avt = {0};

Type tac_ir_get_opr_type(TAC_Operand op) {
	static Type tuptr = {.kind = TYPE_UPTR};
	switch (op.kind) {
		case OPR_VAR: {
			if (op.var.type.kind == TYPE_STRUCT) {
				Type res = op.var.type;
				for (size_t i = 0; i < op.var.fields.count; i++) {
					char *off = da_get(&op.var.fields, i);
					da_foreach (Field, field, &op.var.type.user->ustruct.fields) {
						if (strcmp(field->id, off) == 0) {
							op.var.type = field->type;
							res = field->type;
							break;
						}
					}
				}
				return res;
			}
			return op.var.type;
		} break;
		case OPR_LITERAL:  return op.literal.type;
		case OPR_FUNC_INP: return op.func_inp.type;
		case OPR_FUNC_RET: return op.func_ret.type;
		case OPR_SIZEOF:   return tuptr;
		default: UNREACHABLE; return (Type) {0};
	}
}

typedef struct {
	bool is_right_of_eq;
	bool is_field_gen;
	bool is_field_op;
	uint last_var;
} IRGenExprCtx;

void tac_ir_gen_func_call(TAC_Program *prog, TAC_Func *func, AST_Node *cn);

TAC_Operand tac_ir_gen_deref(IRGenExprCtx *ctx, TAC_Func *func, Type type, TAC_Operand var) {
	TAC_Operand ret = (TAC_Operand) {
		.kind = OPR_VAR,
		.var.kind = VAR_ADDR,
		.var.type = type,
		.var.addr_id = var.var.addr_id,
		.var.addr_kind = var.var.kind,
	};

	if (!ctx->is_right_of_eq) {
		ctx->last_var = 0;
		return ret;
	} else {
		da_append(&func->body, ((TAC_Instruction){
			.op = OP_ASSIGN,
			.arg1 = ret,
			.dst = (TAC_Operand) {
				.kind = OPR_VAR,
				.var.type = type,
				.var.addr_id = var_id++,
			}
		}));

		return da_last(&func->body).dst;
	}
}

TAC_Operand tac_ir_gen_expr(IRGenExprCtx *ctx, TAC_Program *prog, TAC_Func *func, AST_Node *en) {
	if (!en) {
		ctx->last_var = 0;
		return (TAC_Operand) {.kind = OPR_NULL};
	}

	static Type TI8 = {.kind = TYPE_I8};
	switch (en->kind) {
		case AST_LITERAL: {
			ctx->last_var = 0;
			if (en->literal.kind == LIT_STR) {
				da_append(&prog->globals, ((TAC_GlobalVar){
					.type = (Type) {
						.kind = TYPE_ARRAY,
						.array.elem = &TI8,
						.array.length = strlen(en->literal.str) + 1
					},
					.index = data_id,
					.data = (u8*) en->literal.str,
				}));

				return (TAC_Operand) {
					.kind = OPR_VAR,
					.var.kind = VAR_ADDR,
					.var.type = (Type) {.kind = TYPE_POINTER, .pointer.base = &TI8},
					.var.addr_id = data_id++,
					.var.addr_kind = VAR_DATA,
				};
			}

			return (TAC_Operand) {
				.kind = OPR_LITERAL,
				.literal = en->literal,
			};
		};

		case AST_VID: {
			if (ctx->is_field_gen) {
				ctx->last_var = 0;
				return (TAC_Operand) {
					.kind = OPR_FIELD,
					.field_id = en->vid,
				};
			} else {
				ctx->last_var = 0;
				return *ASTVarTable_get(&avt, en->vid);
			}
		} break;

		case AST_FUNC_CALL: {
			tac_ir_gen_func_call(prog, func, en);

			TAC_Operand res = {
				.kind = OPR_FUNC_RET,
				.func_ret.type = en->func_call.type,
			};

			TAC_Instruction inst = {
				.op = OP_ASSIGN,
				.arg1 = res,
				.dst = (TAC_Operand) {
					.kind = OPR_VAR,
					.var.type = en->func_call.type,
					.var.addr_id = var_id++,
				},
			};

			ctx->last_var = inst.dst.var.addr_id;
			da_append(&func->body, inst);
			return inst.dst;
		} break;

		case AST_BIN_EXP: {
			bool is_op_field = en->expr_binary.op == AST_OP_FIELD;
			if (is_op_field) ctx->is_field_gen = false;
			TAC_Operand l = tac_ir_gen_expr(ctx, prog, func, en->expr_binary.l);
			if (is_op_field) ctx->is_field_gen = true;
			TAC_Operand r = tac_ir_gen_expr(ctx, prog, func, en->expr_binary.r);

			if (is_op_field) {
				ctx->is_field_gen = false;
				da_append(&l.var.fields, r.field_id);
				return l;
			}

			TAC_Operand ret; if (tac_ir_opr_calc(en, l, r, &ret)) {
				return ret;
			}

			//printf("%d\n", en->expr_binary.type.kind);
			Type exp_type;
			switch (en->expr_binary.op) {
				case AST_OP_FIELD:
				case AST_OP_ARR:
					exp_type = (Type) {
						.kind = TYPE_POINTER,
						.pointer.base = &en->expr_binary.type
					};
					break;
				default:
					exp_type = en->expr_binary.type;
					break;
			}

			TAC_Instruction inst = {
				.arg1 = l,
				.arg2 = r,
				.dst = (TAC_Operand){
					.kind = OPR_VAR,
					.var.type = exp_type,
					.var.kind = VAR_STACK,
					.var.addr_id = var_id++,
				},
			};

			switch (en->expr_binary.op) {
				case AST_OP_ADD:      inst.op = OP_ADD;      break;
				case AST_OP_SUB:      inst.op = OP_SUB;      break;
				case AST_OP_MUL:      inst.op = OP_MUL;      break;
				case AST_OP_DIV:      inst.op = OP_DIV;      break;
				case AST_OP_MOD:      inst.op = OP_MOD;      break;
				case AST_OP_LESS:     inst.op = OP_LESS;     break;
				case AST_OP_LESS_EQ:  inst.op = OP_LESS_EQ;  break;
				case AST_OP_GREAT:    inst.op = OP_GREAT;    break;
				case AST_OP_GREAT_EQ: inst.op = OP_GREAT_EQ; break;
				case AST_OP_EQ:       inst.op = OP_EQ;       break;
				case AST_OP_NOT_EQ:   inst.op = OP_NOT_EQ;   break;
				case AST_OP_AND:      inst.op = OP_AND;      break;
				case AST_OP_OR:       inst.op = OP_OR;       break;
				case AST_OP_FIELD:    inst.op = OP_FADDR;    break;
				case AST_OP_ARR:                             break;
				default: UNREACHABLE;
			}

			// pointers arithmetic
			if (inst.op == OP_ADD ||
				inst.op == OP_SUB ||
				en->expr_binary.op == AST_OP_ARR) {
				Type lt = tac_ir_get_opr_type(l);
				Type rt = tac_ir_get_opr_type(r);

				if (is_pointer(lt) || is_pointer(rt)) {
					Type ptr_base;
					TAC_Operand tm;
					if (is_pointer(lt)) {
						ptr_base = *lt.pointer.base;
						tm = r;
					} else if (is_pointer(rt)) {
						ptr_base = *rt.pointer.base;
						tm = l;
					}

					TAC_Operand dst = {
						.kind = OPR_VAR,
						.var.type = exp_type,
						.var.addr_id = var_id++,
					};

					TAC_Instruction mult = {
						.op = OP_MUL,
						.arg1 = tm,
						.arg2 = (TAC_Operand) {
							.kind = OPR_SIZEOF,
							.size_of.type = (Type) {.kind = TYPE_IPTR},
							.size_of.v_type = ptr_base,
						},
						.dst = dst,
					};

					da_append(&func->body, mult);
					if (is_pointer(lt)) inst.arg2 = dst;
					if (is_pointer(rt)) inst.arg1 = dst;
					if (en->expr_binary.op == AST_OP_ARR) {
						inst.op = OP_ADD;
						da_append(&func->body, inst);
						return tac_ir_gen_deref(ctx, func, *exp_type.pointer.base, inst.dst);
					}
				}
			}

			ctx->last_var = inst.dst.var.addr_id;
			da_append(&func->body, inst);
			return inst.dst;
		} break;

		case AST_UN_EXP: {
			if (en->expr_unary.op == AST_OP_SIZEOF) {
				TAC_Operand arg = tac_ir_gen_expr(ctx, prog, func, en->expr_unary.v);
				ctx->last_var = 0;
				return (TAC_Operand) {
					.kind = OPR_SIZEOF,
					.size_of.type = en->expr_unary.type,
					.size_of.v_type = tac_ir_get_opr_type(arg),
				};
			} else if (en->expr_unary.op == AST_OP_DEREF) {
				TAC_Operand arg = tac_ir_gen_expr(ctx, prog, func, en->expr_unary.v);
				return tac_ir_gen_deref(ctx, func, en->expr_unary.type, arg);
			}

			TAC_Operand arg = tac_ir_gen_expr(ctx, prog, func, en->expr_unary.v);
			TAC_Operand ret; if (tac_ir_opr_calc(en, arg, (TAC_Operand){}, &ret)) {
				return ret;
			}

			TAC_Instruction inst = {
				.arg1 = arg,
				.dst = (TAC_Operand){
					.kind = OPR_VAR,
					.var.type = en->expr_unary.type,
					.var.addr_id = var_id++,
				},
			};

			switch (en->expr_unary.op) {
				case AST_OP_CAST: inst.op = OP_CAST; break;
				case AST_OP_NOT:  inst.op = OP_NOT;  break;
				case AST_OP_NEG:  inst.op = OP_NEG;  break;
				case AST_OP_REF:  inst.op = OP_REF;  break;
				default: UNREACHABLE;
			}

			if (inst.op == OP_CAST) {
				Type lt = tac_ir_get_opr_type(inst.arg1);
				Type rt = tac_ir_get_opr_type(inst.dst);
				if (lt.kind == rt.kind || (is_pointer(lt) && is_pointer(rt)))
					inst.op = OP_ASSIGN;
			}

			ctx->last_var = inst.dst.var.addr_id;
			da_append(&func->body, inst);
			return inst.dst;
		} break;

		default: UNREACHABLE;
	}

	return (TAC_Operand){0};
}

void tac_ir_gen_var_def(TAC_Program *prog, TAC_Func *func, AST_Node *cn) {
	IRGenExprCtx ctx = {0};
	TAC_Operand res = tac_ir_gen_expr(&ctx, prog, func, cn->var_def.exp);

	if (ctx.last_var == 0) {
		da_append(&func->body, ((TAC_Instruction){
			.op = OP_ASSIGN,
			.arg1 = res,
			.dst = (TAC_Operand) {
				.kind = OPR_VAR,
				.var.type = cn->var_def.type,
				.var.addr_id = var_id,
			},
		}));

		ASTVarTable_add(&avt, cn->var_def.id, (TAC_Operand) {
			.kind = OPR_VAR,
			.var.kind = VAR_STACK,
			.var.type = cn->var_def.type,
			.var.addr_id = var_id++
		});
	} else {
		ASTVarTable_add(&avt, cn->var_def.id, (TAC_Operand) {
			.kind = OPR_VAR,
			.var.kind = VAR_STACK,
			.var.type = cn->var_def.type,
			.var.addr_id = ctx.last_var,
		});
	}
}

void tac_ir_gen_var_mut(TAC_Program *prog, TAC_Func *func, AST_Node *cn) {
	IRGenExprCtx ctx = {0};
	ctx.is_right_of_eq = false;
	TAC_Operand dst = tac_ir_gen_expr(&ctx, prog, func, cn->var_mut.exp->expr_binary.l);
	ctx.is_right_of_eq = true;
	TAC_Operand res = tac_ir_gen_expr(&ctx, prog, func, cn->var_mut.exp->expr_binary.r);

	TAC_OpCode op_eq;
	bool is_op_eq;
	switch (cn->var_mut.exp->expr_binary.op) {
		case AST_OP_ADD_EQ: op_eq = OP_ADD; is_op_eq = true; break;
		case AST_OP_SUB_EQ: op_eq = OP_SUB; is_op_eq = true; break;
		case AST_OP_MUL_EQ: op_eq = OP_MUL; is_op_eq = true; break;
		case AST_OP_DIV_EQ: op_eq = OP_DIV; is_op_eq = true; break;
		default: is_op_eq = false;
	}

	if (is_op_eq) {
		TAC_Instruction op_eq_res = {
			.op = op_eq,
			.arg1 = dst,
			.arg2 = res,
			.dst = (TAC_Operand) {
				.kind = OPR_VAR,
				.var.type = cn->var_mut.type,
				.var.addr_id = var_id++,
			},
		};

		da_append(&func->body, op_eq_res);
		da_append(&func->body, ((TAC_Instruction){
			.op = OP_ASSIGN,
			.arg1 = op_eq_res.dst,
			.dst = dst,
		}));
	} else {
		da_append(&func->body, ((TAC_Instruction){
			.op = OP_ASSIGN,
			.arg1 = res,
			.dst = dst,
		}));
	}
}

void tac_ir_gen_func_call(TAC_Program *prog, TAC_Func *func, AST_Node *cn) {
	TAC_Instruction func_call = {
		.op = OP_FUNC_CALL,
		.dst = (TAC_Operand) {
			.kind = OPR_NAME,
			.name = cn->func_call.id,
		},
	};

	assert(cn->func_call.args.count < 13);
	for (size_t i = 0; i < cn->func_call.args.count; i++) {
		AST_Node *arg = da_get(&cn->func_call.args, i);
		IRGenExprCtx ctx = {0};
		func_call.args[i] = tac_ir_gen_expr(&ctx, prog, func, arg);
	}

	func_call.args[cn->func_call.args.count] = (TAC_Operand) {.kind = OPR_NULL};
	da_append(&func->body,  func_call);
}

typedef struct {
	uint label_start;
	uint label_end;
	AST_Node *for_var_mut;
	int loop_gen;
} IRGenBodyCtx;

void tac_ir_gen_body(IRGenBodyCtx *ctx, TAC_Program *prog, TAC_Func *func, AST_Node *fn);

void tac_ir_gen_if_chain(IRGenBodyCtx *ctx, TAC_Program *prog, TAC_Func *func, AST_Node *cn) {
	IRGenExprCtx ectx = {0};
	TAC_Operand res = tac_ir_gen_expr(&ectx, prog, func, cn->stmt_if.exp);
	uint label_start = label_id++;
	uint label_end = label_id++;

	da_append(&func->body, ((TAC_Instruction){
		.op = OP_JUMP_IF_NOT,
		.arg1 = res,
		.dst = (TAC_Operand) {
			.kind = OPR_LABEL,
			.label_id = label_start,
		},
	}));

	tac_ir_gen_body(ctx, prog, func, cn->stmt_if.body);

	if (cn->stmt_if.next) {
		da_append(&func->body, ((TAC_Instruction){
			.op = OP_JUMP,
			.dst = (TAC_Operand) {
				.kind = OPR_LABEL,
				.label_id = label_end,
			},
		}));
	}

	da_append(&func->body, ((TAC_Instruction){
		.op = OP_LABEL,
		.arg1 = (TAC_Operand) {
			.kind = OPR_LABEL,
			.label_id = label_start,
		},
	}));

	if (cn->stmt_if.next) {
		if (cn->stmt_if.next->kind == AST_IF_STMT) {
			tac_ir_gen_if_chain(ctx, prog, func, cn->stmt_if.next);
		} else if (cn->stmt_if.next->kind == AST_ELSE_STMT) {
			tac_ir_gen_body(ctx, prog, func, cn->stmt_if.next->stmt_else.body);
		} else UNREACHABLE;

		da_append(&func->body, ((TAC_Instruction){
			.op = OP_LABEL,
			.arg1 = (TAC_Operand) {
				.kind = OPR_LABEL,
				.label_id = label_end,
			},
		}));
	}
}

void tac_ir_gen_body(IRGenBodyCtx *ctx, TAC_Program *prog, TAC_Func *func, AST_Node *fn) {
	for (size_t i = 0; i < fn->body.stmts.count; i++) {
		AST_Node *cn = da_get(&fn->body.stmts, i);
		switch (cn->kind) {
			case AST_VAR_DEF:   tac_ir_gen_var_def(prog, func, cn);       break;
			case AST_FUNC_CALL: tac_ir_gen_func_call(prog, func, cn);     break;
			case AST_VAR_MUT:   tac_ir_gen_var_mut(prog, func, cn);       break;
			case AST_IF_STMT:   tac_ir_gen_if_chain(ctx, prog, func, cn); break;
			case AST_BODY:      tac_ir_gen_body(ctx, prog, func, cn);     break;

			case AST_FUNC_RET: {
				if (cn->func_ret.type.kind == TYPE_NULL) {
					da_append(&func->body, ((TAC_Instruction){
						.op = OP_RETURN,
						.arg1 = (TAC_Operand) {.kind = OPR_NULL},
					}));
					break;
				}

				IRGenExprCtx ctx = {0};
				TAC_Operand res = tac_ir_gen_expr(&ctx, prog, func, cn->func_ret.expr);
				switch (res.kind) {
					case OPR_LITERAL: {
						da_append(&func->body, ((TAC_Instruction){
							.op = OP_RETURN,
							.arg1 = res,
						}));
					} break;

					case OPR_VAR: {
						da_append(&func->body, ((TAC_Instruction){
							.op = OP_RETURN,
							.arg1 = (TAC_Operand) {
								.kind = OPR_VAR,
								.var.type = cn->func_ret.type,
								.var.addr_id = res.var.addr_id,
							},
						}));
					} break;

					default: UNREACHABLE;
				}
			} break;

			case AST_LOOP_BREAK:
				if (ctx->loop_gen <= 0) lexer_error(cn->loc, "error: break outside of a loop");
				da_append(&func->body, ((TAC_Instruction){
					.op = OP_JUMP,
					.dst = (TAC_Operand) {
						.kind = OPR_LABEL,
						.label_id = ctx->label_end,
					},
				}));
				break;

			case AST_LOOP_CONTINUE:
				if (ctx->loop_gen <= 0) lexer_error(cn->loc, "error: continue outside of a loop");
				if (ctx->for_var_mut) tac_ir_gen_var_mut(prog, func, ctx->for_var_mut);
				da_append(&func->body, ((TAC_Instruction){
					.op = OP_JUMP,
					.dst = (TAC_Operand) {
						.kind = OPR_LABEL,
						.label_id = ctx->label_start,
					},
				}));
				break;

			case AST_WHILE_STMT: {
				ctx->loop_gen++;
				ctx->label_start = label_id++;

				da_append(&func->body, ((TAC_Instruction){
					.op = OP_LABEL,
					.arg1 = (TAC_Operand) {
						.kind = OPR_LABEL,
						.label_id = ctx->label_start,
					},
				}));

				IRGenExprCtx ectx = {0};
				TAC_Operand res = tac_ir_gen_expr(&ectx, prog, func, cn->stmt_while.exp);
				ctx->label_end = label_id++;

				da_append(&func->body, ((TAC_Instruction){
					.op = OP_JUMP_IF_NOT,
					.arg1 = res,
					.dst = (TAC_Operand) {
						.kind = OPR_LABEL,
						.label_id = ctx->label_end,
					},
				}));

				tac_ir_gen_body(ctx, prog, func, cn->stmt_while.body);

				da_append(&func->body, ((TAC_Instruction){
					.op = OP_JUMP,
					.dst = (TAC_Operand) {
						.kind = OPR_LABEL,
						.label_id = ctx->label_start,
					},
				}));

				da_append(&func->body, ((TAC_Instruction){
					.op = OP_LABEL,
					.arg1 = (TAC_Operand) {
						.kind = OPR_LABEL,
						.label_id = ctx->label_end,
					},
				}));

				ctx->loop_gen--;
			} break;

			case AST_FOR_STMT: {
				ctx->loop_gen++;
				switch (cn->stmt_for.var->kind) {
					case AST_VAR_MUT: tac_ir_gen_var_mut(prog, func, cn->stmt_for.var); break;
					case AST_VAR_DEF: tac_ir_gen_var_def(prog, func, cn->stmt_for.var); break;
					default: UNREACHABLE;
				}

				uint label_start = label_id++;

				da_append(&func->body, ((TAC_Instruction){
					.op = OP_LABEL,
					.arg1 = (TAC_Operand) {
						.kind = OPR_LABEL,
						.label_id = label_start,
					},
				}));

				IRGenExprCtx ectx = {0};
				TAC_Operand res = tac_ir_gen_expr(&ectx, prog, func, cn->stmt_for.expr);

				uint label_end = label_id++;

				da_append(&func->body, ((TAC_Instruction){
					.op = OP_JUMP_IF_NOT,
					.arg1 = res,
					.dst = (TAC_Operand) {
						.kind = OPR_LABEL,
						.label_id = label_end,
					},
				}));

				ctx->for_var_mut = cn->stmt_for.mut;
				tac_ir_gen_body(ctx, prog, func, cn->stmt_for.body);
				tac_ir_gen_var_mut(prog, func, cn->stmt_for.mut);

				da_append(&func->body, ((TAC_Instruction){
					.op = OP_JUMP,
					.dst = (TAC_Operand) {
						.kind = OPR_LABEL,
						.label_id = label_start,
					},
				}));

				da_append(&func->body, ((TAC_Instruction){
					.op = OP_LABEL,
					.arg1 = (TAC_Operand) {
						.kind = OPR_LABEL,
						.label_id = label_end,
					},
				}));
				ctx->loop_gen--;
				ctx->for_var_mut = NULL;
			} break;

			default: UNREACHABLE;
		}
	}
}

void tac_ir_gen_func(TAC_Program *prog, AST_Node *fn) {
	TAC_Func func = {.name = fn->func_def.id, .ret_type = fn->func_def.type};

	for (size_t i = 0; i < fn->func_def.args.count; i++) {
		AST_Node *cn = da_get(&fn->func_def.args, i);
		da_append(&func.body, ((TAC_Instruction) {
			.op = OP_ASSIGN,
			.arg1 = (TAC_Operand) {
				.kind = OPR_FUNC_INP,
				.func_inp.type = cn->func_def_arg.type,
				.func_inp.arg_id = (uint) i,
			},
			.dst = (TAC_Operand) {
				.kind = OPR_VAR,
				.var.type = cn->func_def_arg.type,
				.var.addr_id = var_id,
			},
		}));
		ASTVarTable_add(&avt, cn->func_def_arg.id, (TAC_Operand) {
			.kind = OPR_VAR,
			.var.kind = VAR_STACK,
			.var.type = cn->func_def_arg.type,
			.var.addr_id = var_id++
		});
	}

	IRGenBodyCtx bctx = {0};
	tac_ir_gen_body(&bctx, prog, &func, fn->func_def.body);
	da_append(&prog->funcs, func);
}

TAC_Program tac_ir_gen_prog(Parser *p) {
	TAC_Program prog = {0};
	label_id = 0;

	ht_foreach_node(SymbolTable, &p->st, n) {
		if (n->key.type == SBL_FUNC_EXTERN) {
			da_append(&prog.externs, ((TAC_Extern){
				.name = n->val.func_extern.extern_smb,
				.ret_type = n->val.func_extern.type,
			}));
		} else if (n->key.type == SBL_VAR && n->key.nested[0] == 0) {
			da_append(&prog.globals, ((TAC_GlobalVar){
				.type = n->val.variable.type,
				.index = data_id,
			}));
			ASTVarTable_add(&avt, n->key.id, (TAC_Operand){
				.kind = OPR_VAR,
				.var.kind = VAR_DATA,
				.var.type = n->val.variable.type,
				.var.addr_id = data_id++,
			});
		}
	}

	AST_Node *pn = p->program;
	for (size_t i = 0; i < pn->program.stmts.count; i++) {
		AST_Node *cn = da_get(&pn->program.stmts, i);
		switch (cn->kind) {
			case AST_FUNC_DEF:
				tac_ir_gen_func(&prog, cn);
				break;
			default: UNREACHABLE;
		}
	}

	return prog;
}
