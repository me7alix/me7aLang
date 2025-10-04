#include <stdint.h>
#define SB_IMPLEMENTATION
#include "../thirdparty/sb.h"
#include "../include/parser.h"

#define TAB "    "

#define cg_add(...) \
	sb_append_strf(&cg->code, __VA_ARGS__)

typedef struct {
	StringBuilder code;
	SymbolTable st;
} NASM_Codegen;

void nasm_codegen_expr_get_val(NASM_Codegen *cg, char *buf, AST_Node *exp) {
	switch (exp->type) {
		case AST_INT:
			sprintf(buf, "%zu", exp->num_int);
			break;

		case AST_VAR: {
			Symbol *s = st_get(&cg->st, exp->var_id);
			if (s->variable.type->kind == TYPE_INT) {
				cg_add(TAB"mov edx, dword [rbp - %zu]\n", s->variable.offset);
				sprintf(buf, "edx");
			}
		} break;

		default:
			sprintf(buf, "_");
			break;
	}
}

void nasm_codegen_expr_int(NASM_Codegen *cg, Type *type, AST_Node *exp, char *address) {
	char buf[256];
	char *reg = NULL;

	if (type->kind == TYPE_INT)
		reg = "eax";

	bool isLv = exp->exp_binary.l->type != AST_BIN_EXP;
	bool isRv = exp->exp_binary.r->type != AST_BIN_EXP;

	switch (exp->exp_binary.op) {
		case '+': {
			if (isLv && isRv) {
				nasm_codegen_expr_get_val(cg, buf, exp->exp_binary.l);
				cg_add(TAB"mov %s, %s\n", address, buf);
				nasm_codegen_expr_get_val(cg, buf, exp->exp_binary.r);
				cg_add(TAB"add %s, %s\n", address, buf);
			} else if (isLv && !isRv) {
				nasm_codegen_expr_get_val(cg, buf, exp->exp_binary.l);
				cg_add(TAB"add %s, %s\n", address, buf);
			} else if (isRv && !isLv) {
				nasm_codegen_expr_get_val(cg, buf, exp->exp_binary.r);
				cg_add(TAB"add %s, %s\n", address, buf);
			}
		} break;

		case '*': {
			if (isLv && isRv) {
				nasm_codegen_expr_get_val(cg, buf, exp->exp_binary.l);
				cg_add(TAB"mov %s, %s\n", reg, buf);
				nasm_codegen_expr_get_val(cg, buf, exp->exp_binary.r);
				cg_add(TAB"imul %s, %s\n", reg, buf);
				if (strcmp(address, reg) != 0)
					cg_add(TAB"mov %s, %s\n", address, reg);
			} else if (isLv && !isRv) {
				if (strcmp(address, reg) != 0)
					cg_add(TAB"mov %s, %s\n", reg, address);
				nasm_codegen_expr_get_val(cg, buf, exp->exp_binary.l);
				cg_add(TAB"imul %s, %s\n", reg, buf);
				if (strcmp(address, reg) != 0)
					cg_add(TAB"mov %s, %s\n", address, reg);
			} else if (isRv && !isLv) {
				if (strcmp(address, reg) != 0)
					cg_add(TAB"mov %s, %s\n", reg, address);
				nasm_codegen_expr_get_val(cg, buf, exp->exp_binary.r);
				cg_add(TAB"imul %s, %s\n", reg, buf);
				if (strcmp(address, reg) != 0)
					cg_add(TAB"mov %s, %s\n", address, reg);
			}
		} break;

		case '-': {
			if (isLv && isRv) {
				nasm_codegen_expr_get_val(cg, buf, exp->exp_binary.l);
				cg_add(TAB"mov %s, %s\n", address, buf);
				nasm_codegen_expr_get_val(cg, buf, exp->exp_binary.r);
				cg_add(TAB"sub %s, %s\n", address, buf);
			} else if (isLv && !isRv) {
				nasm_codegen_expr_get_val(cg, buf, exp->exp_binary.l);
				cg_add(TAB"sub %s, %s\n", address, buf);
			} else if (isRv && !isLv) {
				nasm_codegen_expr_get_val(cg, buf, exp->exp_binary.r);
				cg_add(TAB"sub %s, %s\n", address, buf);
			}
		} break;

		case '/': {
			if (isLv && isRv) {
				nasm_codegen_expr_get_val(cg, buf, exp->exp_binary.l);
				cg_add(TAB"mov %s, %s\n", reg, buf);
				nasm_codegen_expr_get_val(cg, buf, exp->exp_binary.r);
				cg_add(TAB"imul %s, %s\n", reg, buf);
				if (strcmp(address, reg) != 0)
					cg_add(TAB"mov %s, %s\n", address, reg);
			} else if (isLv && !isRv) {
				if (strcmp(address, reg) != 0)
					cg_add(TAB"mov %s, %s\n", reg, address);
				nasm_codegen_expr_get_val(cg, buf, exp->exp_binary.l);
				cg_add(TAB"imul %s, %s\n", reg, buf);
				if (strcmp(address, reg) != 0)
					cg_add(TAB"mov %s, %s\n", address, reg);
			} else if (isRv && !isLv) {
				if (strcmp(address, reg) != 0)
					cg_add(TAB"mov %s, %s\n", reg, address);
				nasm_codegen_expr_get_val(cg, buf, exp->exp_binary.r);
				cg_add(TAB"imul %s, %s\n", reg, buf);
				if (strcmp(address, reg) != 0)
					cg_add(TAB"mov %s, %s\n", address, reg);
			}
		} break;
	}
}

void nasm_codegen_expr(NASM_Codegen *cg, AST_Node *p, AST_Node *exp) {
	char address[256];
	char buf[256];

	if (p->type == AST_VAR_DEF) {
		if (p->var_def.exp->type == AST_INT) {
			cg_add(TAB"mov dword [rbp - %zu], %zu\n",
				  p->var_def.offset, exp->num_int);
			return;
		} else if (p->var_def.exp->type == AST_VAR) {
			Symbol *s = st_get(&cg->st, exp->var_id);
			cg_add(TAB"mov eax, dword [rbp - %zu]\n",
				  p->var_def.offset, s->variable.offset);
			return;
		}

		if (p->var_def.type->kind == TYPE_INT)
			sprintf(address, "dword [rbp - %zu]", p->var_def.offset);
	} else if (p->type == AST_FUNC_RET){
		if (p->func_ret.exp->type == AST_INT) {
			cg_add(TAB"mov eax, %zu\n", exp->num_int);
			return;
		} else if (p->func_ret.exp->type == AST_VAR) {
			nasm_codegen_expr_get_val(cg, buf, exp);
			cg_add(TAB"mov eax, %s\n", buf);
			return;
		}

		if (p->func_ret.type->kind == TYPE_INT)
			sprintf(address, "eax");
	} else if (p->type == AST_VAR_MUT) {
		if (p->var_mut.exp->type == AST_INT) {
			cg_add(TAB"mov dword [rbp - %zu], %zu\n",
				  p->var_mut.offset, exp->num_int);
			return;
		} else if (p->var_mut.exp->type == AST_VAR) {
			nasm_codegen_expr_get_val(cg, buf, exp);
			cg_add(TAB"mov dword [rbp - %zu], %s\n",
				  p->var_mut.offset, buf);
			return;
		}

		if (p->var_mut.type->kind == TYPE_INT)
			sprintf(address, "dword [rbp - %zu]", p->var_mut.offset);
	}

	switch (exp->type) {
		case AST_BIN_EXP: {
			nasm_codegen_expr(cg, p, exp->exp_binary.l);
			nasm_codegen_expr(cg, p, exp->exp_binary.r);

			if (exp->exp_binary.type->kind == TYPE_INT) {
				nasm_codegen_expr_int(cg, exp->exp_binary.type, exp, address);
			}
		} break;

		default:
			break;
	}
}

void nasm_codegen_body(NASM_Codegen *cg, AST_Node *p) {
	cg_add(TAB"sub rsp, %zu\n", p->body.total_off+8);
	for (size_t i = 0; i < p->body.stmts.count; i++) {
		AST_Node *cur_node = da_get(&p->body.stmts, i);
		switch (cur_node->type) {
			case AST_VAR_DEF:
				if (cur_node->var_def.exp != NULL)
					nasm_codegen_expr(cg, cur_node, cur_node->var_def.exp);
				break;

			case AST_VAR_MUT:
				printf("here\n");
				nasm_codegen_expr(cg, cur_node, cur_node->var_mut.exp);
				break;

			case AST_FUNC_RET:
				nasm_codegen_expr(cg, cur_node, cur_node->func_ret.exp);
				break;

			default:
				break;
		}
	}
}

void nasm_codegen_func_def(NASM_Codegen *cg, AST_Node *p) {
	cg_add("%s:\n", p->func_def.id);

	cg_add(TAB"push rbp\n");
	cg_add(TAB"mov rbp, rsp\n");

	nasm_codegen_body(cg, p->func_def.block);

	cg_add(TAB"mov rsp, rbp\n");
	cg_add(TAB"pop rbp\n");
	cg_add(TAB"ret\n\n");
}

void nasm_codegen(NASM_Codegen *cg, AST_Node *p) {
	cg_add("global main\n\n");
	cg_add("section .text\n\n");

	printf("stmts count %zu\n", p->program.stmts.count);
	for (size_t i = 0; i < p->program.stmts.count; i++) {
		AST_Node *cur_node = da_get(&p->program.stmts, i);
		switch (cur_node->type) {
			case AST_FUNC_DEF:
				nasm_codegen_func_def(cg, cur_node);
				break;

			default:
				break;
		}
	}
}
