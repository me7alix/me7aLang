#include <assert.h>
#include <threads.h>

#define SB_IMPLEMENTATION
#include "../thirdparty/sb.h"
#include "../include/parser.h"

#define TAB "    "

#define cg_add(...) \
sb_append_strf(&cg->code, __VA_ARGS__)

size_t id = 0;

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
			TypeKind t = s->variable.type->kind;

			if (t == TYPE_INT || t == TYPE_I32) {
				cg_add(TAB"mov r10d, dword [rbp - %zu]\n", s->variable.offset);
				sprintf(buf, "r10d");
			}
		} break;

		default:
			sprintf(buf, "_");
			break;
	}
}

void nasm_codegen_expr_int(NASM_Codegen *cg, AST_Node *p, Type *type, AST_Node *exp, char *address) {
	char src_buf[256];

	bool isLv = exp->exp_binary.l->type != AST_BIN_EXP;
	bool isRv = exp->exp_binary.r->type != AST_BIN_EXP;

	printf("%s\n", tok_to_str(exp->exp_binary.op));
	switch (exp->exp_binary.op) {
		case TOK_PLUS: {
			if (isLv && isRv) {
				nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.l);
				cg_add(TAB"mov %s, %s\n", address, src_buf);
				nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.r);
				cg_add(TAB"add %s, %s\n", address, src_buf);
			} else if (isLv && !isRv) {
				nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.l);
				cg_add(TAB"add %s, %s\n", address, src_buf);
			} else if (isRv && !isLv) {
				nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.r);
				cg_add(TAB"add %s, %s\n", address, src_buf);
			}
		} break;

		case TOK_STAR: {
			char *reg = NULL;

			if (type->kind == TYPE_INT || type->kind == TYPE_I32) reg = "r9d";
			else if (type->kind == TYPE_I64) reg = "r9";

			if (isLv && isRv) {
				nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.l);
				cg_add(TAB"mov %s, %s\n", reg, src_buf);
				nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.r);
				cg_add(TAB"imul %s, %s\n", reg, src_buf);
				cg_add(TAB"mov %s, %s\n", address, reg);
			} else if (isLv && !isRv) {
				cg_add(TAB"mov %s, %s\n", reg, address);
				nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.l);
				cg_add(TAB"imul %s, %s\n", reg, src_buf);
				cg_add(TAB"mov %s, %s\n", address, reg);
			} else if (isRv && !isLv) {
				cg_add(TAB"mov %s, %s\n", reg, address);
				nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.r);
				cg_add(TAB"imul %s, %s\n", reg, src_buf);
				cg_add(TAB"mov %s, %s\n", address, reg);
			}
		} break;

		case TOK_MINUS: {
			if (isLv && isRv) {
				nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.l);
				cg_add(TAB"mov %s, %s\n", address, src_buf);
				nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.r);
				cg_add(TAB"sub %s, %s\n", address, src_buf);
			} else if (isLv && !isRv) {
				nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.l);
				cg_add(TAB"sub %s, %s\n", address, src_buf);
			} else if (isRv && !isLv) {
				nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.r);
				cg_add(TAB"sub %s, %s\n", address, src_buf);
			}
		} break;

		case TOK_SLASH: {
			char *dividend_help_reg = NULL;
			char *dividend_reg = NULL;
			char *divisor_reg = NULL;

			if (type->kind == TYPE_INT || type->kind == TYPE_I32) {
				dividend_help_reg = "edx";
				dividend_reg = "eax";
				divisor_reg = "ecx";
			}

			bool fl = false;
			cg_add(TAB"mov %s, 0\n", dividend_help_reg);

			if (isLv && isRv) {
				nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.l);
				cg_add(TAB"mov %s, %s\n", dividend_reg, src_buf);

				nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.r);
				cg_add(TAB"mov %s, %s\n", divisor_reg, src_buf);
			} else if (isLv && !isRv) {
				if (strcmp(address, dividend_reg) == 0) {
					cg_add(TAB"mov r9d, %s\n", address);
					fl = true;
				}

				nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.l);
				cg_add(TAB"mov %s, %s\n", dividend_reg, src_buf);

				if (strcmp(address, divisor_reg) != 0)
					cg_add(TAB"mov %s, %s\n", divisor_reg, address);
			} else if (isRv && !isLv) {
				if (strcmp(address, dividend_reg) != 0)
					cg_add(TAB"mov %s, %s\n", dividend_reg, address);

				nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.r);
				cg_add(TAB"mov %s, %s\n", divisor_reg, src_buf);
			}

			cg_add(TAB"idiv %s\n", divisor_reg);
			if (strcmp(address, dividend_reg) != 0)
				cg_add(TAB"mov %s, %s\n", address, dividend_reg);

			if (fl) cg_add(TAB"mov %s, r9d\n", address);
		} break;

		default: break;
	}
}

void nasm_codegen_expr_bool(NASM_Codegen *cg, AST_Node *p, Type *type, AST_Node *exp, char *address) {
	char src_buf[256];

	bool isLv = exp->exp_binary.l->type != AST_BIN_EXP;
	bool isRv = exp->exp_binary.r->type != AST_BIN_EXP;
	switch (exp->exp_binary.op) {
		case TOK_EQ_EQ: {
			if (p->type == AST_IF_STMT) {
				if (isLv && isRv) {
					nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.l);
					cg_add(TAB"mov eax, %s\n", src_buf);
					nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.r);
					cg_add(TAB"cmp eax, %s\n", src_buf);
				} else if (isLv && !isRv) {
					cg_add(TAB"mov eax, %s\n", address);
					nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.l);
					cg_add(TAB"cmp eac, %s\n", src_buf);
				} else if (!isLv && isRv) {
					cg_add(TAB"mov eax, %s\n", address);
					nasm_codegen_expr_get_val(cg, src_buf, exp->exp_binary.r);
					cg_add(TAB"cmp eax, %s\n", src_buf);
				}

				cg_add(TAB"jne .L%zu\n", id);
			}
		} break;

		default: break;
	}
}


void nasm_codegen_expr(NASM_Codegen *cg, AST_Node *p, AST_Node *exp) {
	char address[256];
	char buf[256];

	if (p->type == AST_VAR_DEF) {
		if (p->var_def.exp->type == AST_INT) {
			cg_add(TAB"mov dword [rbp - %zu], %zu\n", p->var_def.offset, exp->num_int);
			return;
		} else if (p->var_def.exp->type == AST_VAR) {
			Symbol *s = st_get(&cg->st, exp->var_id);
			cg_add(TAB"mov eax, dword [rbp - %zu]\n", p->var_def.offset, s->variable.offset);
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
			cg_add(TAB"mov dword [rbp - %zu], %zu\n", p->var_mut.offset, exp->num_int);
			return;
		} else if (p->var_mut.exp->type == AST_VAR) {
			nasm_codegen_expr_get_val(cg, buf, exp);
			cg_add(TAB"mov dword [rbp - %zu], %s\n", p->var_mut.offset, buf);
			return;
		}

		if (p->var_mut.type->kind == TYPE_INT)
			sprintf(address, "dword [rbp - %zu]", p->var_mut.offset);
	} else if (p->type == AST_IF_STMT){
		if (p->stmt_if.exp->type == AST_INT) {
			cg_add(TAB"mov eax, %zu\n", exp->num_int);
			return;
		} else if (p->stmt_if.exp->type == AST_VAR) {
			nasm_codegen_expr_get_val(cg, buf, exp);
			cg_add(TAB"mov eax, %s\n", buf);
			return;
		}

		sprintf(address, "eax");
	}

	switch (exp->type) {
		case AST_BIN_EXP: {
			nasm_codegen_expr(cg, p, exp->exp_binary.l);
			nasm_codegen_expr(cg, p, exp->exp_binary.r);

			if (exp->exp_binary.type->kind == TYPE_INT) {
				nasm_codegen_expr_int(cg, p, exp->exp_binary.type, exp, address);
			} else if (exp->exp_binary.type->kind == TYPE_BOOL) {
				nasm_codegen_expr_bool(cg, p, exp->exp_binary.type, exp, address);
			}
		} break;

		default: break;
	}
}

void nasm_codegen_body(NASM_Codegen *cg, AST_Node *p);

void nasm_codegen_if_stmt(NASM_Codegen *cg, AST_Node *p) {
	nasm_codegen_expr(cg, p, p->stmt_if.exp);
	nasm_codegen_body(cg, p->stmt_if.body);
	cg_add(".L%zu:\n", id++);
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
				nasm_codegen_expr(cg, cur_node, cur_node->var_mut.exp);
				break;

			case AST_FUNC_RET:
				nasm_codegen_expr(cg, cur_node, cur_node->func_ret.exp);
				cg_add(TAB"mov rsp, rbp\n");
				cg_add(TAB"pop rbp\n");
				cg_add(TAB"ret\n");
				break;

			case AST_IF_STMT:
				nasm_codegen_if_stmt(cg, cur_node);
				break;

			default: break;
		}
	}

	cg_add(TAB"add rsp, %zu\n", p->body.total_off+8);
}

void nasm_codegen_func_def(NASM_Codegen *cg, AST_Node *p) {
	cg_add("%s:\n", p->func_def.id);

	cg_add(TAB"push rbp\n");
	cg_add(TAB"mov rbp, rsp\n");

	nasm_codegen_body(cg, p->func_def.body);

	cg_add(TAB"mov rsp, rbp\n");
	cg_add(TAB"pop rbp\n");
	cg_add(TAB"ret\n\n");
}

void nasm_codegen(NASM_Codegen *cg, Parser *parser) {
	cg->st = parser->st;

	cg_add("global main\n\n");
	cg_add("section .text\n\n");

	for (size_t i = 0; i < parser->program->program.stmts.count; i++) {
		AST_Node *cur_node = da_get(&parser->program->program.stmts, i);
		switch (cur_node->type) {
			case AST_FUNC_DEF:
				nasm_codegen_func_def(cg, cur_node);
				break;

			default:
				break;
		}
	}
}
