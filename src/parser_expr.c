#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#include "../include/parser.h"

int64_t parse_int(char *data) {
	return atol(data);
}

double parse_float(char *data) {
	return atof(data);
}

float op_cost(char op, bool is_left) {
	switch (op) {
		case '+': case '-':
			if (is_left)
				return 1.1;
			else
				return 1.0;
		case '*': case '/':
			if (is_left)
				return 2.1;
			else
				return 2.0;
	}

	return 0.0;
}

// TODO: add func call and variables supporting
typedef struct ExprNode {
	Token *tok;
	struct ExprNode *l, *r;
	struct ExprNode *next, *prev;
} ExprNode;

void expr_node_add(ExprNode **en, Token *n) {
	if ((*en)->tok == NULL) {
		(*en)->tok = n;
		return;
	}

	(*en)->next = malloc(sizeof(ExprNode));
	(*en)->next->tok = n;
	(*en)->next->l = NULL;
	(*en)->next->r = NULL;
	(*en)->next->next = NULL;
	(*en)->next->prev = (*en);
	(*en) = (*en)->next;
}


void expr_node_free(ExprNode *en) {
	free(en);
}

void expr_nodes_free(ExprNode *head) {
	if (head->next != NULL) expr_nodes_free(head->next);
	expr_node_free(head);
}

bool is_tok_op(TokenType type) {
	switch (type) {
		case TOK_PLUS: case TOK_MINUS:
		case TOK_STAR: case TOK_SLASH:
			return true;
		default:
			return false;
	}
}

bool is_expr_node_var(ExprNode *en) {
	return
		en->tok->type == TOK_FLOAT ||
		en->tok->type == TOK_INT ||
		en->tok->type == TOK_ID ||
		(is_tok_op(en->tok->type) &&
		 en->l != NULL && en->r != NULL);
}

AST_Node *expr_tree_to_ast(ExprNode *expr) {
	switch (expr->tok->type) {
		case TOK_PLUS: case TOK_MINUS: case TOK_STAR: case TOK_SLASH:
			return ast_alloc((AST_Node){
					.type = AST_BIN_EXP,
					.payload.exp_binary.op = expr->tok->data[0],
					.payload.exp_binary.l = expr_tree_to_ast(expr->l),
					.payload.exp_binary.r = expr_tree_to_ast(expr->r),
					});

		case TOK_FLOAT:
			return ast_alloc((AST_Node){
					.type = AST_FLOAT,
					.payload.num_float = parse_float(expr->tok->data),
					});

		case TOK_INT:
			return ast_alloc((AST_Node){
					.type = AST_INT,
					.payload.num_int = parse_int(expr->tok->data),
					});

		default:
			printf("parsing error: expr tree to ast parsing error\n");
			return NULL;
	}

	return NULL;
}

ExprNode *parse_expression_r(ExprNode **head) {
	if ((*head)->next == NULL)
		return (*head);

	ExprNode *cur_node = (*head);
	ExprNode *br_prev = NULL;
	ExprNode *br_expr = NULL;
	ExprNode *br_expr_s = NULL;
	bool br_fl = false;
	int br_cnt = 0;

	while (cur_node != NULL) {
		if (cur_node->tok->type == TOK_RBRA) {
			br_cnt--;

			if (br_cnt == 0) {
				br_fl = false;

				ExprNode *expr = parse_expression_r(&br_expr_s);
				if (br_prev != NULL) {
					br_prev->next = expr;
					expr->prev = br_prev;
				} else {
					*head = expr;
					expr->prev = NULL;
				}

				expr->next = cur_node->next;
				if (cur_node->next != NULL) {
					cur_node->next->prev = expr;
				}

				cur_node = expr;
				continue;
			}
		}

		if (br_fl) expr_node_add(&br_expr, cur_node->tok);	

		if (cur_node->tok->type == TOK_LBRA) {
			if (br_cnt == 0) {
				br_fl = true;
				br_prev = cur_node->prev;

				br_expr = malloc(sizeof(ExprNode));
				*br_expr = (ExprNode){0};
				br_expr_s = br_expr;
			}

			br_cnt++;
		}

		if (is_expr_node_var(cur_node) && !br_fl) {
			if (cur_node->prev == NULL && cur_node->next == NULL) break;
			float l_cost = 0, r_cost = 0;

			if (cur_node->prev != NULL)
				l_cost = op_cost(cur_node->prev->tok->data[0], false);

			if (cur_node->next != NULL)
				r_cost = op_cost(cur_node->next->tok->data[0], true);

			if (l_cost > r_cost)
				cur_node->prev->r = cur_node;
			else
				cur_node->next->l = cur_node;

			if (cur_node->prev != NULL)
				cur_node->prev->next = cur_node->next;

			if (cur_node->next != NULL)
				cur_node->next->prev = cur_node->prev;

			if (cur_node == (*head))
				(*head) = cur_node->next;
		}

		cur_node = cur_node->next;
	}

	return parse_expression_r(head);
}

AST_Node *parse_expression(Parser *parser, ExprParsingType type) {
	ExprNode *cur_node = malloc(sizeof(ExprNode));
	ExprNode *head = cur_node;
	*cur_node = (ExprNode){0};
	int br_cnt = 0;

	while (true) {
		if (parser->cur_token->type == TOK_LBRA) br_cnt++;
		if (parser->cur_token->type == TOK_RBRA) br_cnt--;

		if (type == EXPR_PARSING_FUNC_CALL) {
			if (br_cnt == 0 && parser->cur_token->type == TOK_COM) break;
			if (br_cnt == -1) break;
		} else if (type == EXPR_PARSING_VAR) {
			if (parser->cur_token->type == TOK_SEMI) break;
		}

		switch (parser->cur_token->type) {
			case TOK_RBRA: case TOK_LBRA:
			case TOK_INT: case TOK_FLOAT:
			case TOK_PLUS: case TOK_MINUS:
			case TOK_STAR: case TOK_SLASH:
				expr_node_add(&cur_node, parser->cur_token);
				break;

			default:
				printf("expression parsing error: wrong token");
				break;
		}

		parser->cur_token++;
	}

	parser->cur_token++;
	ExprNode *expr_tree = parse_expression_r(&head);
	return expr_tree_to_ast(expr_tree);
}
