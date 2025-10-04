#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include "../include/parser.h"

#define ARR_LEN(arr) (sizeof(arr)/sizeof(arr[0]))

double calculate_expr(AST_Node *exp) {
	if (exp->type == AST_INT) return exp->payload.num_int;
	if (exp->type == AST_FLOAT) return exp->payload.num_float;

	double a = calculate_expr(exp->payload.exp_binary.l);
	double b = calculate_expr(exp->payload.exp_binary.r);

	switch (exp->payload.exp_binary.op) {
		case '+':
			return a + b;
		case '*':
			return a * b;
		case '-':
			return a - b;
		case '/':
			return a / b;
	}

	return 0;
}

int main(void) {
	printf("Expressions parsing tests:\n");

	Token test1_tokens[] = {
		{TOK_INT, {0}, "10"},
		{TOK_MINUS, {0}, "-"},
		{TOK_LBRA, {0}, "("},
		{TOK_INT, {0}, "10"},
		{TOK_PLUS, {0}, "+"},
		{TOK_INT, {0}, "2"},
		{TOK_RBRA, {0}, ")"},
		{TOK_STAR, {0}, "*"},
		{TOK_INT, {0}, "4"},
		{TOK_MINUS, {0}, "-"},
		{TOK_INT, {0}, "3"},
		{TOK_SEMI, {0}, ";"},
	};

	Parser parser = {
		.cur_token = &(test1_tokens[0]),
	};

	AST_Node *res = parse_expression(&parser, EXPR_PARSING_VAR);
	double res_val = calculate_expr(res);

	if ((int) res_val == -35) printf("test 1 passed\n");
	else printf("test 1 failed\n");

	// (10 * (3 + 2)) / 2 = 25
	Token test2_tokens[] = {
		{TOK_LBRA, {0}, "("},
		{TOK_INT, {0}, "10"},
		{TOK_PLUS, {0}, "*"},
		{TOK_LBRA, {0}, "("},
		{TOK_INT, {0}, "3"},
		{TOK_PLUS, {0}, "+"},
		{TOK_INT, {0}, "2"},
		{TOK_RBRA, {0}, ")"},
		{TOK_RBRA, {0}, ")"},
		{TOK_STAR, {0}, "/"},
		{TOK_INT, {0}, "2"},
		{TOK_SEMI, {0}, ";"},
	};

	parser = (Parser) {
		.cur_token = &(test2_tokens[0]),
	};

	res = parse_expression(&parser, EXPR_PARSING_VAR);
	res_val = calculate_expr(res);

	if ((int) res_val == 25) printf("test 2 passed\n");
	else printf("test 2 failed\n");

	return 0;
}
