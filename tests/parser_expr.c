#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

#define DA_DEBUG
#include "../include/lexer.h"
#include "../include/parser.h"

#define ARR_LEN(arr) (sizeof(arr)/sizeof(arr[0]))

double calculate_expr(AST_Node *exp) {
	if (exp->type == AST_INT) return exp->num_int;
	if (exp->type == AST_FLOAT) return exp->num_float;

	double a = calculate_expr(exp->exp_binary.l);
	double b = calculate_expr(exp->exp_binary.r);

	switch (exp->exp_binary.op) {
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

void test_expression(int test_num, char *expr_str, double result) {
	Lexer lexer = lexer_alloc(expr_str);
	lexer_lex(&lexer);
	Parser parser = {.cur_token = lexer.tokens};

	AST_Node *res = parse_expr(&parser, EXPR_PARSING_VAR);
	double res_val = calculate_expr(res);

	if ((int) res_val == (int) result) printf("test %d passed\n", test_num);
	else printf("test %d failed\n", test_num);
}

int main(void) {
	printf("Expressions parsing tests:\n");

	test_expression(1, "10 - (10 + 2) * 4 - 3;", -41);
	test_expression(2, "223 - 89 + (78 - (1 * 3));", 209);
	test_expression(3, "5;", 5);

	return 0;
}
