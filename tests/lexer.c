#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include "../include/lexer.h"

#define ARR_LEN(arr) (sizeof(arr)/sizeof(arr[0]))

bool tok_compare(Token a, Token b) {
	if (a.type == b.type) {
		if (a.type == TOK_STRING || a.type == TOK_INT ||
			a.type == TOK_FLOAT || a.type == TOK_ID ||
			a.type == TOK_CHAR || a.type == TOK_TYPE) {
			if (strcmp(a.data, b.data))	{
				return false;
			}
		}
		return true;
	}
	return false;
}


bool test_lexer(char *code, Token *tokens, size_t tokens_num) {
	Lexer lexer = {0};
	lexer_lex(&lexer, code);

	if (tokens_num != lexer.tokens.count) 
		return false;

	for (size_t i = 0; i < lexer.tokens.count; i++) {
		if (!tok_compare(da_get(&lexer.tokens, i), tokens[i])) {
			printf("error: %s | %s\n", tok_to_str(da_get(&lexer.tokens, i).type), tok_to_str(tokens[i].type));
			return false;
		}
	}

	lexer_free(&lexer);
	return true;
}

void run_test(char *code, Token *tokens, size_t tokens_num, size_t test_num) {
	if (test_lexer(code, tokens, tokens_num))
		printf("test %zu passed\n", test_num);
	else
		printf("test %zu failed\n", test_num);
}

int main() {
	printf("Lexer tests:\n");
	char *test_code_1 = "func main() {\n a: i32 = 10 + 2\nb := 2; }";
	Token test_tokens_1[] = {
		{ .type = TOK_FUNC },
		{ .type = TOK_ID, .data = "main" },
		{ .type = TOK_OPAR },
		{ .type = TOK_CPAR },
		{ .type = TOK_OBRA },
		{ .type = TOK_ID, .data = "a" },
		{ .type = TOK_TYPE, .data = "i32" },
		{ .type = TOK_EQ },
		{ .type = TOK_INT, .data = "10" },
		{ .type = TOK_PLUS },
		{ .type = TOK_INT, .data = "2" },
		{ .type = TOK_SEMI },
		{ .type = TOK_ID, .data = "b" },
		{ .type = TOK_ASSIGN },
		{ .type = TOK_INT, .data = "2" },
		{ .type = TOK_SEMI },
		{ .type = TOK_CBRA },
		{ .type = TOK_EOF },
	};

	run_test(test_code_1, test_tokens_1, ARR_LEN(test_tokens_1), 1);

	char *test_code_2 = "func print_hi() {\n println(\"hi\")\n}";
	Token test_tokens_2[] = {
		{ .type = TOK_FUNC },
		{ .type = TOK_ID, .data = "print_hi" },
		{ .type = TOK_OPAR },
		{ .type = TOK_CPAR },
		{ .type = TOK_OBRA },
		{ .type = TOK_ID, .data = "println" },
		{ .type = TOK_OPAR },
		{ .type = TOK_STRING, .data = "hi" },
		{ .type = TOK_CPAR },
		{ .type = TOK_SEMI },
		{ .type = TOK_CBRA },
		{ .type = TOK_EOF },
	};

	run_test(test_code_2, test_tokens_2, ARR_LEN(test_tokens_2), 2);

	char *test_code_3 = "if (a == b && c != true || 10 == 10)";
	Token test_tokens_3[] = {
		{ .type = TOK_IF_SYM },
		{ .type = TOK_OPAR },
		{ .type = TOK_ID, .data = "a" },
		{ .type = TOK_EQ_EQ },
		{ .type = TOK_ID, .data = "b" },
		{ .type = TOK_AND },
		{ .type = TOK_ID, .data = "c" },
		{ .type = TOK_NOT_EQ },
		{ .type = TOK_ID, .data = "true" },
		{ .type = TOK_OR },
		{ .type = TOK_INT, .data = "10" },
		{ .type = TOK_EQ_EQ },
		{ .type = TOK_INT, .data = "10" },
		{ .type = TOK_CPAR },
		{ .type = TOK_EOF },
	};

	run_test(test_code_3, test_tokens_3, ARR_LEN(test_tokens_3), 3);

	printf("\n");
	return 0;
}
