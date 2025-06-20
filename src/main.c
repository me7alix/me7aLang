#include <stdio.h>
#include "../include/lexer.h"

char *read_file(const char *filename) {
	FILE* file = fopen(filename, "rb");
	if (!file) {
		perror("Failed to open file");
		return NULL;
	}

	fseek(file, 0, SEEK_END);
	long filesize = ftell(file);
	rewind(file);

	char *buffer = (char *)malloc(filesize + 1);
	if (!buffer) {
		perror("Failed to allocate buffer");
		fclose(file);
		return NULL;
	}

	size_t read_size = fread(buffer, 1, filesize, file);
	if (read_size != filesize) {
		perror("Failed to read entire file");
		free(buffer);
		fclose(file);
		return NULL;
	}

	buffer[filesize] = '\0';

	fclose(file);
	return buffer;
}

int main(void) {
	char *code = read_file("main.vec");
	if (code == NULL) {
		perror("error while opening file\n");
		return 0;
	}

	Lexer lexer = lexer_alloc(code);
	lexer_lex(&lexer);

	for (int i = 0; i < lexer.tokens_num; i++) {
		printf("%s", tok_to_str(lexer.tokens[i].type));
		if (lexer.tokens[i].type == TOK_ID || lexer.tokens[i].type == TOK_INT || 
			lexer.tokens[i].type == TOK_FLOAT || lexer.tokens[i].type == TOK_STRING)
			printf("(%s)", lexer.tokens[i].data);
		printf(" ");
		if (lexer.tokens[i].type == TOK_LBRC ||
			lexer.tokens[i].type == TOK_RBRC ||
			lexer.tokens[i].type == TOK_SEMI)
			printf("\n");
	}

	lexer_free(&lexer);
	return 0;
}
