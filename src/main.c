#include <stdio.h>
#include "../include/lexer.h"
#include "../include/parser.h"

#include "codegen.c"

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

int write_to_file(const char *filename, const char *text) {
	FILE *file = fopen(filename, "w");
	if (file == NULL) {
		perror("Error opening file");
		return -1;
	}

	if (fputs(text, file) == EOF) {
		perror("Error writing to file");
		fclose(file);
		return -1;
	}

	if (fclose(file) == EOF) {
		perror("Error closing file");
		return -1;
	}

	return 0;
}


int main(void) {
	char *code = read_file("test.met");
	if (code == NULL) {
		perror("error while opening file\n");
		return 0;
	}

	Lexer lexer = lexer_alloc(code);
	lexer_lex(&lexer);

	Parser parser = {0};
	parser_parse(&parser, lexer.tokens);

	NASM_Codegen cg = {0};
	nasm_codegen(&cg, &parser);

	printf("NASM output:\n%s", sb_to_str(cg.code));
	write_to_file("prog.asm", sb_to_str(cg.code));
	system("nasm -f elf64 prog.asm");
	system("gcc -no-pie prog.o -o prog");
	system("rm prog.asm");
	system("rm prog.o");

	lexer_free(&lexer);
	return 0;
}
