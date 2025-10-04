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


int main(int argc, char **argv) {
	char *input_file = NULL;
	char *output_bin = NULL;
	bool save_asm_output = false;

	if (argc == 1) {
		printf(
			"Usage: options file...\n"
			"Options:\n"
			"  -o   Output file path\n"
			"  -asm Returns asm output");
		return 0;
	}

	for (size_t i = 0; i < argc; i++) {
		if (strcmp(argv[i], "-o") == 0) {
			if (i >= argc) {
				fprintf(stderr, "invalid -o argument\n");
				return 1;
			}

			output_bin = argv[i+1];
			i++;
		} else if (strcmp(argv[i], "-asm") == 0) {
			save_asm_output = true;
		} else {
			if (argv[i][0] == '-') {
				fprintf(stderr, "invalid option\n");
				return 1;
			}

			input_file = argv[i];
		}
	}

	char *code = read_file(input_file);
	if (code == NULL) {
		perror("error while opening file\n");
		return 1;
	}

	Lexer lexer = {0};
	lexer_lex(&lexer, code);

	Parser parser = {0};
	parser_parse(&parser, lexer.tokens.items);

	NASM_Codegen cg = {0};
	nasm_codegen(&cg, &parser);

	char buf[512];
	char output_file[256];

	sprintf(output_file, "%s.asm", output_bin);
	write_to_file(output_file, sb_to_str(cg.code));

	sprintf(buf, "nasm -f elf64 %s", output_file); system(buf);
	sprintf(buf, "gcc -no-pie %s.o -o %s", output_bin, output_bin); system(buf);
	sprintf(buf, "rm %s.o", output_bin); system(buf);
	if (!save_asm_output) { sprintf(buf, "rm %s", output_file); system(buf); }

	lexer_free(&lexer);
	return 0;
}
