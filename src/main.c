#include <stdio.h>

#define DA_DEBUG
#include "../include/lexer.h"
#include "../include/parser.h"
#include "../include/ir.h"
#include "../thirdparty/sb.h"

#define SB_IMPLEMENTATION
#include "irdump.c"
#include "nasmgen.c"

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

void print_usage() {
	printf(
		"Usage: [options] file...\n"
		"Options:\n"
		"  -o   Output file path\n"
		"  -asm Saves asm output\n"
		"  -ir  Saves IR output\n");
}

int main(int argc, char **argv) {
	char *input_file = NULL;
	char *output_bin = "a.out";
	bool save_asm_output = false;
	bool save_ir_output = false;

	if (argc == 1) {
		print_usage();
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
		} else if (strcmp(argv[i], "-ir") == 0) {
			save_ir_output = true;
		} else if (
			strcmp(argv[i], "-h") == 0 ||
			strcmp(argv[i], "--help") == 0) {
			print_usage();
			return 0;
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

	char buf[512];
	char output_file[256];

	Program prog = ir_gen_prog(parser.program);
	StringBuilder cg = nasm_gen_prog(&prog);

	sprintf(output_file, "%s.asm", output_bin);
	write_to_file(output_file, sb_to_str(cg));

	sprintf(buf, "nasm -f elf64 %s", output_file); system(buf); printf("info: %s\n", buf);
	sprintf(buf, "gcc -no-pie ./examples/runtime.o %s.o -o %s", output_bin, output_bin); system(buf); printf("info: %s\n", buf);
	sprintf(buf, "rm %s.o", output_bin); system(buf);  printf("info: %s\n", buf);
	if (!save_asm_output) { sprintf(buf, "rm %s", output_file); system(buf); }
	if (save_ir_output) {
		sprintf(buf, "%s.ir", output_bin);
		ir_dump_prog(&prog, buf);
	}

	lexer_free(&lexer);
	da_free(&prog.funcs);
	return 0;
}
