#include <stdio.h>
#include <string.h>
#include <time.h>

#include "../include/preprocessor.h"
#include "../include/lexer.h"
#include "../include/parser.h"
#include "../include/tac_ir.h"

#include "./codegen/amd64-nasm.c"
#include "tac_ir_dump.c"

#if defined(_WIN32)
TargetPlatform tp = TP_WINDOWS;
#elif defined(__linux__)
TargetPlatform tp = TP_LINUX;
#elif defined(__APPLE__)
TargetPlatform tp = TP_MACOS;
#endif

char *read_file(const char *filename) {
	FILE* file = fopen(filename, "rb");
	if (!file) {
		return NULL;
	}

	fseek(file, 0, SEEK_END);
	long filesize = ftell(file);
	rewind(file);

	char *buffer = (char*) malloc(filesize + 1);
	if (!buffer) {
		fclose(file);
		return NULL;
	}

	size_t read_size = fread(buffer, 1, filesize, file);
	if (read_size != filesize) {
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

#define systemf(...) sprintf(buf, __VA_ARGS__); system(buf);

void print_usage() {
	printf(
		"Usage: [options] file\n"
		"Options:\n"
		"  -o   Output file path\n"
		"  -c   Compile to object file\n"
		"  -obj Object files\n"
		"  -ld  Link dynamicly\n"
		"  -asm Save assembler output\n"
		"  -ir  Save IR output\n");
}

int main(int argc, char **argv) {
	char *input_file;
	Imports imports = {0};
	da_append(&imports, "");
	da_append(&imports, ".");
	char *output_bin = "a.out";
	char *ld = "";
	char *obj_files = "";
	bool compile_to_obj = false;
	bool save_asm_output = false;
	bool save_ir_output = false;

	if (argc == 1) {
		print_usage();
		return 0;
	}

	for (size_t i = 1; i < argc; i++) {
		if (strcmp(argv[i], "-o") == 0) {
			if (i >= argc) {
				fprintf(stderr, "invalid -o argument\n");
				return 1;
			}

			output_bin = argv[++i];
		} else if (strcmp(argv[i], "-obj") == 0) {
			if (i >= argc) {
				fprintf(stderr, "invalid -obj argument\n");
				return 1;
			}

			obj_files = argv[++i];
		} else if (strcmp(argv[i], "-ld") == 0) {
			if (i >= argc) {
				fprintf(stderr, "invalid -ld argument\n");
				return 1;
			}

			ld = argv[++i];
		} else if (strcmp(argv[i], "-I") == 0) {
			if (i >= argc) {
				fprintf(stderr, "invalid -I argument\n");
				return 1;
			}

			da_append(&imports, argv[++i]);
		} else if (strcmp(argv[i], "-asm") == 0) {
			save_asm_output = true;
		} else if (strcmp(argv[i], "-ir") == 0) {
			save_ir_output = true;
		} else if (strcmp(argv[i], "-c") == 0) {
			compile_to_obj = true;
		} else if (
			strcmp(argv[i], "-h") == 0 ||
			strcmp(argv[i], "--help") == 0) {
			print_usage();
			return 0;
		} else {
			if (argv[i][0] == '-') {
				fprintf(stderr, "invalid option %s\n", argv[i]);
				return 1;
			}

			input_file = argv[i];
		}
	}

	char buf[512];
	char output_file[256];

	char *ep_code = read_file(input_file);
	if (!ep_code) {
		printf("error: no such file %s\n", input_file);
		return 1;
	}

	Lexer entry_point = lexer_lex(input_file, ep_code);
	preprocessor(&imports, &entry_point);

	Parser parser = parser_parse(entry_point.tokens.items);
	TAC_Program prog = tac_ir_gen_prog(&parser);
	if (save_ir_output) {
		sprintf(buf, "%s.ir", output_bin);
		ir_dump_prog(&prog, buf);
	}

	char *cg = nasm_gen_prog(&prog, tp);
	sprintf(output_file, "%s.asm", output_bin);
	write_to_file(output_file, cg);

	systemf("nasm -f %s %s", tp == TP_WINDOWS ? "win64" : "elf64", output_file);

	if (!compile_to_obj) {
		switch (tp) {
			case TP_MACOS:
			case TP_LINUX:
				systemf(
					"ld -o %s %s.o %s %s -L/usr/lib -lc "
					"-dynamic-linker /lib64/ld-linux-x86-64.so.2 /usr/lib/crt1.o /usr/lib/crti.o /usr/lib/crtn.o",
					output_bin, output_bin, obj_files, ld);
				systemf("rm %s.o", output_bin);
				break;
			case TP_WINDOWS:
				systemf("gcc -fPIE -o %s %s.obj %s %s", output_bin, output_bin, obj_files, ld);
				systemf("rm %s.obj", output_bin);
				break;
		}
	}

	if (!save_asm_output) {
		systemf("rm %s", output_file);
	}

	return 0;
}
