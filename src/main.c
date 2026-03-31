#include <stdio.h>
#include <string.h>
#include <time.h>

#include "../include/platform.h"
#include "../include/preprocessor.h"
#include "../include/lexer.h"
#include "../include/parser.h"
#include "../include/tac_ir.h"

// codegens
#include "./codegen/amd64-nasm.c"

#if defined(_WIN32)
TargetPlatform tp = TP_WINDOWS;
#elif defined(__linux__)
TargetPlatform tp = TP_LINUX;
#elif defined(__APPLE__)
TargetPlatform tp = TP_MACOS;
#else
TargetPlatform tp = TP_NULL;
#endif

void throw_error(Location loc, char *error) {
	size_t lines_num = loc.line_num + 1;
	size_t chars_num = loc.line_char-loc.line_start + 1;
	printf("%s:%zu:%zu: %s\n", loc.file, lines_num, chars_num, error);

	loc.line_char = loc.line_start;
	char error_pointer[128];
	size_t cnt = 0;

	while (*loc.line_char != '\n' && *loc.line_char != '\0'){
		fprintf(stderr, "%c", *loc.line_char);
		if (cnt < chars_num - 1) {
			if (*loc.line_char != '\t') error_pointer[cnt++] = ' ';
			else                        error_pointer[cnt++] = '\t';
		}
		loc.line_char++;
	}

	fprintf(stderr, "\n");
	error_pointer[cnt++] = '^';
	error_pointer[cnt] = '\0';
	fprintf(stderr, "%s\n", error_pointer);
	exit(1);
}

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

char buf[512];
#define systemf(...) \
	do { \
		sprintf(buf, __VA_ARGS__); \
		system(buf); \
	} while(0)

void print_usage() {
	printf(
		"Usage: [options] files\n"
		"Options:\n"
		"  -o   Output file path\n"
		"  -c   Compile to object file\n"
		"  -lf  Linker flags\n"
		"  -asm Save assembler output\n"
		"  -ir  Save IR output\n");
}

bool is_src_file(char *str) {
	size_t strl = strlen(str);
	for (int rp = strl - 1; rp >= 0; rp--) {
		if (str[rp] == '.') {
			if (strcmp(str + rp + 1, "m7") == 0) {
				return true;
			}
		}
	}

	return false;
}

static void get_folder(char *dst, const char *file) {
	const char *slash = strrchr(file, '/');
	if (!slash) slash = strrchr(file, '\\');
	if (slash) {
		size_t len = slash - file;
		memcpy(dst, file, len);
		dst[len] = '\0';
	} else sprintf(dst, "./");
}

static void get_file(char *dst, const char *file) {
	const char *slash = strrchr(file, '/');
	if (!slash) slash = strrchr(file, '\\');
	if (slash) {
		size_t len = strlen(file) - (slash - file + 1);
		memcpy(dst, slash + 1, len);
		dst[len] = '\0';
	} else sprintf(dst, file);
}

int main(int argc, char **argv) {
	if (tp == TP_NULL) {
		fprintf(stderr, "Unsupported platform\n");
		return 1;
	}

	Imports imports = {0};
	da_append(&imports, ""); // reserved
	da_append(&imports, ".");

	DA(char*) src_files = {0};
	DA(char*) obj_files = {0};

	char *output_bin       = "a.out";
	char *link_dynamically = "";
	bool compile_to_obj    = false;
	bool save_asm_output   = false;
	bool save_ir_output    = false;

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
		} else if (strcmp(argv[i], "-lf") == 0) {
			if (i >= argc) {
				fprintf(stderr, "invalid -lf argument\n");
				return 1;
			}

			link_dynamically = argv[++i];
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

			if (is_src_file(argv[i])) {
				da_append(&src_files, argv[i]);
			} else {
				da_append(&obj_files, argv[i]);
			}
		}
	}

	DA(char*) srcs = {0};
	da_foreach (char*, src_file, &src_files) {
		char build_folder[2048];
		get_folder(build_folder, output_bin);
		char src_file_name[2048];
		get_file(src_file_name, *src_file);
		StringBuilder sb = {0};
		sb_appendf(&sb, "%s/%s", build_folder, src_file_name);
		da_append(&srcs, sb.items);
	}

	for (size_t i = 0; i < srcs.count; i++) {
		char output_file[2048];
		char *src_file = src_files.items[i];

		char *ep_code = read_file(src_file);
		if (!ep_code) {
			fprintf(stderr, "error: no such file %s\n", src_file);
			return 1;
		}

		Lexer entry_point = lexer_lex(src_file, ep_code);
		PreprocCtx preprocCtx = {.imports = &imports, .lexer = entry_point};
		preprocessor(&preprocCtx, false);
		entry_point = preprocCtx.lexer;

		Parser parser = parser_parse(entry_point.tokens.items);
		TAC_Program prog = tac_ir_gen_prog(&parser);
		if (save_ir_output) {
			sprintf(buf, "%s.ir", srcs.items[i]);
			tac_ir_dump_prog(&prog, buf);
		}

		char *cg = nasm_gen_prog(&prog, tp);
		sprintf(output_file, "%s.asm", srcs.items[i]);
		write_to_file(output_file, cg);

		systemf(
			"nasm -f %s %s",
			tp == TP_WINDOWS ? "win64"   :
			tp == TP_LINUX   ? "elf64"   :
			tp == TP_MACOS   ? "macho64" : "",
			output_file);
	}

	char *obj_ext =
		tp == TP_WINDOWS ? "obj" :
		tp == TP_LINUX   ? "o"   :
		tp == TP_MACOS   ? "o"   : "";

	if (!save_asm_output) {
		switch (tp) {
		case TP_MACOS:
		case TP_LINUX:
			da_foreach (char*, src, &srcs) {
				systemf("rm %s.asm", *src);
			} break;

		case TP_WINDOWS:
			da_foreach (char*, src, &srcs) {
				systemf("del /F /Q %s.asm", *src);
			}
		}
	}

	if (!compile_to_obj) {
		StringBuilder cmd = {0};
		sb_appendf(&cmd, "gcc -no-pie -o \"%s\"", output_bin);

		da_foreach (char*, src, &srcs)
			sb_appendf(&cmd, " %s.%s", *src, obj_ext);

		da_foreach (char*, obj_file, &obj_files)
			sb_appendf(&cmd, " %s", *obj_file);

		sb_appendf(&cmd, " %s", link_dynamically);
		system(cmd.items);

		switch (tp) {
		case TP_MACOS:
		case TP_LINUX:
			da_foreach (char*, src, &srcs) {
				systemf("rm %s.o", *src);
			} break;

		case TP_WINDOWS:
			da_foreach (char*, src, &srcs) {
				systemf("del /F /Q %s.obj", *src);
			}
		}
	}

	return 0;
}
