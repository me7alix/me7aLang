CC = gcc
CFLAGS = -std=c99 -Wall -D_CP_RUNTIME_CHECKS -D_GNU_SOURCE

build:
	mkdir -p build
	$(CC) -c examples/runtime.c -o build/runtime.o
	$(CC) $(CFLAGS) -ggdb src/main.c src/preprocessor.c src/lexer.c src/parser.c src/parse_expr.c src/irgen.c -o ./build/metc

clean:
	rm -rf build
