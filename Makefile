CC = gcc
CFLAGS = -std=c99 -Wall -D_BC_RUNTIME_CHECKS -D_GNU_SOURCE

build:
	mkdir -p build
	#$(CC) $(CFLAGS) src/lexer.c tests/lexer.c -o ./build/lexer_tests
	#$(CC) $(CFLAGS) src/lexer.c src/parser.c src/parser_expr.c tests/parser_expr.c -o ./build/parser_expr_tests
	$(CC) -c examples/runtime.c -o build/runtime.o
	$(CC) $(CFLAGS) -ggdb src/main.c src/preprocessor.c src/lexer.c src/parser.c src/parse_expr.c src/irgen.c -o ./build/metc

tests: build
	./build/lexer_tests
	./build/parser_expr_tests

clean:
	rm -rf build
