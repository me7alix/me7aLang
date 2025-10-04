CC = gcc
CFLAGS = -std=c99 -Wall -DDA_RUNTIME_CHECKS

build:
	mkdir -p build
	#$(CC) $(CFLAGS) src/lexer.c tests/lexer.c -o ./build/lexer_tests
	#$(CC) $(CFLAGS) src/lexer.c src/parser.c src/parser_expr.c tests/parser_expr.c -o ./build/parser_expr_tests
	$(CC) $(CFLAGS) -ggdb src/main.c src/lexer.c src/parser.c src/parse_expr.c src/irgen.c -o ./build/compiler

tests: build
	./build/lexer_tests
	./build/parser_expr_tests

clean:
	rm -rf build
