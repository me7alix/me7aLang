CC = gcc
CFLAGS = -std=c99 -Wall

build:
	mkdir -p build
	$(CC) $(CFLAGS) src/lexer.c tests/lexer.c -o ./build/lexer_tests && ./build/lexer_tests
	$(CC) $(CFLAGS) src/parser_expr.c tests/parser_expr.c -o ./build/parser_expr_tests &&./build/parser_expr_tests 

tests: build
	./build/lexer_tests
	./build/parser_expr_tests

clean:
	rm -rf build
