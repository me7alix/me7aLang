mkdir -p build
cc src/lexer.c tests/lexer.c -o ./build/lexer_tests && ./build/lexer_tests
#cc src/lexer.c src/main.c src/parser.c -o ./build/compiler && ./build/compiler
cc src/parser.c src/lexer.c tests/parser_expr.c -o ./build/parser_expr_tests &&./build/parser_expr_tests 
