#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "../include/parser.h"

// This function gets left and right brackets. size_t *l should point to TOK_LBRA
void parse_get_brackets(Parser *parser, size_t ptr, size_t *l, size_t *r) {
	bool is_first = true;
	int cnt = 0;

	for (size_t i = ptr; i < parser->tokens_num; i++) {
		if (parser->tokens[i].type == TOK_LBRA) {
			cnt++;
			if (is_first) {
				is_first = false;
				*l = i;
			}	
		} else if (parser->tokens[i].type == TOK_RBRA) {
			cnt--;
			*r = i;
		}

		if (cnt == 0) {
			return;
		}
	}
}

// This function gets left and right braces. size_t *l should point to TOK_LBRC
void parse_get_braces(Parser *parser, size_t ptr, size_t *l, size_t *r) {
	bool is_first = true;
	int cnt = 0;

	for (size_t i = ptr; i < parser->tokens_num; i++) {
		if (parser->tokens[i].type == TOK_LBRC) {
			cnt++;
			if (is_first) {
				is_first = false;
				*l = i;
			}
		} else if (parser->tokens[i].type == TOK_RBRC) {
			cnt--;
			*r = i;
		}

		if (cnt == 0) {
			return;
		}
	}
}

AST_Node *ast_alloc(AST_Node node) {
	AST_Node *new = malloc(sizeof(AST_Node)); 
	memcpy(new, &node, sizeof(node));
	return new;
}

void ast_block_add_node(AST_Node *parent, AST_Node node) {
	parent->payload.block.stmts[parent->payload.block.count++] = ast_alloc(node);
}

int64_t parse_int(char *data) {
	return atoi(data);
}

// 10 * 7 * 5 

AST_Node *parse_expression(Parser *parser, size_t ptr) {
	AST_Node *stack[256];
	size_t stack_ptr = 0;

	while (parser->tokens[ptr].type != TOK_SEMI) {
		switch (parser->tokens[ptr].type) {
			case TOK_INT:
				stack[stack_ptr++] = ast_alloc((AST_Node){
					.type = AST_INT,
					.payload.int_num.val = parse_int(parser->tokens[ptr].data),
				});
				break;

			default:
				printf("expression parsing error wrong token");
				break;
		}
	}

	return NULL;
}

void parse_block(Parser *parser, AST_Node *parent, size_t prev_ptr) {
	size_t ptr, to;
	parse_get_braces(parser, prev_ptr, &ptr, &to);

	while (ptr < to) {
		switch (parser->tokens[ptr].type) {
			case TOK_TYPE:
				if (parser->tokens[ptr-1].type == TOK_ID &&
					parser->tokens[ptr+1].type == TOK_EQ) {
					ast_block_add_node(parent, (AST_Node){
						.type = AST_VARIABLE,
						.payload.variable.name = ast_alloc((AST_Node){
							.type = AST_ID,
							.payload.identifier.id = parser->tokens[ptr-1].data,
						}),
						.payload.variable.type = ast_alloc((AST_Node){
							.type = AST_ID,
							.payload.identifier.id = parser->tokens[ptr].data,
						}),
						.payload.variable.exp = parse_expression(parser, ptr),
					});
				}
				break;
			default:
				break;
		}
		ptr++;
	}
}

void parser_parse(Parser *parser) {
	size_t ptr = 0;
	while (true) {
		switch (parser->tokens[ptr++].type) {
			case TOK_FUNC:

				break;
			default:
				break;
		}
	}
}
