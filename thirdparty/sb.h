/*
 * sb.h — Minimal StringBuilder for C (header + implementation)
 *
 * Small, easy-to-embed StringBuilder that grows automatically.
 * Intended for simple string construction where repeated strcat calls
 * would be inefficient.
 *
 */

#ifndef SB_H
#define SB_H

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

typedef struct {
	char   *str;
	size_t  cnt;
	size_t  cap;
} StringBuilder;

void sb_append_str(StringBuilder *sb, const char *s);
void sb_append_strf(StringBuilder *sb, const char *fmt, ...);
void sb_append_char(StringBuilder *sb, char ch);
void sb_reset(StringBuilder *sb);
void sb_destroy(StringBuilder *sb);
const char *sb_to_str(StringBuilder sb);

#endif

#ifdef SB_IMPLEMENTATION

void sb_capacity_grow(StringBuilder *sb, size_t extra) {
	if (!sb) return;
	if (sb->cap == 0) {
		if (extra <= 16) sb->cap = 32;
		else sb->cap = extra * 2;
		sb->str = (char *) malloc(sizeof(char) * sb->cap);
		return;
	}

	size_t required = sb->cnt + extra + 1;
	if (required <= sb->cap) return;
	while (sb->cap < required) {
		sb->cap *= 2;
	}
	sb->str = (char *) realloc(sb->str, sb->cap);
}

void sb_append_str(StringBuilder *sb, const char *s) {
	if (!sb) return;
	size_t len = strlen(s);
	sb_capacity_grow(sb, len);
	memcpy(sb->str + sb->cnt, s, len);
	sb->cnt += len;
	sb->str[sb->cnt] = '\0';
}

void sb_append_strf(StringBuilder *sb, const char *fmt, ...) {
	if (!sb || !fmt) return;
	va_list args, args_copy;
	va_start(args, fmt);

	va_copy(args_copy, args);

	int len = vsnprintf(NULL, 0, fmt, args);
	va_end(args);

	if (len < 0) {
		va_end(args_copy);
		return;
	}

	sb_capacity_grow(sb, (size_t)len);

	vsnprintf(sb->str + sb->cnt, sb->cap - sb->cnt, fmt, args_copy);
	sb->cnt += (size_t)len;

	va_end(args_copy);
}

void sb_append_char(StringBuilder *sb, char ch) {
	if (!sb) return;
	sb_capacity_grow(sb, 1);
	sb->str[sb->cnt++] = ch;
	sb->str[sb->cnt] = '\0';
}

void sb_reset(StringBuilder *sb) {
	if (!sb) return;
	sb->cnt = 0;
	if (sb->str) sb->str[0] = '\0';
}

const char *sb_to_str(StringBuilder sb) {
	return (const char *) sb.str;
}

void sb_destroy(StringBuilder *sb) {
	if (!sb) return;
	free(sb->str);
	sb->str = NULL;
	sb->cnt = sb->cap = 0;
}

#endif
