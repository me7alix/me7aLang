#ifndef TYPE_H
#define TYPE_H

#include "../thirdparty/cplus.h"

typedef enum {
	TYPE_NULL,
	TYPE_INT,  TYPE_UINT,
	TYPE_I8,   TYPE_U8,
	TYPE_I16,  TYPE_U16,
	TYPE_I32,  TYPE_U32,
	TYPE_I64,  TYPE_U64,
	TYPE_IPTR, TYPE_UPTR,

	TYPE_FLOAT,
	TYPE_F16,
	TYPE_F32,
	TYPE_F64,

	TYPE_BOOL,
	TYPE_POINTER,
	TYPE_ARRAY,
	TYPE_FUNCTION,
	// user types
	TYPE_STRUCT,
} TypeKind;

typedef struct AST_Node AST_Node;
typedef struct UserType UserType;
typedef struct Struct Struct;
typedef struct Type Type;

struct Type {
	TypeKind kind;

	union {
		struct {
			Type *base;
		} pointer;
		struct {
			Type *elem;
			size_t length;
		} array;
		struct {
			DA(Type) args;
			Type *ret;
		} func;
		UserType *user;
	} as;
};

typedef struct {
	enum {
		MBR_FIELD,
		MBR_METHOD,
	} kind;

	union {
		struct {
			Type type;
			char *id;
		} field;
		struct {
			AST_Node *func;
		} method;
	} as;
} Member;

struct UserType {
	TypeKind kind;
	char *id;

	union {
		struct {
			DA(Member) members;
		} ustruct;
	} as;
};

HT_DECL_STR(UserTypes, UserType)

#define is_pointer(t) ((t).kind == TYPE_ARRAY || (t).kind == TYPE_POINTER)
#define get_pointer_base(t) ((t).kind == TYPE_POINTER ? (t).as.pointer.base : (t).as.array.elem)

static bool compare_types(Type a, Type b) {
	if (is_pointer(a) && is_pointer(b)) {
		if (
			get_pointer_base(a)->kind != get_pointer_base(b)->kind &&
			!(get_pointer_base(a)->kind == TYPE_NULL ||
			get_pointer_base(b)->kind == TYPE_NULL)
		) return false;
	} else if (a.kind == TYPE_FUNCTION && b.kind == TYPE_FUNCTION) {
		if (!compare_types(*a.as.func.ret, *b.as.func.ret)) return false;
		if (a.as.func.args.count != b.as.func.args.count)   return false;
		for (size_t i = 0; i < a.as.func.args.count; i++) {
			if (!compare_types(a.as.func.args.items[i], b.as.func.args.items[i])) {
				return false;
			}
		}
	} else if (a.kind != b.kind) {
		return false;
	}

	return true;
}

static void render_type(StringBuilder *sb, Type t) {
	switch (t.kind) {
	case TYPE_NULL:     sb_appendf (sb, "null");  break;
	case TYPE_INT:      sb_appendf (sb, "int");   break;
	case TYPE_UINT:     sb_appendf (sb, "uint");  break;
	case TYPE_I8:       sb_appendf (sb, "i8");    break;
	case TYPE_U8:       sb_appendf (sb, "u8");    break;
	case TYPE_I16:      sb_appendf (sb, "i16");   break;
	case TYPE_U16:      sb_appendf (sb, "u16");   break;
	case TYPE_I32:      sb_appendf (sb, "i32");   break;
	case TYPE_U32:      sb_appendf (sb, "u32");   break;
	case TYPE_I64:      sb_appendf (sb, "i64");   break;
	case TYPE_U64:      sb_appendf (sb, "u64");   break;
	case TYPE_IPTR:     sb_appendf (sb, "iptr");  break;
	case TYPE_UPTR:     sb_appendf (sb, "uptr");  break;
	case TYPE_FLOAT:    sb_appendf (sb, "float"); break;
	case TYPE_F16:      sb_appendf (sb, "f16");   break;
	case TYPE_F32:      sb_appendf (sb, "f32");   break;
	case TYPE_F64:      sb_appendf (sb, "f64");   break;
	case TYPE_BOOL:     sb_appendf (sb, "bool");  break;
	case TYPE_FUNCTION: sb_appendf (sb, "func");  break;

	case TYPE_POINTER:
		sb_appendf(sb, "*");
		render_type(sb, *t.as.pointer.base);
		break;
	case TYPE_ARRAY:
		sb_appendf(sb, "[%zu]", t.as.array.length);
		render_type(sb, *t.as.array.elem);
		break;
	case TYPE_STRUCT:
		sb_appendf(sb, "%s", t.as.user->id);
	}
}

static Type TU8 = {.kind = TYPE_U8};
static Type TU0 = {.kind = TYPE_NULL};
static Type TUPTR = {.kind = TYPE_UPTR};

#endif //TYPE_H
