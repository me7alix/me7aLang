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
	};
};

typedef struct AST_Node AST_Node;

typedef struct {
	enum {
		STMEM_FIELD,
		STMEM_METHOD,
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
} StructMember;

struct UserType {
	TypeKind kind;
	char *id;

	union {
		struct {
			DA(StructMember) members;
		} ustruct;
	};
};

HT_DECL_STR(UserTypes, UserType*)

#define is_pointer(t) ((t).kind == TYPE_ARRAY || (t).kind == TYPE_POINTER)
#define get_pointer_base(t) ((t).kind == TYPE_POINTER ? (t).pointer.base : (t).array.elem)

#define type_new(...) type_alloc((Type){__VA_ARGS__})
static Type *type_alloc(Type type) {
	Type *nt = malloc(sizeof(*nt));
	*nt = type;
	return nt;
}

static bool compare_types(Type a, Type b) {
	if (is_pointer(a) && is_pointer(b)) {
		if (
			get_pointer_base(a)->kind != get_pointer_base(b)->kind &&
			!(get_pointer_base(a)->kind == TYPE_NULL ||
			get_pointer_base(b)->kind == TYPE_NULL)
		) return false;
	} else if (a.kind == TYPE_FUNCTION && b.kind == TYPE_FUNCTION) {
		if (!compare_types(*a.func.ret, *b.func.ret)) return false;
		if (a.func.args.count != b.func.args.count)   return false;
		for (size_t i = 0; i < a.func.args.count; i++) {
			if (!compare_types(a.func.args.items[i], b.func.args.items[i])) {
				return false;
			}
		}
	} else if (a.kind != b.kind) {
		return false;
	}

	return true;
}

#endif //TYPE_H
