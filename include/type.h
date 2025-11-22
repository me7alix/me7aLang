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
		struct { Type *base; } pointer;
		struct { Type *elem; size_t length; } array;
		struct { Type **params; size_t param_count; Type *ret; } func;
		UserType *user;
	};
};

typedef  struct {
	Type type;
	char *id;
} Field;

struct UserType {
	TypeKind kind;

	union {
		struct {
			DA(Field) fields;
		} ustruct;
	};
};

HT_DECL_STR(UserTypes, UserType)

#endif //TYPE_H
