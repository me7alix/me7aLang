#include "amd64.h"

HT_IMPL_NUM(OffTable, uint, uint)

size_t get_reg_size(Type t) {
	switch (t.kind) {
	case TYPE_BOOL:
	case TYPE_I8:
	case TYPE_U8:
		return 0;
	case TYPE_I16:
	case TYPE_U16:
		return 1;
	case TYPE_U32:
	case TYPE_I32:
	case TYPE_INT:
	case TYPE_UINT:
		return 2;
	case TYPE_FUNCTION:
	case TYPE_ARRAY:
	case TYPE_POINTER:
	case TYPE_IPTR:
	case TYPE_UPTR:
	case TYPE_I64:
	case TYPE_U64:
		return 3;
	default:
		UNREACHABLE;
	}
}
