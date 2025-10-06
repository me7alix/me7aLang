/*
 * da.h — A lightweight, header-only dynamic array library for C
 *
 * This library provides simple macros to manage resizable arrays in C 
 * without the need for manual capacity tracking or boilerplate code.
 * 
 * Usage:
 *   1. Define a struct that contains at least these fields:
 *        - <type> *items   // pointer to array elements
 *        - size_t count    // number of elements stored
 *        - size_t capacity // total allocated slots
 *      Or use da(type) macro
 *
 *   2. Use the provided macros to manipulate your dynamic array:
 *        - da_append(&da, item)         // append element
 *        - da_resize(&da, item)         // change count
 *        - da_shrink(&da, item)         // shrink the capacity of the
 *        - da_insert(&da, i, item)      // insert element at index
 *        - da_get(&da, i)               // access element at index (with bounds check in debug mode)
 *        - da_last(&da)                 // access the last element
 *        - da_remove_unordered(&da, i)  // remove element (fast, order not preserved)
 *        - da_remove_ordered(&da, i)    // remove element (order preserved, slower)
 *        - da_free(&da)                 // release memory
 *
 *   Capacity management is handled automatically:
 *        - The array grows when full (capacity × 2).
 *        - The array shrinks when usage falls below 25%.
 *
 * Debugging:
 *   Define DA_RUNTIME_CHECKS before including this header to enable runtime
 *   error checks (invalid indices, empty access, etc.)
 *
 * Features:
 *   - Minimal, single-header implementation
 *   - No external dependencies
 *   - Familiar API with flexible memory management
 *   - Optional debug safety checks
 *
 */

#ifndef DA_H
#define DA_H

#ifndef DA_INIT_CAP
#define DA_INIT_CAP 256
#endif

#ifndef DA_REALLOC
#include <stdlib.h>
#define DA_REALLOC realloc
#endif

#ifndef DA_FREE
#include <stdlib.h>
#define DA_FREE free
#endif

#ifndef DA_MEMCPY
#include <string.h>
#define DA_MEMCPY memcpy
#endif

#ifndef DA_RUNTIME_CHECKS
#define DA_ASSERT(a) ((void)0)
#else
#ifndef DA_ASSERT
#include <assert.h>
#define DA_ASSERT assert
#endif
#endif

#define da(type) struct { type *items; size_t count, capacity; }

#define da_foreach(Type, it, da) for (Type *it = (da)->items; it < (da)->items + (da)->count; ++it)

#define da_capacity_grow(da) \
	do { \
		if ((da)->capacity == 0) { \
			if ((da)->count == 0) (da)->capacity = 8; \
			else (da)->capacity = (da)->count * 2; \
			(da)->items = DA_REALLOC(NULL, sizeof(*(da)->items) * (da)->capacity); \
		} else { \
			if ((da)->count >= (da)->capacity) (da)->capacity = (da)->count * 2; \
			(da)->items = DA_REALLOC((da)->items, sizeof(*(da)->items) * (da)->capacity); \
		} \
	} while(0)

#define da_shrink(da) \
	do { \
		if ((da)->capacity == 0) break; \
		if ((da)->count <= (da)->capacity / 4) (da)->capacity = (da)->count * 2; \
		(da)->items = DA_REALLOC((da)->items, sizeof(*(da)->items) * (da)->capacity); \
	} while(0)

#define da_append(da, item) \
	do { \
		da_capacity_grow(da); \
		(da)->items[(da)->count++] = (item); \
	} while(0)

#define da_insert(da, index, item) \
	do { \
		(da)->count++; \
		da_capacity_grow(da); \
		DA_MEMCPY((da)->items+(index)+1, (da)->items+(index), sizeof(*(da)->items)*((da)->count-index)); \
		da_get(da, (index)) = item; \
	} while(0)

#define da_get(da, index) \
	(da)->items[DA_ASSERT((index) >= 0 && (index) < (da)->count), (index)]

#define da_last(da) \
	(da)->items[DA_ASSERT((da)->count > 0), ((da)->count - 1)]

#define da_resize(da, cnt) \
	do { \
		(da)->count = (cnt); \
		da_capacity_grow(da); \
		da_shrink(da); \
	} while(0)

#define da_remove_unordered(da, index) \
	do { \
		da_get(da, (index)) = da_last(da); \
		(da)->count--; \
	} while(0)

#define da_remove_ordered(da, index) \
	do { \
		da_get(da, index) = da_last(da); \
		memcpy((da)->items+(index), (da)->items+(index)+1, \
				sizeof(*(da)->items)*((da)->count-index)); \
		(da)->count--; \
	} while(0)

#define da_free(da) \
	do { \
		if ((da)->items) \
			DA_FREE((da)->items); \
		(da)->count = 0; \
		(da)->capacity = 0; \
	} while(0)

#define da_remove_last(da) \
	do { \
		DA_ASSERT((da)->count > 0); \
		(da)->count--; \
	} while (0)

#endif
