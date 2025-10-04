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
 *        - da_unordered_remove(&da, i)  // remove element (fast, order not preserved)
 *        - da_ordered_remove(&da, i)    // remove element (order preserved, slower)
 *        - da_free(&da)                 // release memory
 *
 *   Capacity management is handled automatically:
 *        - The array grows when full (capacity × 2).
 *        - The array shrinks when usage falls below 25%.
 *
 * Debugging:
 *   Define DA_DEBUG before including this header to enable runtime 
 *   error checks (invalid indices, empty access, etc.). Error messages 
 *   are printed to stderr and abort the program. For release builds, 
 *   simply omit DA_DEBUG to remove these checks.
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

#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <stdio.h>

#ifdef DA_DEBUG
#define _da_error_if(b, ...) \
	if (b) { \
		fprintf(stderr, "%s:%d: da error: ", __FILE__, __LINE__); \
		fprintf(stderr, __VA_ARGS__); \
		exit(1); \
	}
#else
#define _da_error_if(...)
#endif

#define da(type) struct { type *items; size_t count, capacity; }

#define _da_capacity_grow(da) \
	do { \
		if ((da)->capacity == 0) { \
			if ((da)->count == 0) (da)->capacity = 8; \
			else (da)->capacity = (da)->count * 2; \
			(da)->items = malloc(sizeof(*(da)->items) * (da)->capacity); \
		} else { \
			if ((da)->count >= (da)->capacity) (da)->capacity = (da)->count * 2; \
			(da)->items = realloc((da)->items, sizeof(*(da)->items) * (da)->capacity); \
		} \
	} while(0)

#define da_shrink(da) \
	do { \
		if ((da)->capacity == 0) break; \
		if ((da)->count <= (da)->capacity / 4) (da)->capacity = (da)->count * 2; \
		(da)->items = realloc((da)->items, sizeof(*(da)->items) * (da)->capacity); \
	} while(0)

#define da_append(da, item) \
	do { \
		_da_capacity_grow(da); \
		(da)->items[(da)->count++] = (item); \
	} while(0)

#define da_insert(da, index, item) \
	do{ \
		(da)->count++; \
		_da_capacity_grow(da); \
		memcpy((da)->items+(index)+1, (da)->items+(index), sizeof(*(da)->items)*((da)->count-index)); \
		da_get(da, (index)) = item; \
	} while(0)

#define da_get(da, index) \
	(*({ \
	   _da_error_if(index >= (da)->count || (index) < 0, "index %" PRId64 " is out of the range 0..%zu\n", \
			   ((int64_t)(index)), (da)->count); \
		(da)->items + index; \
	})) \

#define da_last(da) \
	({ \
		_da_error_if((da)->count == 0, "da_last on empty dynamic array\n"); \
		(da)->items[(da)->count - 1]; \
	})

#define da_resize(da, cnt) \
	do { \
		(da)->count = (cnt); \
		_da_capacity_grow(da); \
		da_shrink(da); \
	} while(0)

#define da_unordered_remove(da, index) \
	do { \
		da_get(da, (index)) = da_last(da); \
		(da)->count--; \
	} while(0)

#define da_ordered_remove(da, index) \
	do { \
		da_get(da, (index)) = da_last(da); \
		memcpy((da)->items+(index), (da)->items+(index)+1, \
				sizeof(*(da)->items)*((da)->count-index)); \
		(da)->count--; \
	} while(0)

#define da_free(da) \
	do { \
		if ((da)->items) \
			free((da)->items); \
		(da)->count = 0; \
		(da)->capacity = 0; \
	} while(0)

#define da_remove_last(da) \
	do { \
		_da_error_if((da)->count == 0, "da_remove_last on empty dynamic array\n"); \
		(da)->count--; \
	} while (0)

#endif
