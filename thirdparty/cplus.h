#ifndef _CP_H_
#define _CP_H_

#include <stdio.h>
#include <string.h>

#ifndef CP_DA_INIT_CAP
#define CP_DA_INIT_CAP 256
#endif

#ifndef CP_REALLOC
#include <stdlib.h>
#define CP_REALLOC realloc
#endif

#ifndef CP_CALLOC
#include <stdlib.h>
#define CP_CALLOC calloc
#endif

#ifndef CP_FREE
#include <stdlib.h>
#define CP_FREE free
#endif

#ifndef CP_MEMMOVE
#include <string.h>
#define CP_MEMMOVE memmove
#endif

#ifndef _CP_RUNTIME_CHECKS
#define CP_ASSERT(a) ((void)0)
#else
#ifndef CP_ASSERT
#include <assert.h>
#define CP_ASSERT assert
#endif
#endif

#ifdef __cplusplus
#define CP_DECLTYPE_CAST(T) (decltype(T))
#else
#define CP_DECLTYPE_CAST(T)
#endif // __cplusplus

#ifndef CP_INT_DEFINED
    #ifdef CP_USE_INT /* optional for any system that might not have stdint.h */
        typedef unsigned char       u8;
        typedef signed char         i8;
        typedef unsigned short     u16;
        typedef signed short        i16;
        typedef unsigned long int  u32;
        typedef signed long int    i32;
        typedef unsigned long long u64;
        typedef signed long long   i64;
    #else /* use stdint standard types instead of c "standard" types */
        #include <stdint.h>

        typedef uint8_t  u8;
        typedef int8_t   i8;
        typedef uint16_t u16;
        typedef int16_t  i16;
        typedef uint32_t u32;
        typedef int32_t  i32;
        typedef uint64_t u64;
        typedef int64_t  i64;
    #endif
    #define CP_INT_DEFINED
#endif

#ifndef ARR_LEN
#define ARR_LEN(arr) (sizeof(arr)/sizeof(arr[0]))
#endif

/* Dynamic array */

#define DA(type) struct { type *items; size_t count, capacity; }

#define da_foreach(Type, it, da) \
    for (Type *it = (da)->items; it < (da)->items + (da)->count; ++it)

#define da_reserve(da, expected_capacity) \
    do { \
        if ((expected_capacity) > (da)->capacity) { \
            size_t new_capacity = (da)->capacity ? (da)->capacity : CP_DA_INIT_CAP; \
            while ((expected_capacity) > new_capacity) { \
                new_capacity *= 2; \
            } \
            void *new_items = CP_REALLOC((da)->items, new_capacity * sizeof(*(da)->items)); \
            CP_ASSERT(new_items != NULL || (expected_capacity) == 0); \
            if (new_items || (expected_capacity) == 0) { \
                (da)->items = CP_DECLTYPE_CAST((da)->items)new_items; \
                (da)->capacity = new_capacity; \
            } \
        } \
    } while (0)

#define da_shrink(da) \
    do { \
        if ((da)->capacity == 0) break; \
        if ((da)->count <= (da)->capacity / 4) (da)->capacity = (da)->count * 2; \
        (da)->items = CP_REALLOC((da)->items, sizeof(*(da)->items) * (da)->capacity); \
    } while (0)

#define da_append(da, item) \
    do { \
        da_reserve((da), (da)->count + 1); \
        (da)->items[(da)->count++] = (item); \
    } while (0)

#define da_insert(da, index, item) \
    do { \
        size_t _idx = (size_t)(index); \
        size_t _old = (da)->count; \
        CP_ASSERT(_idx <= _old); \
        da_reserve((da), _old + 1); \
        if (_idx < _old) { \
            CP_MEMMOVE((da)->items + _idx + 1, \
                       (da)->items + _idx, \
                       sizeof *(da)->items * (_old - _idx)); \
        } \
        (da)->count = _old + 1; \
        da_get(da, _idx) = (item); \
    } while (0)

#define da_get(da, index) \
    (da)->items[CP_ASSERT((index) >= 0 && (index) < (da)->count), (index)]

#define da_last(da) \
    (da)->items[CP_ASSERT((da)->count > 0), ((da)->count - 1)]

#define da_resize(da, cnt) \
    do { \
        (da)->count = (cnt); \
        da_reserve((da), (da)->count); \
        da_shrink(da); \
    } while (0)

#define da_remove_unordered(da, index) \
    do { \
        da_get(da, (index)) = da_last(da); \
        (da)->count--; \
    } while (0)

#define da_remove_ordered(da, index) \
    do { \
        da_get(da, index) = da_last(da); \
        CP_MEMMOVE((da)->items+(index), (da)->items+(index)+1, \
                sizeof(*(da)->items)*((da)->count-index)); \
        (da)->count--; \
    } while (0)

#define da_free(da) \
    do { \
        if ((da)->items) \
            CP_FREE((da)->items); \
        (da)->count = 0; \
        (da)->capacity = 0; \
    } while (0)

#define da_remove_last(da) \
    do { \
        CP_ASSERT((da)->count > 0); \
        (da)->count--; \
    } while (0)

#define da_append_many(da, new_items, new_items_count) \
    do { \
        CP_ASSERT(new_items); \
        da_reserve((da), (da)->count + (new_items_count)); \
        CP_MEMMOVE((da)->items + (da)->count, (new_items), (new_items_count)*sizeof(*(da)->items)); \
        (da)->count += (new_items_count); \
    } while (0)

/* Hashtable templates */

#define HT_DECL(hashtable_type, key_type, value_type) \
    typedef struct hashtable_type##_node { \
        key_type key; \
        value_type val; \
        struct hashtable_type##_node *next; \
    } hashtable_type##_node; \
    typedef struct { \
        hashtable_type##_node **arr; \
        size_t count; \
        size_t capacity; \
    } hashtable_type; \
    u64 hashtable_type##_hashf(key_type key); \
    i32 hashtable_type##_compare(key_type a, key_type b); \
    void hashtable_type##_add(hashtable_type *ht, key_type key, value_type val); \
    value_type *hashtable_type##_get(hashtable_type *ht, key_type key); \
    void hashtable_type##_remove(hashtable_type *ht, key_type key); \
    void hashtable_type##_free(hashtable_type *ht);

#define HT_IMPL(hashtable_type, key_type, value_type) \
extern u64 hashtable_type##_hashf(key_type key); \
extern i32 hashtable_type##_compare(key_type a, key_type b); \
\
static void hashtable_type##_ensure_capacity(hashtable_type *ht) { \
    if (ht->capacity != 0) return; \
    ht->capacity = 128; \
    ht->arr = (hashtable_type##_node**) CP_CALLOC(ht->capacity, sizeof(hashtable_type##_node*)); \
} \
\
void hashtable_type##_add(hashtable_type *ht, key_type key, value_type val) { \
    if (ht->capacity == 0) hashtable_type##_ensure_capacity(ht); \
    size_t idx = (size_t)(hashtable_type##_hashf(key) % ht->capacity); \
    hashtable_type##_node *cur = ht->arr[idx]; \
    while (cur) { \
        if (hashtable_type##_compare(cur->key, key) == 0) { \
            cur->val = val; \
            return; \
        } \
        cur = cur->next; \
    } \
    hashtable_type##_node *n = (hashtable_type##_node*) CP_CALLOC(1, sizeof(hashtable_type##_node)); \
    n->key = key; \
    n->val = val; \
    n->next = ht->arr[idx]; \
    ht->arr[idx] = n; \
    ht->count++; \
    if (ht->count > ht->capacity * 2) { \
        size_t old_cap = ht->capacity; \
        size_t new_cap = old_cap * 3; \
        hashtable_type##_node **new_arr = (hashtable_type##_node**) CP_CALLOC(new_cap, sizeof(hashtable_type##_node*)); \
        for (size_t i = 0; i < old_cap; ++i) { \
            hashtable_type##_node *it = ht->arr[i]; \
            while (it) { \
                hashtable_type##_node *next = it->next; \
                size_t j = (size_t)(hashtable_type##_hashf(it->key) % new_cap); \
                it->next = new_arr[j]; \
                new_arr[j] = it; \
                it = next; \
            } \
        } \
        CP_FREE(ht->arr); \
        ht->arr = new_arr; \
        ht->capacity = new_cap; \
    } \
} \
\
value_type *hashtable_type##_get(hashtable_type *ht, key_type key) { \
    if (ht->capacity == 0) return NULL; \
    size_t idx = (size_t)(hashtable_type##_hashf(key) % ht->capacity); \
    hashtable_type##_node *cur = ht->arr[idx]; \
    while (cur) { \
        if (hashtable_type##_compare(cur->key, key) == 0) return &cur->val; \
        cur = cur->next; \
    } \
    return NULL; \
} \
\
void hashtable_type##_remove(hashtable_type *ht, key_type key) { \
    if (ht->capacity == 0) return; \
    size_t idx = (size_t)(hashtable_type##_hashf(key) % ht->capacity); \
    hashtable_type##_node *cur = ht->arr[idx]; \
    hashtable_type##_node *prev = NULL; \
    while (cur) { \
        if (hashtable_type##_compare(cur->key, key) == 0) { \
            if (prev) prev->next = cur->next; else ht->arr[idx] = cur->next; \
            CP_FREE(cur); \
            ht->count--; \
            return; \
        } \
        prev = cur; \
        cur = cur->next; \
    } \
} \
\
void hashtable_type##_free(hashtable_type *ht) { \
    if (ht->capacity == 0) return; \
    for (size_t i = 0; i < ht->capacity; ++i) { \
        hashtable_type##_node *cur = ht->arr[i]; \
        while (cur) { \
            hashtable_type##_node *next = cur->next; \
            CP_FREE(cur); \
            cur = next; \
        } \
    } \
    CP_FREE(ht->arr); \
    ht->arr = NULL; \
    ht->capacity = 0; \
    ht->count = 0; \
}

#define HT(hashtable_type, key_type, value_type) \
    HT_DECL(hashtable_type, key_type, value_type) \
    HT_IMPL(hashtable_type, key_type, value_type)

#define ht_foreach_node(hashtable_type, ht, nodevar) \
    for (size_t _ht_idx = 0; (ht)->capacity && _ht_idx < (ht)->capacity; ++_ht_idx) \
        for (hashtable_type##_node *nodevar = (ht)->arr[_ht_idx]; nodevar; nodevar = nodevar->next)

#define strhash(dst, str) \
    do { \
        u64 h = 14695981039346656037ULL; \
        u8 *p = (u8*)(str); \
        while (*p) { \
            h ^= (u64)(*p++); \
            h *= 1099511628211ULL; \
        } \
        *(dst) = h; \
    } while(0)

#define numhash(num) \
    ((u64)((u64)6364136223846793005ULL * (u64)(num) + 1442695040888963407ULL))

#define hash_combine(h1, h2) \
    h1 ^ (h2 + 0x9e3779b97f4a7c15ULL + (h1 << 6) + (h1 >> 2))

#define HT_DECL_STR(hashtable_type, value_type) \
    HT_DECL(hashtable_type, char*, value_type)

#define HT_IMPL_STR(hashtable_type, value_type) \
HT_IMPL(hashtable_type, char*, value_type) \
\
u64 hashtable_type##_hashf(char* s) { \
    u64 res; \
    strhash(&res, s); \
    return res; \
} \
\
i32 hashtable_type##_compare(char* a, char* b) { \
    return strcmp(a, b); \
}

#define HT_STR(hashtable_type, value_type) \
    HT_DECL_STR(hashtable_type, value_type) \
    HT_IMPL_STR(hashtable_type, value_type)

#define HT_IMPL_NUM(hashtable_type, key_type, value_type) \
HT_IMPL(hashtable_type, key_type, value_type) \
\
u64 hashtable_type##_hashf(key_type num) { \
    return numhash(num); \
} \
\
i32 hashtable_type##_compare(key_type a, key_type b) { \
    return !(a == b); \
}

/* String builder */

#include <stdarg.h>

typedef DA(char) StringBuilder;

static inline int sb_appendf(StringBuilder *sb, const char *fmt, ...) {
    va_list args;

    va_start(args, fmt);
    int n = vsnprintf(NULL, 0, fmt, args);
    va_end(args);

    da_reserve(sb, sb->count + n + 1);
    char *dest = sb->items + sb->count;
    va_start(args, fmt);
    vsnprintf(dest, n+1, fmt, args);
    va_end(args);

    sb->count += n;
    return n;
}

#define sb_append(sb, c) da_append(sb, c)
#define sb_reset(sb)     da_resize(sb, 0)
#define sb_free(sb)      da_free(sb)

#endif // _CP_H_
