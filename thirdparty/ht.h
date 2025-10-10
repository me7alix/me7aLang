#ifndef HT_H
#define HT_H

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>

/*
 * Usage:
 * 1) In a header: HT_DECL(mytable, key_type, val_type)
 * 2) In one .c:   implement functions:
 *      uint64_t mytable_hashf(key_type k);
 *      int      mytable_compare(key_type a, key_type b); // return 0 when equal
 *    then put: HT_IMPL(mytable, key_type, val_type)
 *
 * Notes:
 * - get returns NULL if key not found, otherwise pointer to stored value (valt*).
 * - Freeing keys (if they are heap pointers) is the user's responsibility;
 *   you may extend this template with a key-dtor if you want automatic cleanup.
 */

#define HT_NODEDEF(htype, keyt, valt) \
typedef struct htype##_node { \
    keyt key; \
    valt val; \
    struct htype##_node *next; \
} htype##_node;

#define HT_STRUCTDEF(htype) \
typedef struct { \
    htype##_node **arr; \
    size_t count; \
    size_t capacity; \
} htype;

/* Declare types and prototypes */
#define HT_DECL(htype, keyt, valt) \
    HT_NODEDEF(htype, keyt, valt) \
    HT_STRUCTDEF(htype) \
    /* user must provide these two functions (link-time): */ \
    uint64_t htype##_hashf(keyt key); \
    int htype##_compare(keyt a, keyt b); \
    /* basic API */ \
    void htype##_add(htype *ht, keyt key, valt val); \
    valt *htype##_get(htype *ht, keyt key); \
    void htype##_remove(htype *ht, keyt key); \
    void htype##_free(htype *ht);

/* Implementations - include in ONE .c file per table type */
#define HT_IMPL(htype, keyt, valt) \
/* forward declare user functions (linker will resolve) */ \
extern uint64_t htype##_hashf(keyt key); \
extern int htype##_compare(keyt a, keyt b); \
\
static void htype##_ensure_capacity(htype *ht) { \
    if (ht->capacity != 0) return; \
    ht->capacity = 128; \
    ht->arr = calloc(ht->capacity, sizeof(htype##_node*)); \
} \
\
void htype##_add(htype *ht, keyt key, valt val) { \
    if (ht->capacity == 0) htype##_ensure_capacity(ht); \
    size_t idx = (size_t)(htype##_hashf(key) % ht->capacity); \
    /* search for existing key */ \
    htype##_node *cur = ht->arr[idx]; \
    while (cur) { \
        if (htype##_compare(cur->key, key) == 0) { \
            cur->val = val; \
            return; \
        } \
        cur = cur->next; \
    } \
    /* insert at head */ \
    htype##_node *n = malloc(sizeof(htype##_node)); \
    n->key = key; \
    n->val = val; \
    n->next = ht->arr[idx]; \
    ht->arr[idx] = n; \
    ht->count++; \
    /* grow if too dense */ \
    if (ht->count > ht->capacity * 2) { \
        /* rehash to larger table */ \
        size_t old_cap = ht->capacity; \
        size_t new_cap = old_cap * 3; \
        htype##_node **new_arr = calloc(new_cap, sizeof(htype##_node*)); \
        for (size_t i = 0; i < old_cap; ++i) { \
            htype##_node *it = ht->arr[i]; \
            while (it) { \
                htype##_node *next = it->next; \
                size_t j = (size_t)(htype##_hashf(it->key) % new_cap); \
                it->next = new_arr[j]; \
                new_arr[j] = it; \
                it = next; \
            } \
        } \
        free(ht->arr); \
        ht->arr = new_arr; \
        ht->capacity = new_cap; \
    } \
} \
\
valt *htype##_get(htype *ht, keyt key) { \
    if (ht->capacity == 0) return NULL; \
    size_t idx = (size_t)(htype##_hashf(key) % ht->capacity); \
    htype##_node *cur = ht->arr[idx]; \
    while (cur) { \
        if (htype##_compare(cur->key, key) == 0) return &cur->val; \
        cur = cur->next; \
    } \
    return NULL; \
} \
\
void htype##_remove(htype *ht, keyt key) { \
    if (ht->capacity == 0) return; \
    size_t idx = (size_t)(htype##_hashf(key) % ht->capacity); \
    htype##_node *cur = ht->arr[idx]; \
    htype##_node *prev = NULL; \
    while (cur) { \
        if (htype##_compare(cur->key, key) == 0) { \
            if (prev) prev->next = cur->next; else ht->arr[idx] = cur->next; \
            free(cur); \
            ht->count--; \
            return; \
        } \
        prev = cur; \
        cur = cur->next; \
    } \
} \
\
void htype##_free(htype *ht) { \
    if (ht->capacity == 0) return; \
    for (size_t i = 0; i < ht->capacity; ++i) { \
        htype##_node *cur = ht->arr[i]; \
        while (cur) { \
            htype##_node *next = cur->next; \
            free(cur); \
            cur = next; \
        } \
    } \
    free(ht->arr); \
    ht->arr = NULL; \
    ht->capacity = 0; \
    ht->count = 0; \
}

#define HT(htype, keyt, valt) \
    HT_DECL(htype, keyt, valt) \
    HT_IMPL(htype, keyt, valt)

#define HT_FOREACH_NODE(htype, ht, nodevar) \
    for (size_t _ht_idx = 0; (ht)->capacity && _ht_idx < (ht)->capacity; ++_ht_idx) \
        for (htype##_node *nodevar = (ht)->arr[_ht_idx]; nodevar; nodevar = nodevar->next)

#define strhash(dst, str) \
    do { \
        uint64_t h = 14695981039346656037ULL; \
        unsigned char *p = (unsigned char*)(str); \
        while (*p) { \
            h ^= (uint64_t)(*p++); \
            h *= 1099511628211ULL; \
        } \
        *(dst) = h; \
    } while(0)

#define numhash(num) \
    ((uint64_t)((uint64_t)6364136223846793005ULL * (uint64_t)(num) + 1442695040888963407ULL))

#define hash_combine(h1, h2) \
	h1 ^ (h2 + 0x9e3779b97f4a7c15ULL + (h1 << 6) + (h1 >> 2))

#define HT_DECL_STR(htype, valt) \
	HT_DECL(htype, char*, valt)

#define HT_IMPL_STR(htype, valt) \
HT_IMPL(htype, char*, valt) \
\
uint64_t htype##_hashf(char* s) { \
    uint64_t res; \
    strhash(&res, s); \
    return res; \
} \
\
int htype##_compare(char* a, char* b) { \
    return strcmp(a, b); \
}

#define HT_STR(htype, valt) \
    HT_DECL_STR(htype, valt) \
    HT_IMPL_STR(htype, valt)

#define HT_IMPL_NUM(htype, keyt, valt) \
HT_IMPL(htype, keyt, valt) \
\
uint64_t htype##_hashf(keyt num) { \
    return numhash(num); \
} \
\
int htype##_compare(keyt a, keyt b) { \
    return !(a == b); \
}

#endif
