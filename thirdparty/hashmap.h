/*
 * hashmap.h
 *
 * - HashMap: a simple separate-chaining hash map using uint64_t keys.
 * - Public API prefix: hm_
 * - Implementations compiled only when HASHMAP_IMPLEMENTATION is defined
 *   in exactly one C file before including this header.
 *
 * Notes:
 * - Keys are uint64_t and treated as the canonical key. Collisions (equal keys)
 *   behave as equal keys. If you build keys from structs, use struct_hash().
 * - Values are stored as void*; optionally freed with the val_dtor passed to hm_create.
 * - Not thread-safe.
 *
 * Usage:
 *   #include "hashmap.h"               // to use the API
 *   // in exactly one .c
 *   #define HASHMAP_IMPLEMENTATION
 *   #include "hashmap.h"
 *
 * Public API:
 *   HashMap *hm_create(size_t initial_buckets, hm_dtor_fn val_dtor);
 *   void      hm_destroy(HashMap *hm);
 *   int       hm_put(HashMap *hm, uint64_t key, void *val);  // 1=new,0=replaced,-1=err
 *   void     *hm_get(const HashMap *hm, uint64_t key);       // NULL if not found
 *   int       hm_remove(HashMap *hm, uint64_t key);         // 1=removed,0=not found
 *   size_t    hm_count(const HashMap *hm);
 *   void      hm_foreach(const HashMap *hm, void (*cb)(uint64_t key, void *val, void *ud), void *ud);
 *   Helpers: hm_strdup, hm_strhash, struct_hash, struct_hash_to_uint64
 */

#ifndef HASHMAP_H
#define HASHMAP_H

#include <stdint.h>
#include <stddef.h>

/* --- configuration --- */
#ifndef HM_DEFAULT_BUCKETS
#define HM_DEFAULT_BUCKETS 16
#endif

/* --- types --- */
typedef void (*hm_dtor_fn)(void *ptr);

typedef struct hm_entry {
    uint64_t key;
    void *val;
    struct hm_entry *next;
} hm_entry_t;

typedef struct HashMap {
    hm_entry_t **buckets;
    size_t nbuckets;
    size_t nitems;
    hm_dtor_fn val_dtor;
} HashMap;

/* --- API declarations --- */

/* helpers */
char    *hm_strdup(const char *s);           /* portable strdup */
uint64_t hm_strhash(const char *s);          /* djb2 -> uint64_t */

/* universal memory/struct hash (Murmur-like) returning 64-bit */
uint64_t hm_struct_hash(const void *data, size_t len);
void    *hm_struct_dup(const void *src, size_t len);

/* hashmap API (uint64_t keys only) */
HashMap *hm_create(size_t initial_buckets, hm_dtor_fn val_dtor);
void     hm_destroy(HashMap *hm);
int      hm_put(HashMap *hm, uint64_t key, void *val);  /* 1=new,0=replaced,-1=err */
void    *hm_get(const HashMap *hm, uint64_t key);       /* NULL if not found */
int      hm_remove(HashMap *hm, uint64_t key);         /* 1=removed,0=not found */
size_t   hm_count(const HashMap *hm);
void     hm_foreach(const HashMap *hm, void (*cb)(uint64_t key, void *val, void *ud), void *ud);

#endif /* HASHMAP_H */


/* ---------------- Implementations ----------------
   Define HASHMAP_IMPLEMENTATION in exactly one C file before including this header.
   Example:
       #define HASHMAP_IMPLEMENTATION
       #include "hashmap.h"
*/
#ifdef HASHMAP_IMPLEMENTATION

/* implementation includes */
#include <stdlib.h>
#include <string.h>

/* ---------- helpers ---------- */

char *hm_strdup(const char *s) {
    if (!s) return NULL;
    size_t len = strlen(s) + 1;
    char *p = (char*)malloc(len);
    if (!p) return NULL;
    memcpy(p, s, len);
    return p;
}

void *hm_struct_dup(const void *src, size_t len) {
    if (!src || len == 0) return NULL;
    void *p = malloc(len);
    if (!p) return NULL;
    memcpy(p, src, len);
    return p;
}

/* djb2 string hash -> uint64_t */
uint64_t hm_strhash(const char *s) {
    const unsigned char *p = (const unsigned char*)s;
    uint64_t h = 5381ULL;
    int c;
    while ((c = *p++)) h = ((h << 5) + h) + (unsigned)c; /* h*33 + c */
    return h;
}

/* ---------- universal memory/struct hash (Murmur-like) ---------- */

/* Returns 64-bit hash. Provide seed to randomize if desired (seed=0 OK). */
uint64_t hm_struct_hash(const void *data, size_t len) {
    const uint8_t *bytes = (const uint8_t*)data;
    const uint64_t m = 0xc6a4a7935bd1e995ULL;
    const int r = 47;

    uint64_t h = 0x9E3779B97F4A7C15ULL ^ (len * m);

    size_t i = 0;
    while (len >= 8) {
        uint64_t k;
        memcpy(&k, bytes + i, sizeof(uint64_t));
        k *= m;
        k ^= k >> r;
        k *= m;

        h ^= k;
        h *= m;

        i += 8;
        len -= 8;
    }

    uint64_t tail = 0;
    switch (len) {
        case 7: tail ^= (uint64_t)bytes[i + 6] << 48; /* fallthrough */
        case 6: tail ^= (uint64_t)bytes[i + 5] << 40; /* fallthrough */
        case 5: tail ^= (uint64_t)bytes[i + 4] << 32; /* fallthrough */
        case 4: tail ^= (uint64_t)bytes[i + 3] << 24; /* fallthrough */
        case 3: tail ^= (uint64_t)bytes[i + 2] << 16; /* fallthrough */
        case 2: tail ^= (uint64_t)bytes[i + 1] << 8;  /* fallthrough */
        case 1: tail ^= (uint64_t)bytes[i + 0];
                tail *= m;
                tail ^= tail >> r;
                tail *= m;
                h ^= tail;
    }

    h ^= h >> r;
    h *= m;
    h ^= h >> r;

    return h;
}

/* ---------- internal helpers for hashmap ---------- */

static hm_entry_t *hm_entry_new(uint64_t key, void *val) {
    hm_entry_t *e = (hm_entry_t*)malloc(sizeof(hm_entry_t));
    if (!e) return NULL;
    e->key = key;
    e->val = val;
    e->next = NULL;
    return e;
}

static void hm_free_chain(hm_entry_t *e, hm_dtor_fn dtor) {
    while (e) {
        hm_entry_t *n = e->next;
        if (dtor) dtor(e->val);
        free(e);
        e = n;
    }
}

/* rehash: allocate new_buckets and move existing entries */
static int hm_rehash(HashMap *hm, size_t new_buckets) {
    if (!hm) return 0;
    hm_entry_t **newtab = (hm_entry_t**)calloc(new_buckets, sizeof(hm_entry_t*));
    if (!newtab) return 0;
    for (size_t i = 0; i < hm->nbuckets; ++i) {
        hm_entry_t *e = hm->buckets[i];
        while (e) {
            hm_entry_t *next = e->next;
            /* compute index using uint64_t arithmetic, then cast to size_t for indexing */
            size_t idx = (size_t)(e->key % (uint64_t)new_buckets);
            e->next = newtab[idx];
            newtab[idx] = e;
            e = next;
        }
    }
    free(hm->buckets);
    hm->buckets = newtab;
    hm->nbuckets = new_buckets;
    return 1;
}

static int hm_maybe_resize(HashMap *hm) {
    if (!hm) return 0;
    const double grow = 0.75, shrink = 0.15;
    if (hm->nbuckets == 0) return hm_rehash(hm, HM_DEFAULT_BUCKETS);
    double load = (double)hm->nitems / (double)hm->nbuckets;
    if (load > grow) return hm_rehash(hm, hm->nbuckets * 2);
    if (hm->nbuckets > HM_DEFAULT_BUCKETS && load < shrink) return hm_rehash(hm, hm->nbuckets / 2);
    return 1;
}

/* ---------- public hashmap API implementations ---------- */

HashMap *hm_create(size_t initial_buckets, hm_dtor_fn val_dtor) {
    if (initial_buckets < HM_DEFAULT_BUCKETS) initial_buckets = HM_DEFAULT_BUCKETS;
    HashMap *hm = (HashMap*)malloc(sizeof(HashMap));
    if (!hm) return NULL;
    hm->nbuckets = initial_buckets;
    hm->nitems = 0;
    hm->val_dtor = val_dtor;
    hm->buckets = (hm_entry_t**)calloc(hm->nbuckets, sizeof(hm_entry_t*));
    if (!hm->buckets) { free(hm); return NULL; }
    return hm;
}

void hm_destroy(HashMap *hm) {
    if (!hm) return;
    for (size_t i = 0; i < hm->nbuckets; ++i) {
        hm_free_chain(hm->buckets[i], hm->val_dtor);
    }
    free(hm->buckets);
    free(hm);
}

int hm_put(HashMap *hm, uint64_t key, void *val) {
    if (!hm) return -1;
    if (!hm_maybe_resize(hm)) return -1;
    size_t idx = (size_t)(key % (uint64_t)hm->nbuckets);
    hm_entry_t *e = hm->buckets[idx];
    while (e) {
        if (e->key == key) {
            if (hm->val_dtor) hm->val_dtor(e->val);
            e->val = val;
            return 0;
        }
        e = e->next;
    }
    hm_entry_t *ne = hm_entry_new(key, val);
    if (!ne) return -1;
    ne->next = hm->buckets[idx];
    hm->buckets[idx] = ne;
    hm->nitems++;
    return 1;
}

void *hm_get(const HashMap *hm, uint64_t key) {
    if (!hm) return NULL;
    size_t idx = (size_t)(key % (uint64_t)hm->nbuckets);
    hm_entry_t *e = hm->buckets[idx];
    while (e) {
        if (e->key == key) return e->val;
        e = e->next;
    }
    return NULL;
}

int hm_remove(HashMap *hm, uint64_t key) {
    if (!hm) return 0;
    size_t idx = (size_t)(key % (uint64_t)hm->nbuckets);
    hm_entry_t *prev = NULL;
    hm_entry_t *e = hm->buckets[idx];
    while (e) {
        if (e->key == key) {
            if (prev) prev->next = e->next;
            else hm->buckets[idx] = e->next;
            if (hm->val_dtor) hm->val_dtor(e->val);
            free(e);
            hm->nitems--;
            hm_maybe_resize(hm);
            return 1;
        }
        prev = e;
        e = e->next;
    }
    return 0;
}

size_t hm_count(const HashMap *hm) {
    return hm ? hm->nitems : 0;
}

void hm_foreach(const HashMap *hm, void (*cb)(uint64_t key, void *val, void *ud), void *ud) {
    if (!hm || !cb) return;
    for (size_t i = 0; i < hm->nbuckets; ++i) {
        hm_entry_t *e = hm->buckets[i];
        while (e) {
            cb(e->key, e->val, ud);
            e = e->next;
        }
    }
}

#endif /* HASHMAP_IMPLEMENTATION */

