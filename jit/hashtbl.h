#ifndef HASHTBL_H
#define HASHTBL_H

#include <stdint.h>

#define NBUCK 211

struct hashtbl_blist {
  uint64_t key;
  uint64_t val;
  struct hashtbl_blist *next;
  /* for traversal */
  struct hashtbl_blist *down;
  struct hashtbl_blist *up;
};

typedef struct hashtbl {
  struct hashtbl_blist **bucks;
  struct hashtbl_blist *top;
} hashtbl;

void init_hashtbl(hashtbl *h);
uint64_t *hashtbl_add(hashtbl *h, uint64_t key, uint64_t val);
uint64_t *hashtbl_find(hashtbl *h, uint64_t key);
uint64_t *hashtbl_find_or_add(hashtbl *h, uint64_t key, uint64_t d);
void hashtbl_remove(hashtbl *h, uint64_t key);
void hashtbl_clear(hashtbl *h);

#endif
