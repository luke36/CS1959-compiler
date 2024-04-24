#include "hashtbl.h"
#include <stdlib.h>

static void ensure_bucks(hashtbl *h) {
  if (h->bucks != NULL)
    return;
  int i;
  h->bucks = (struct hashtbl_blist **)malloc(NBUCK * sizeof(struct hashtbl_blist *));
  for (i = 0; i < NBUCK; i++)
    h->bucks[i] = NULL;
}

void init_hashtbl(hashtbl *h) {
  h->bucks = NULL;
  h->top = NULL;
}

uint64_t *hashtbl_add(hashtbl *h, uint64_t key, uint64_t val) {
  unsigned int ind;
  struct hashtbl_blist *buc;

  ensure_bucks(h);

  ind = key % NBUCK;
  buc = malloc(sizeof(struct hashtbl_blist));
  buc->key = key;
  buc->val = val;

  buc->up = NULL;
  buc->down = h->top;
  if (h->top != NULL)
    h->top->up = buc;
  h->top = buc;

  buc->next = h->bucks[ind];
  h->bucks[ind] = buc;
  return &buc->val;
}

uint64_t *hashtbl_find(hashtbl *h, uint64_t key) {
  unsigned int ind;
  struct hashtbl_blist **i;

  if (h->bucks == NULL) {
    return NULL;
  }

  ind = key % NBUCK;
  for (i = &h->bucks[ind]; *i != NULL; i = &(*i)->next)
    if (key == (*i)->key) {
      struct hashtbl_blist *b = *i;
      /* LRU */
      *i = b->next;
      b->next = h->bucks[ind];
      h->bucks[ind] = b;

      return &b->val;
    }
  return NULL;
}

uint64_t *hashtbl_find_or_add(hashtbl *h, uint64_t key, uint64_t d) {
  uint64_t *p = hashtbl_find(h, key);
  if (p != NULL) {
    return p;
  } else {
    return hashtbl_add(h, key, d);
  }
}

void hashtbl_remove(hashtbl *h, uint64_t key) {
  unsigned int ind;
  struct hashtbl_blist **it;

  if (h->bucks == NULL)
    return;

  ind = key % NBUCK;
  for (it = &h->bucks[ind]; *it != NULL; it = &(*it)->next) {
    struct hashtbl_blist *b = *it;
    if (key == b->key) {
      if (h->top == b)
        h->top = b->down;

      if (b->up != NULL)
        b->up->down = b->down;
      if (b->down != NULL)
        b->down->up = b->up;

      *it = b->next;
      free(b);
      return;
    }
  }
}

static void hashtbl_free_blist(struct hashtbl_blist *bl) {
  if (bl != NULL) {
    hashtbl_free_blist(bl->next);
    free(bl);
  }
}

void hashtbl_clear(hashtbl *h) {
  int i;

  if (h->bucks == NULL)
    return;

  for (i = 0; i < NBUCK; i++) {
    hashtbl_free_blist(h->bucks[i]);
    h->bucks[i] = NULL;
  }

  free(h->bucks);
  h->bucks = NULL;
  h->top = NULL;
}
