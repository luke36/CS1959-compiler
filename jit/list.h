#ifndef LIST_H
#define LIST_H

#include <stdint.h>
#include <stdlib.h>

typedef struct list {
  uint64_t x;
  struct list *next;
} *list;

inline list nil() {
  return NULL;
}

inline list cons(uint64_t x, list next) {
  list p = (list)malloc(sizeof(struct list));
  p->x = x;
  p->next = next;
  return p;
}

#endif
