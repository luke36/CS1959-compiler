#ifndef SET_H
#define SET_H

#include <stdint.h>
#include <stdbool.h>

typedef struct set {
  uint64_t x;
  struct set *next;
} *set;

set set_empty();
bool set_in(uint64_t x, set s);
set set_single(uint64_t x);
set set_add(uint64_t x, set s);
set set_add_ex(uint64_t x, set s);
set set_union(set s1, set s2);
set set_union_ex(set s1, set s2);
set set_remove(set s, uint64_t x);
set set_diff(set s1, set s2);
uint64_t set_size(set s);

#endif
