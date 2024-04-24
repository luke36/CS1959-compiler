#include "set.h"
#include <stdlib.h>

/* persistent. please forget memory leak */

static inline set alloc(uint64_t x, set next) {
  set s = (set)malloc(sizeof(struct set));
  s->x = x;
  s->next = next;
  return s;
}

set set_empty() {
  return NULL;
}

bool set_in(uint64_t x, set s) {
  for (; s != set_empty(); s = s->next) {
    if (x == s->x) {
      return true;
    }
  }
  return false;
}

set set_single(uint64_t x) {
  return alloc(x, set_empty());
}

set set_add(uint64_t x, set s) {
  if (!set_in(x, s)) {
    return alloc(x, s);
  } else {
    return s;
  }
}

set set_add_ex(uint64_t x, set s) {
  return alloc(x, s);
}

set set_union(set s1, set s2) {
  if (s1 == set_empty()) {
    return s2;
  } else {
    return set_add(s1->x, set_union(s1->next, s2));
  }
}

set set_union_ex(set s1, set s2) {
  if (s1 == set_empty()) {
    return s2;
  } else {
    return set_add_ex(s1->x, set_union_ex(s1->next, s2));
  }
}

set set_remove(set s, uint64_t x) {
  if (s == set_empty()) {
    return s;
  } else if (s->x == x) {
    return s->next;
  } else {
    set t = set_remove(s->next, x);
    if (t == s->next) {
      return s;
    } else {
      return set_add_ex(s->x, t);
    }
  }
}

set set_diff(set s1, set s2) {
  if (s2 == set_empty()) {
    return s1;
  } else {
    return set_remove(set_diff(s1, s2->next), s2->x);
  }
}

uint64_t set_size(set s) {
  uint64_t n = 0;
  for (; s != set_empty(); s = s->next) {
    n++;
  }
  return n;
}
