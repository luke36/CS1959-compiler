#include "common.h"

uint64_t get_fresh() {
  static uint64_t fresh = 0;
  return fresh++;
}

