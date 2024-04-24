#ifndef SELECT_H
#define SELECT_H

#include "ir.h"
#include "set.h"

ir_code select_x86_64(ir_code c, set *unspillable);

#endif
