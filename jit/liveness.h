#ifndef LIVENESS_H
#define LIVENESS_H

#include "hashtbl.h"
#include "ir.h"

void uncover_conflict(ir_code c, hashtbl *ret);

#endif
