#ifndef COLOR_H
#define COLOR_H

#include "hashtbl.h"
#include "set.h"
#include "ir.h"

bool color(set u, hashtbl *fc, hashtbl *fa, hashtbl *c, ir_code code);

#endif
