#ifndef IR_H
#define IR_H

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include "set.h"

typedef uint64_t uvar_id;
typedef uint64_t fvar_pos;
typedef uint64_t svar_pos;
typedef uint64_t label_id;

#define ir_reg_count 13

typedef enum ir_reg {
  ir_rax,
  ir_rdi,
  ir_rsi,
  ir_rdx,
  ir_rcx,
  ir_r8,
  ir_r9,
  ir_r10,
  ir_r11,
  ir_rbx,
  /* ir_rbp, */
  /* ir_r12, */  /* due to limitation in assembler */
  ir_r13,
  ir_r14,
  ir_r15
  /* ir_rsp */
} ir_reg;

typedef enum ir_triv_type {
  ir_triv_const,
  ir_triv_uvar,
  ir_triv_reg,
  ir_triv_fvar,
  ir_triv_svar
} ir_triv_type;

typedef struct ir_triv {
  enum ir_triv_type type;
  union {
    int64_t const_;
    uvar_id uv;
    ir_reg reg;
    fvar_pos fv;
    svar_pos sv;
  };
} ir_triv;

typedef enum ir_rhs_type {
  ir_rhs_triv,
  ir_rhs_bin,
  ir_rhs_mref
} ir_rhs_type;

typedef enum ir_bin {
  ir_bin_add,
  ir_bin_sub,
  ir_bin_mul,
  ir_bin_and,
  ir_bin_or,
  ir_bin_xor,
  ir_bin_sar,
  ir_bin_shr,
  ir_bin_shl
} ir_bin;

typedef struct ir_rhs {
  ir_rhs_type type;
  union {
    ir_triv triv;
    struct { ir_bin op; ir_triv x; ir_triv y; } bin;
    struct { ir_triv base; ir_triv offset; } mref;
  };
} ir_rhs;

typedef enum ir_instr_type {
  ir_instr_set,
  ir_instr_mset,
  ir_instr_label,
  ir_instr_jmp,
  ir_instr_jcc,
  ir_instr_call,
  ir_instr_tcall,
  ir_instr_ret
} ir_instr_type;

typedef enum ir_rel {
  ir_rel_eq,
  ir_rel_ne,
  ir_rel_gt,
  ir_rel_ge,
  ir_rel_lt,
  ir_rel_le
} ir_rel;

typedef struct ir_instr {
  ir_instr_type type;
  union {
    struct { ir_triv l; ir_rhs r; } set;
    struct { ir_triv base; ir_triv offset; ir_rhs r; } mset;
    label_id label;
    struct { label_id dest; } jmp;
    struct { ir_rel op; ir_triv x; ir_triv y; label_id dest; } jcc;
    struct { ir_rhs f; set live; } call;
  };
} ir_instr;

typedef struct ir_code {
  set live;
  ir_instr i;
  struct ir_code *next;
} *ir_code;

inline ir_triv ir_mk_triv_const(int64_t c) {
  struct ir_triv tr = { .type = ir_triv_const, .const_ = c };
  return tr;
}

inline ir_triv ir_mk_triv_reg(ir_reg r) {
  struct ir_triv tr = { .type = ir_triv_reg, .reg = r };
  return tr;
}

inline ir_triv ir_mk_triv_uvar(uvar_id uv) {
  struct ir_triv tr = { .type = ir_triv_uvar, .uv = uv };
  return tr;
}

inline ir_triv ir_mk_triv_fvar(fvar_pos fv) {
  struct ir_triv tr = { .type = ir_triv_fvar, .fv = fv };
  return tr;
}

inline ir_triv ir_mk_triv_svar(svar_pos sv) {
  struct ir_triv tr = { .type = ir_triv_svar, .sv = sv };
  return tr;
}

inline ir_rhs ir_mk_rhs_triv(ir_triv triv) {
  struct ir_rhs r = { .type = ir_rhs_triv, .triv = triv };
  return r;
}

inline ir_rhs ir_mk_rhs_bin(ir_bin op, ir_triv x, ir_triv y) {
  struct ir_rhs r =
    { .type = ir_rhs_bin, .bin = { .op = op, .x = x, .y = y } };
  return r;
}

inline ir_rhs ir_mk_rhs_mref(ir_triv base, ir_triv offset) {
  struct ir_rhs r =
    { .type = ir_rhs_mref, .mref = { .base = base, .offset = offset } };
  return r;
}

inline ir_instr ir_mk_instr_set(ir_triv l, ir_rhs r) {
  struct ir_instr i =
    { .type = ir_instr_set, .set = { .l = l, .r = r } };
  return i;
}

inline ir_instr ir_mk_instr_mset(ir_triv base, ir_triv offset, ir_rhs r) {
  struct ir_instr i =
    { .type = ir_instr_mset, .mset = { .base = base, .offset = offset, .r = r} };
  return i;
}

inline ir_instr ir_mk_instr_label(label_id lab) {
  struct ir_instr i =
    { .type = ir_instr_label, .label = lab };
  return i;
}

inline ir_instr ir_mk_instr_jmp(label_id dest) {
  struct ir_instr i =
    { .type = ir_instr_jmp, .jmp = { .dest = dest } };
  return i;
}

inline ir_instr ir_mk_instr_jcc(ir_rel op, ir_triv x, ir_triv y, label_id dest) {
  struct ir_instr i =
    { .type = ir_instr_jcc, .jcc = { .op = op, .x = x, .y = y, .dest = dest } };
  return i;
}

inline ir_instr ir_mk_instr_call(ir_rhs f, set live) {
  struct ir_instr i =
    { .type = ir_instr_call, .call = { .f = f, .live = live } };
  return i;
}

inline ir_instr ir_mk_instr_tcall(ir_rhs f, set live) {
  struct ir_instr i =
    { .type = ir_instr_tcall, .call = { .f = f, .live = live } };
  return i;
}

inline ir_instr ir_mk_instr_ret() {
  struct ir_instr i = { .type = ir_instr_ret };
  return i;
}

inline bool ir_triv_eq(ir_triv x, ir_triv y) {
  if (x.type != y.type) {
    return false;
  } else {
    switch (x.type) {
    case ir_triv_const:
      return x.const_ == y.const_;
    case ir_triv_uvar:
      return x.uv == y.uv;
    case ir_triv_reg:
      return x.reg == y.reg;
    case ir_triv_fvar:
      return x.fv == y.fv;
    case ir_triv_svar:
      return x.sv == y.sv;
    }
  }
}

inline bool ir_reg_is_callee_saved(ir_reg r) {
  switch (r) {
  case ir_rax:
  case ir_rdi:
  case ir_rsi:
  case ir_rdx:
  case ir_rcx:
  case ir_r8:
  case ir_r9:
  case ir_r10:
  case ir_r11:
    return false;
  case ir_rbx:
  case ir_r13:
  case ir_r14:
  case ir_r15:
    return true;
  }
}

inline bool ir_triv_is_const(ir_triv tr) {
  return tr.type == ir_triv_const;
}

inline bool ir_triv_is_reg(ir_triv tr) {
  return tr.type == ir_triv_reg;
}

inline bool ir_triv_is_uvar(ir_triv tr) {
  return tr.type == ir_triv_uvar;
}

inline bool ir_triv_is_fvar(ir_triv tr) {
  return tr.type == ir_triv_fvar;
}

inline bool ir_triv_is_svar(ir_triv tr) {
  return tr.type == ir_triv_svar;
}

inline bool ir_rhs_is_triv(ir_rhs r) {
  return r.type == ir_rhs_triv;
}

inline bool ir_rhs_is_bin(ir_rhs r) {
  return r.type == ir_rhs_bin;
}

inline bool ir_rhs_is_mref(ir_rhs r) {
  return r.type == ir_rhs_mref;
}

inline ir_code ir_code_cons(ir_instr i, ir_code next) {
  ir_code c = (ir_code)malloc(sizeof(struct ir_code));
  c->i = i;
  c->next = next;
  return c;
}

inline ir_code ir_code_nil() {
  return NULL;
}

void ir_show(ir_code c, FILE *fp);

#endif
