#include "gen_x86_64.h"
#include "assembler.h"
#include "hashtbl.h"
#include "list.h"
#include <limits.h>
#include <stdio.h>

#define ms_register asm_rbx

static void bad_format() {
  fprintf(stderr, "ir: bad format");
  exit(1);
}

/* approximate jump offset */

static hashtbl approx_label_addr;
static hashtbl approx_jmp_addr;

static void gen_one(ir_instr *i);

static void approx_label(ir_instr *i) {
  switch (i->type) {
  case ir_instr_set:
  case ir_instr_mset:
  case ir_instr_call:
  case ir_instr_tcall:
  case ir_instr_ret:
    gen_one(i);
    break;
  case ir_instr_label:
    hashtbl_add(&approx_label_addr, i->label, (uint64_t)asm_cp);
    break;
  case ir_instr_jmp:
    hashtbl_add(&approx_jmp_addr, (uint64_t)i, (uint64_t)asm_cp);
    asm_emit_jmp(asm_mk_rand_imm(INT32_MIN));
    break;
  case ir_instr_jcc:
    hashtbl_add(&approx_jmp_addr, (uint64_t)i, (uint64_t)asm_cp);
    asm_emit_jcc(0, INT32_MIN);
    break;
  }
}

/* approximate jump offset end */

/* collect information */

static int callee_used[ir_reg_count];
static fvar_pos max_fv_pos;

static void collect_triv(ir_triv tr) {
  switch (tr.type) {
  case ir_triv_reg:
    callee_used[tr.reg] = 1;
    break;
  case ir_triv_fvar:
    if (tr.fv > max_fv_pos) {
      max_fv_pos = tr.fv;
    }
    break;
  case ir_triv_const:
  case ir_triv_uvar:
  case ir_triv_svar:
    break;
  }
}

static void collect_rhs(ir_rhs r) {
  switch (r.type) {
  case ir_rhs_triv:
    collect_triv(r.triv);
    break;
  case ir_rhs_bin:
    collect_triv(r.bin.x);
    collect_triv(r.bin.y);
    break;
  case ir_rhs_mref:
    collect_triv(r.mref.base);
    collect_triv(r.mref.offset);
    break;
  }
}

static void collect_instr(ir_instr *i) {
  switch (i->type) {
  case ir_instr_set:
    collect_triv(i->set.l);
    collect_rhs(i->set.r);
    break;
  case ir_instr_mset:
    collect_triv(i->mset.base);
    collect_triv(i->mset.offset);
    collect_rhs(i->mset.r);
    break;
  case ir_instr_label:
  case ir_instr_jmp:
  case ir_instr_jcc:
    break;
  case ir_instr_call:
  case ir_instr_tcall:
    collect_rhs(i->call.f);
    break;
  case ir_instr_ret:
    break;
  }
}

/* collect information end */

/* generate code */

static hashtbl label_addr;
static hashtbl label_pend_32;
static hashtbl label_pend_8;

static bool is_int8(int32_t x) {
  return INT8_MIN <= x && x <= INT8_MAX;
}

static asm_rand triv_to_rand(ir_triv tr) {
  switch (tr.type) {
  case ir_triv_const:
    return asm_mk_rand_imm(tr.const_);
  case ir_triv_reg:
    switch (tr.reg) {
      case ir_rax: return asm_mk_rand_reg(asm_rax);
      case ir_rdi: return asm_mk_rand_reg(asm_rdi);
      case ir_rsi: return asm_mk_rand_reg(asm_rsi);
      case ir_rdx: return asm_mk_rand_reg(asm_rdx);
      case ir_rcx: return asm_mk_rand_reg(asm_rcx);
      case ir_r8:  return asm_mk_rand_reg(asm_r8);
      case ir_r9:  return asm_mk_rand_reg(asm_r9);
      case ir_r10: return asm_mk_rand_reg(asm_r10);
      case ir_r11: return asm_mk_rand_reg(asm_r11);
      case ir_rbx: return asm_mk_rand_reg(asm_rbx);
      case ir_r13: return asm_mk_rand_reg(asm_r13);
      case ir_r14: return asm_mk_rand_reg(asm_r14);
      case ir_r15: return asm_mk_rand_reg(asm_r15);
    }
  case ir_triv_fvar:
    return asm_mk_rand_disp(asm_rbp, -tr.fv * 8);
  case ir_triv_svar:
    return asm_mk_rand_disp(ms_register, tr.sv * 8);
  case ir_triv_uvar:
    bad_format();
    return asm_mk_rand_imm(0); /* make the compiler happy */
  }
}

static asm_rand mem_to_rand(ir_triv base, ir_triv offset) {
  if (ir_triv_is_const(base)) {
    return asm_mk_rand_disp(triv_to_rand(offset).reg, base.const_);
  } else if (ir_triv_is_const(offset)) {
    return asm_mk_rand_disp(triv_to_rand(base).reg, offset.const_);
  } else {
    return asm_mk_rand_index(triv_to_rand(base).reg, triv_to_rand(offset).reg);
  }
}

static asm_op to_op(ir_bin op) {
  switch (op) {
  case ir_bin_add: return asm_add;
  case ir_bin_sub: return asm_sub;
  case ir_bin_mul: return asm_imul;
  case ir_bin_and: return asm_and;
  case ir_bin_or:  return asm_or;
  case ir_bin_xor: return asm_xor;
  case ir_bin_sar: return asm_sar;
  case ir_bin_shr: return asm_shr;
  case ir_bin_shl: return asm_shl;
  }
}

static asm_jmp_op to_jmp_op(ir_rel op) {
  switch (op) {
  case ir_rel_eq: return asm_je;
  case ir_rel_ne: return asm_jne;
  case ir_rel_gt: return asm_jg;
  case ir_rel_ge: return asm_jge;
  case ir_rel_lt: return asm_jl;
  case ir_rel_le: return asm_jle;
  }
}

static void gen_prologue() {
  if (max_fv_pos > 0) {
    asm_emit_push_reg(asm_rbp);
    asm_emit_compute(asm_mov, asm_mk_rand_reg(asm_rsp), asm_mk_rand_reg(asm_rbp));
    asm_emit_compute(asm_sub, asm_mk_rand_imm(8 * max_fv_pos), asm_mk_rand_reg(asm_rsp));
  }
  for (int r = 0; r <= ir_reg_count; r++) {
    if (ir_reg_is_callee_saved(r) && callee_used[r]) {
      asm_emit_push_reg(triv_to_rand(ir_mk_triv_reg(r)).reg);
    }
  }
}

static void gen_epilogue() {
  for (int r = ir_reg_count; r >= 0; r--) {
    if (ir_reg_is_callee_saved(r) && callee_used[r]) {
      asm_emit_pop_reg(triv_to_rand(ir_mk_triv_reg(r)).reg);
    }
  }
  if (max_fv_pos > 0) {
    asm_emit_compute(asm_mov, asm_mk_rand_reg(asm_rbp), asm_mk_rand_reg(asm_rsp));
    asm_emit_pop_reg(asm_rbp);
  }
}

static void fill_8(char *next_ip_adr, int8_t offset) {
  char *t = asm_cp;
  asm_cp = next_ip_adr - 1;
  asm_emit_i8(offset);
  asm_cp = t;
}

static void fill_32(char *next_ip_adr, int32_t offset) {
  char *t = asm_cp;
  asm_cp = next_ip_adr - 4;
  asm_emit_i32(offset);
  asm_cp = t;
}

static void fill_or_pend_8(char *next_ip_adr, label_id dest) {
  uint64_t *maybe_adr = hashtbl_find(&label_addr, dest);
  if (maybe_adr != NULL) {
    fill_8(next_ip_adr, (int64_t)dest - (int64_t)next_ip_adr);
  } else {
    list *pending = (list *)hashtbl_find_or_add(&label_pend_8, dest, (uint64_t)nil());
    *pending = cons((uint64_t)next_ip_adr, *pending);
  }
}

static void fill_or_pend_32(char *next_ip_adr, label_id dest) {
  uint64_t *maybe_adr = hashtbl_find(&label_addr, dest);
  if (maybe_adr != NULL) {
    fill_32(next_ip_adr, (int64_t)dest - (int64_t)next_ip_adr);
  } else {
    list *pending = (list *)hashtbl_find_or_add(&label_pend_32, dest, (uint64_t)nil());
    *pending = cons((uint64_t)next_ip_adr, *pending);
  }
}

static void resolve_offset() {
  struct hashtbl_blist *i;
  list j;
  for (i = label_pend_8.top; i != NULL; i = i->down) {
    int64_t adr = (int64_t)*hashtbl_find(&label_addr, i->key);
    for (j = (list)i->val; j != nil(); j = j->next) {
      char *next_ip = (char *)j->x;
      fill_8(next_ip, adr - (int64_t)next_ip);
    }
  }
  for (i = label_pend_32.top; i != NULL; i = i->down) {
    int64_t adr = (int64_t)*hashtbl_find(&label_addr, i->key);
    for (j = (list)i->val; j != nil(); j = j->next) {
      char *next_ip = (char *)j->x;
      fill_32(next_ip, adr - (int64_t)next_ip);
    }
  }
}

static void gen_one(ir_instr *i) {
  switch (i->type) {
  case ir_instr_set:
    switch (i->set.r.type) {
    case ir_rhs_triv:
      asm_emit_compute(asm_mov, triv_to_rand(i->set.r.triv), triv_to_rand(i->set.l));
      break;
    case ir_rhs_bin:
      asm_emit_compute(to_op(i->set.r.bin.op), triv_to_rand(i->set.r.bin.y), triv_to_rand(i->set.l));
      break;
    case ir_rhs_mref:
      asm_emit_compute(asm_mov,
                       mem_to_rand(i->set.r.mref.base, i->set.r.mref.offset),
                       triv_to_rand(i->set.l));
      break;
    }
    break;
  case ir_instr_mset:
    asm_emit_compute(asm_mov,
                     triv_to_rand(i->mset.r.triv),
                     mem_to_rand(i->mset.base, i->mset.offset));
    break;
  case ir_instr_label:
    hashtbl_add(&label_addr, i->label, (uint64_t)asm_cp);
    break;
  case ir_instr_jmp: {
    int64_t raw_offset =
      *hashtbl_find(&approx_label_addr, i->jmp.dest) -
      *hashtbl_find(&approx_jmp_addr, (uint64_t)i);
    if (is_int8(raw_offset - (1 + 1))) { // length of jmp i8
      asm_emit_jmp(asm_mk_rand_imm(INT8_MIN));
      fill_or_pend_8(asm_cp, i->jmp.dest);
    } else {
      asm_emit_jmp(asm_mk_rand_imm(INT32_MIN));
      fill_or_pend_32(asm_cp, i->jmp.dest);
    }
  }
    break;
  case ir_instr_jcc: {
    asm_emit_compute(asm_cmp, triv_to_rand(i->jcc.y), triv_to_rand(i->jcc.x));
    int64_t raw_offset =
      *hashtbl_find(&approx_label_addr, i->jcc.dest) -
      *hashtbl_find(&approx_jmp_addr, (uint64_t)i);
    if (is_int8(raw_offset - (1 + 1))) { // length of jcc i8
      asm_emit_jcc(to_jmp_op(i->jcc.op), INT8_MIN);
      fill_or_pend_8(asm_cp, i->jcc.dest);
    } else {
      asm_emit_jcc(to_jmp_op(i->jcc.op), INT32_MIN);
      fill_or_pend_32(asm_cp, i->jcc.dest);
    }
  }
    break;
  case ir_instr_call:
    asm_emit_call(triv_to_rand(i->call.f.triv).reg);
    break;
  case ir_instr_tcall:
    gen_epilogue();
    asm_emit_jmp(triv_to_rand(i->call.f.triv));
    break;
  case ir_instr_ret:
    gen_epilogue();
    asm_emit_ret();
    break;
  }
}

/* generate code end */

char *gen_x86_64(ir_code c, char *cp) {
  asm_cp = 0;
  asm_emit = asm_emit_fake;
  init_hashtbl(&approx_label_addr);
  init_hashtbl(&approx_jmp_addr);
  max_fv_pos = 0;
  for (int i = 0; i < ir_reg_count; i++) {
    callee_used[i] = 0;
  }

  for (ir_code i = c; i != ir_code_nil(); i = i->next) {
    collect_instr(&i->i);
    approx_label(&i->i);
  }

  asm_cp = cp;
  asm_emit = asm_emit_real;
  init_hashtbl(&label_addr);
  init_hashtbl(&label_pend_32);
  init_hashtbl(&label_pend_8);

  gen_prologue();
  for (ir_code i = c; i != ir_code_nil(); i = i->next) {
    gen_one(&i->i);
  }
  resolve_offset();

  hashtbl_clear(&approx_label_addr);
  hashtbl_clear(&approx_jmp_addr);
  hashtbl_clear(&label_addr);
  hashtbl_clear(&label_pend_32);
  hashtbl_clear(&label_pend_8);

  return asm_cp;
}
