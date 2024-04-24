#include "genir.h"
#include "set.h"
#include "common.h"
#include "hashtbl.h"
#include <stdio.h>

static void error_unsupported() {
  fprintf(stderr, "error: trying to jit unsupported bytecode");
  exit(1);
}

/* decide function range */

static hashtbl instr_to_label;
static instruction_t *farthest;

static void add_label(instruction_t *dest) {
  if (hashtbl_find(&instr_to_label, (uint64_t)dest) == NULL) {
    uint64_t label = get_fresh();
    hashtbl_add(&instr_to_label, (uint64_t)dest, label);
    if (dest > farthest) {
      farthest = dest;
    }
  }
}

static void gen_label(instruction_t *i) {
  switch (INSTR_op(*i)) {
  case pb_nop:
  case pb_mov_register:
  case pb_mov_immediate:
  case pb_bin_add_register:
  case pb_bin_add_immediate:
  case pb_bin_sub_register:
  case pb_bin_sub_immediate:
  case pb_bin_mul_register:
  case pb_bin_mul_immediate:
  case pb_bin_div_register:
  case pb_bin_div_immediate:
  case pb_bin_rem_register:
  case pb_bin_rem_immediate:
  case pb_bin_and_register:
  case pb_bin_and_immediate:
  case pb_bin_ior_register:
  case pb_bin_ior_immediate:
  case pb_bin_xor_register:
  case pb_bin_xor_immediate:
  case pb_bin_lsl_register:
  case pb_bin_lsl_immediate:
  case pb_bin_lsr_register:
  case pb_bin_lsr_immediate:
  case pb_bin_asr_register:
  case pb_bin_asr_immediate:
  case pb_ld_immediate:
  case pb_ld_register:
  case pb_st_immediate:
  case pb_st_register:
  case pb_cmp_eq_register:
  case pb_cmp_eq_immediate:
  case pb_cmp_lt_register:
  case pb_cmp_lt_immediate:
  case pb_cmp_gt_register:
  case pb_cmp_gt_immediate:
  case pb_cmp_le_register:
  case pb_cmp_le_immediate:
  case pb_cmp_ge_register:
  case pb_cmp_ge_immediate:
  case pb_addr:
  case pb_call_immediate:
  case pb_call_register:
    gen_label(i + 1);
    break;
  case pb_literal:
  case pb_ncall:
    gen_label(i + 3);
    break;
  case pb_jmp_true:
  case pb_jmp_false:
    add_label((instruction_t *)((char *)(i + 1) + INSTR_i_imm(*i)));
    gen_label(i + 1);
    break;
  case pb_jmp_always:
    add_label((instruction_t *)((char *)(i + 1) + INSTR_i_imm(*i)));
  case pb_tcall_immediate:
  case pb_tcall_register:
  case pb_return:
    if (i + 1 <= farthest) {
      gen_label(i + 1);
    }
    break;
  case pb_ntcall:
    if (i + 3 <= farthest) {
      gen_label(i + 3);
    }
    break;
  case pb_jit:
    error_unsupported();
  }
}

/* decide function range end */

/* generate code */

#define ms_register ir_rbx /* store machine_state * */
#define Carg1       ir_rdi
#define Carg2       ir_rsi

#define LOAD_UNALIGNED_PTR(addr)                                        \
  ((((uptr)((unsigned *)(addr))[1]) << 32) | ((unsigned *)(addr))[0])

static ir_triv reg_map[pb_reg_count];

static ir_triv gentriv(uint32_t r) {
  switch (r) {
  case pb_allocation_pointer:
    return ir_mk_triv_svar(pb_allocation_pointer);
  case pb_frame_pointer:
    return ir_mk_triv_svar(pb_frame_pointer);
  default:
    if (reg_map[r].type != ir_triv_uvar) {
      reg_map[r] = ir_mk_triv_uvar(get_fresh());
    }
    return reg_map[r];
  }
}

static ir_code gen_drr_bin(ir_bin op, instruction_t instr, ir_code next) {
  return ir_code_cons(ir_mk_instr_set(gentriv(INSTR_drr_dest(instr)),
                                      ir_mk_rhs_bin(op,
                                                    gentriv(INSTR_drr_reg1(instr)),
                                                    gentriv(INSTR_drr_reg2(instr)))), next);
}

static ir_code gen_dri_bin(ir_bin op, instruction_t instr, ir_code next) {
  return ir_code_cons(ir_mk_instr_set(gentriv(INSTR_dri_dest(instr)),
                                      ir_mk_rhs_bin(op,
                                                    gentriv(INSTR_dri_reg(instr)),
                                                    ir_mk_triv_const(INSTR_dri_imm(instr)))), next);
}

static ir_rel neg_rel(ir_rel op) {
  switch (op) {
  case ir_rel_eq: return ir_rel_ne;
  case ir_rel_ne: return ir_rel_eq;
  case ir_rel_gt: return ir_rel_le;
  case ir_rel_ge: return ir_rel_lt;
  case ir_rel_lt: return ir_rel_ge;
  case ir_rel_le: return ir_rel_gt;
  }
}

static label_id get_label(instruction_t *jmp) {
  return *hashtbl_find(&instr_to_label,
                       (uint64_t)((char *)(jmp + 1) + INSTR_i_imm(*jmp)));
}

static ir_code gen_dr_jcc(ir_rel op, instruction_t cmp, instruction_t *jmp, ir_code next) {
  switch (INSTR_op(*jmp)) {
  case pb_jmp_true:
    break;
  case pb_jmp_false:
    op = neg_rel(op);
    break;
  default:
    error_unsupported();
  }
  return
    ir_code_cons(ir_mk_instr_jcc
                 (op, gentriv(INSTR_dr_dest(cmp)), gentriv(INSTR_dr_reg(cmp)),
                  get_label(jmp)),
                 next);
}

static ir_code gen_di_jcc(ir_rel op, instruction_t cmp, instruction_t *jmp, ir_code next) {
  switch (INSTR_op(*jmp)) {
  case pb_jmp_true:
    break;
  case pb_jmp_false:
    op = neg_rel(op);
    break;
  default:
    error_unsupported();
  }
  return
    ir_code_cons(ir_mk_instr_jcc
                 (op, gentriv(INSTR_di_dest(cmp)), ir_mk_triv_const(INSTR_di_imm(cmp)),
                  get_label(jmp)),
                 next);
}

static ir_code genins(instruction_t *c) {
  instruction_t i = *c;

  ir_code maybe_label = NULL;
  label_id *label = hashtbl_find(&instr_to_label, (uint64_t)c);
  if (label != NULL) {
    maybe_label = ir_code_cons(ir_mk_instr_label(*label), ir_code_nil());
  }

  ir_code ret;

  switch (INSTR_op(i)) {
  case pb_nop:
    ret = genins(c + 1);
  case pb_mov_register:
    ret = ir_code_cons(ir_mk_instr_set(gentriv(INSTR_dr_dest(i)),
                                       ir_mk_rhs_triv(gentriv(INSTR_dr_reg(i)))),
                       genins(c + 1));
    break;
  case pb_mov_immediate:
    ret = ir_code_cons(ir_mk_instr_set(gentriv(INSTR_di_dest(i)),
                                       ir_mk_rhs_triv(ir_mk_triv_const(INSTR_di_imm(i)))),
                       genins(c + 1));
    break;
  case pb_literal:
    ret = ir_code_cons(ir_mk_instr_set(gentriv(INSTR_di_dest(i)),
                                       ir_mk_rhs_triv
                                       (ir_mk_triv_const
                                        (LOAD_UNALIGNED_PTR(c + 1)))),
                       genins(c + 3));
    break;
  case pb_bin_add_register:  ret = gen_drr_bin(ir_bin_add, i, genins(c + 1)); break;
  case pb_bin_add_immediate: ret = gen_dri_bin(ir_bin_add, i, genins(c + 1)); break;
  case pb_bin_sub_register:  ret = gen_drr_bin(ir_bin_sub, i, genins(c + 1)); break;
  case pb_bin_sub_immediate: ret = gen_dri_bin(ir_bin_sub, i, genins(c + 1)); break;
  case pb_bin_mul_register:  ret = gen_drr_bin(ir_bin_mul, i, genins(c + 1)); break;
  case pb_bin_mul_immediate: ret = gen_dri_bin(ir_bin_mul, i, genins(c + 1)); break;
  case pb_bin_and_register:  ret = gen_drr_bin(ir_bin_and, i, genins(c + 1)); break;
  case pb_bin_and_immediate: ret = gen_dri_bin(ir_bin_and, i, genins(c + 1)); break;
  case pb_bin_ior_register:  ret = gen_drr_bin(ir_bin_or, i, genins(c + 1)); break;
  case pb_bin_ior_immediate: ret = gen_dri_bin(ir_bin_or, i, genins(c + 1)); break;
  case pb_bin_xor_register:  ret = gen_drr_bin(ir_bin_xor, i, genins(c + 1)); break;
  case pb_bin_xor_immediate: ret = gen_dri_bin(ir_bin_xor, i, genins(c + 1)); break;
  case pb_bin_lsl_register:  ret = gen_drr_bin(ir_bin_shl, i, genins(c + 1)); break;
  case pb_bin_lsl_immediate: ret = gen_dri_bin(ir_bin_shl, i, genins(c + 1)); break;
  case pb_bin_lsr_register:  ret = gen_drr_bin(ir_bin_shr, i, genins(c + 1)); break;
  case pb_bin_lsr_immediate: ret = gen_dri_bin(ir_bin_shr, i, genins(c + 1)); break;
  case pb_bin_asr_register:  ret = gen_drr_bin(ir_bin_sar, i, genins(c + 1)); break;
  case pb_bin_asr_immediate: ret = gen_dri_bin(ir_bin_sar, i, genins(c + 1)); break;
  case pb_bin_div_register:
  case pb_bin_div_immediate:
  case pb_bin_rem_register:
  case pb_bin_rem_immediate:
    error_unsupported();
  case pb_ld_immediate:
    ret = ir_code_cons(ir_mk_instr_set(gentriv(INSTR_dri_dest(i)),
                                       ir_mk_rhs_mref
                                       (gentriv(INSTR_dri_reg(i)),
                                        ir_mk_triv_const(INSTR_dri_imm(i)))),
                       genins(c + 1));
    break;
  case pb_ld_register:
    ret = ir_code_cons(ir_mk_instr_set(gentriv(INSTR_drr_dest(i)),
                                       ir_mk_rhs_mref
                                       (gentriv(INSTR_drr_reg1(i)),
                                        gentriv(INSTR_drr_reg2(i)))),
                       genins(c + 1));
    break;
  case pb_st_immediate:
    ret = ir_code_cons(ir_mk_instr_mset(gentriv(INSTR_dri_reg(i)),
                                        ir_mk_triv_const(INSTR_dri_imm(i)),
                                        ir_mk_rhs_triv(gentriv(INSTR_dri_dest(i)))),
                       genins(c + 1));
    break;
  case pb_st_register:
    ret = ir_code_cons(ir_mk_instr_mset(gentriv(INSTR_drr_reg1(i)),
                                        gentriv(INSTR_drr_reg2(i)),
                                        ir_mk_rhs_triv(gentriv(INSTR_drr_dest(i)))),
                       genins(c + 1));
    break;
  case pb_cmp_eq_register:  ret = gen_dr_jcc(ir_rel_eq, i, c + 1, genins(c + 2)); break;
  case pb_cmp_eq_immediate: ret = gen_di_jcc(ir_rel_eq, i, c + 1, genins(c + 2)); break;
  case pb_cmp_lt_register:  ret = gen_dr_jcc(ir_rel_lt, i, c + 1, genins(c + 2)); break;
  case pb_cmp_lt_immediate: ret = gen_di_jcc(ir_rel_lt, i, c + 1, genins(c + 2)); break;
  case pb_cmp_gt_register:  ret = gen_dr_jcc(ir_rel_gt, i, c + 1, genins(c + 2)); break;
  case pb_cmp_gt_immediate: ret = gen_di_jcc(ir_rel_gt, i, c + 1, genins(c + 2)); break;
  case pb_cmp_le_register:  ret = gen_dr_jcc(ir_rel_le, i, c + 1, genins(c + 2)); break;
  case pb_cmp_le_immediate: ret = gen_di_jcc(ir_rel_le, i, c + 1, genins(c + 2)); break;
  case pb_cmp_ge_register:  ret = gen_dr_jcc(ir_rel_ge, i, c + 1, genins(c + 2)); break;
  case pb_cmp_ge_immediate: ret = gen_di_jcc(ir_rel_ge, i, c + 1, genins(c + 2)); break;
  case pb_jmp_true:
  case pb_jmp_false:
    error_unsupported();
  case pb_jmp_always:
    ret =
      ir_code_cons(ir_mk_instr_jmp(get_label(c)),
                   c + 1 <= farthest ? genins(c + 1) : ir_code_nil());
    break;
  case pb_addr:
    ret = ir_code_cons(ir_mk_instr_set(gentriv(INSTR_adr_dest(i)),
                                       ir_mk_rhs_triv
                                       (ir_mk_triv_const
                                        ((uint64_t)(c + 1) + (INSTR_adr_imm(i) << 2)))),
                       genins(c + 1));
    break;
  case pb_call_immediate:
    ret =
      ir_code_cons
      (ir_mk_instr_set(ir_mk_triv_reg(Carg1),
                       ir_mk_rhs_triv(ir_mk_triv_reg(ms_register))),
       ir_code_cons
       (ir_mk_instr_set(ir_mk_triv_reg(Carg2),
                        ir_mk_rhs_triv
                        (ir_mk_triv_const
                         ((uint64_t)((char *)(c + 1) + INSTR_i_imm(i))))),
        ir_code_cons
        (ir_mk_instr_call(ir_mk_rhs_triv(ir_mk_triv_const((uint64_t)&pb_interp)),
                          set_add_ex(Carg1, set_add_ex(Carg2, set_empty()))),
         ir_code_cons
         (ir_mk_instr_set(gentriv(pb_return_value),
                          ir_mk_rhs_triv(ir_mk_triv_svar(pb_return_value))),
          genins(c + 1)))));
    break;
  case pb_call_register:
    ret =
      ir_code_cons
      (ir_mk_instr_set(ir_mk_triv_reg(Carg1),
                       ir_mk_rhs_triv(ir_mk_triv_reg(ms_register))),
       ir_code_cons
       (ir_mk_instr_set(ir_mk_triv_reg(Carg2),
                        ir_mk_rhs_triv(gentriv(INSTR_dr_reg(i)))),
        ir_code_cons
        (ir_mk_instr_call(ir_mk_rhs_triv(ir_mk_triv_const((uint64_t)&pb_interp)),
                          set_add_ex(Carg1, set_add_ex(Carg2, set_empty()))),
         ir_code_cons
         (ir_mk_instr_set(gentriv(pb_return_value),
                          ir_mk_rhs_triv(ir_mk_triv_svar(pb_return_value))),
          genins(c + 1)))));
    break;
  case pb_tcall_immediate:
    ret =
      ir_code_cons
      (ir_mk_instr_set(ir_mk_triv_reg(Carg1),
                       ir_mk_rhs_triv(ir_mk_triv_reg(ms_register))),
       ir_code_cons
       (ir_mk_instr_set(ir_mk_triv_reg(Carg2),
                        ir_mk_rhs_triv
                        (ir_mk_triv_const
                         ((uint64_t)((char *)(c + 1) + INSTR_i_imm(i))))),
        ir_code_cons
        (ir_mk_instr_tcall(ir_mk_rhs_triv(ir_mk_triv_const((uint64_t)&pb_interp)),
                           set_add_ex(Carg1, set_add_ex(Carg2, set_empty()))),
         c + 1 <= farthest ? genins(c + 1) : ir_code_nil())));
    break;
  case pb_tcall_register:
    ret =
      ir_code_cons
      (ir_mk_instr_set(ir_mk_triv_reg(Carg1),
                       ir_mk_rhs_triv(ir_mk_triv_reg(ms_register))),
       ir_code_cons
       (ir_mk_instr_set(ir_mk_triv_reg(Carg2),
                        ir_mk_rhs_triv(gentriv(INSTR_dr_reg(i)))),
        ir_code_cons
        (ir_mk_instr_tcall(ir_mk_rhs_triv(ir_mk_triv_const((uint64_t)&pb_interp)),
                           set_add_ex(Carg1, set_add_ex(Carg2, set_empty()))),
         c + 1 <= farthest ? genins(c + 1) : ir_code_nil())));
    break;
  case pb_return:
    ret = ir_code_cons
      (ir_mk_instr_set(ir_mk_triv_svar(pb_return_value),
                       ir_mk_rhs_triv(gentriv(pb_return_value))),
       ir_code_cons
       (ir_mk_instr_ret(),
        c + 1 <= farthest ? genins(c + 1) : ir_code_nil()));
    break;
  case pb_jit:
    error_unsupported();
  case pb_ncall:
    ret =
      ir_code_cons
      (ir_mk_instr_set(ir_mk_triv_reg(Carg1),
                       ir_mk_rhs_triv(ir_mk_triv_reg(ms_register))),
       ir_code_cons
       (ir_mk_instr_call(ir_mk_rhs_triv
                         (ir_mk_triv_const
                          (LOAD_UNALIGNED_PTR(c + 1))),
                         set_single(Carg1)),
        ir_code_cons
        (ir_mk_instr_set(gentriv(pb_return_value),
                         ir_mk_rhs_triv(ir_mk_triv_svar(pb_return_value))),
         genins(c + 3))));
    break;
  case pb_ntcall:
    ret =
      ir_code_cons
      (ir_mk_instr_set(ir_mk_triv_reg(Carg1),
                       ir_mk_rhs_triv(ir_mk_triv_reg(ms_register))),
       ir_code_cons(ir_mk_instr_tcall(ir_mk_rhs_triv
                                      (ir_mk_triv_const
                                       (LOAD_UNALIGNED_PTR(c + 1))),
                                      set_single(Carg1)),
                    c + 3 <= farthest ? genins(c + 3) : ir_code_nil()));
    break;
  }

  if (maybe_label != NULL) {
    maybe_label->next = ret;
    return maybe_label;
  } else {
    return ret;
  }
}

/* generate code end */

ir_code gen_ir(instruction_t *c) {
  init_hashtbl(&instr_to_label);
  farthest = NULL;
  gen_label(c);

  for (int i = 0; i < pb_reg_count; i++) {
    reg_map[i] = ir_mk_triv_const(0);
  }
  ir_code ret = genins(c);
  ret = ir_code_cons(ir_mk_instr_set(ir_mk_triv_reg(ms_register),
                                     ir_mk_rhs_triv(ir_mk_triv_reg(Carg1))),
                     ret);
 
  hashtbl_clear(&instr_to_label);
  return ret;
}
