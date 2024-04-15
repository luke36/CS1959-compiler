#include <stdint.h>

typedef int64_t  iptr;
typedef uint64_t uptr;
typedef uint32_t instruction_t;

#define INSTR_op(instr)       ((instr) & 0xFF)

#define INSTR_d_dest(instr)   (((instr) >> 8) & 0xF)

#define INSTR_dr_dest(instr)  INSTR_d_dest(instr)
#define INSTR_dr_reg(instr)   (((instr) >> 16) & 0xF)

#define INSTR_di_dest(instr)  INSTR_d_dest(instr)
#define INSTR_di_imm(instr)   (((int32_t)(instr)) >> 16)

#define INSTR_adr_dest(instr) INSTR_di_dest(instr)
#define INSTR_adr_imm(instr)  (((int32_t)(instr)) >> 12)

#define INSTR_drr_dest(instr) INSTR_d_dest(instr)
#define INSTR_drr_reg1(instr) (((instr) >> 12) & 0xF)
#define INSTR_drr_reg2(instr) (((instr) >> 16) & 0xF)

#define INSTR_dri_dest(instr) INSTR_d_dest(instr)
#define INSTR_dri_reg(instr)  (((instr) >> 12) & 0xF)
#define INSTR_dri_imm(instr)  (((int32_t)(instr)) >> 16)

#define INSTR_i_imm(instr)    (((int32_t)(instr)) >> 8)

enum {
  pb_nop,

  pb_mov_register,
  pb_mov_immediate,
  pb_literal,

  pb_bin_add_register,
  pb_bin_add_immediate,
  pb_bin_sub_register,
  pb_bin_sub_immediate,
  pb_bin_mul_register,
  pb_bin_mul_immediate,
  pb_bin_div_register,
  pb_bin_div_immediate,
  pb_bin_rem_register,
  pb_bin_rem_immediate,
  pb_bin_and_register,
  pb_bin_and_immediate,
  pb_bin_ior_register,
  pb_bin_ior_immediate,
  pb_bin_xor_register,
  pb_bin_xor_immediate,
  pb_bin_lsl_register,
  pb_bin_lsl_immediate,
  pb_bin_lsr_register,
  pb_bin_lsr_immediate,
  pb_bin_asr_register,
  pb_bin_asr_immediate,

  pb_un_not_register,
  pb_un_not_immediate,

  pb_ld_immediate,
  pb_ld_register,
  pb_st_immediate,
  pb_st_register,

  pb_cmp_eq_register,
  pb_cmp_eq_immediate,
  pb_cmp_lt_register,
  pb_cmp_lt_immediate,
  pb_cmp_gt_register,
  pb_cmp_gt_immediate,
  pb_cmp_le_register,
  pb_cmp_le_immediate,
  pb_cmp_ge_register,
  pb_cmp_ge_immediate,

  pb_jmp_true,
  pb_jmp_false,
  pb_jmp_always,

  pb_addr,
  pb_call_immediate,
  pb_call_register,
  pb_tcall_immediate,
  pb_tcall_register,
  pb_return,

  pb_jit,
  pb_ncall,
};

#define pb_mk_dr(op, dest, reg)                         \
  ((instruction_t)((op) | ((dest) << 8) | ((reg) << 16)))
#define pb_mk_di(op, dest, imm)                         \
  ((instruction_t)((op) | ((dest) << 8) | ((uint32_t)(imm) << 16)))
#define pb_mk_drr(op, dest, reg1, reg2)                                 \
  ((instruction_t)((op) | ((dest) << 8) | ((reg1) << 12) | ((reg2) << 16)))
#define pb_mk_dri(op, dest, reg, imm)                                   \
  ((instruction_t)((op) | ((dest) << 8) | ((reg) << 12) | ((uint32_t)(imm) << 16)))
#define pb_mk_i(op, imm)                        \
  ((instruction_t)((op) | ((uint32_t)(imm) << 8)))
#define pb_mk_adr(op, dest, imm)                        \
  ((instruction_t)((op) | ((dest) << 8) | ((uint32_t)(imm) << 12)))

#define pb_mk_nop()                              ((instruction_t)pb_nop)
#define pb_mk_mov_register(dest, reg)            pb_mk_dr(pb_mov_register, dest, reg)
#define pb_mk_mov_immediate(dest, imm)           pb_mk_di(pb_mov_immediate, dest, imm)
#define pb_mk_literal(dest, lit)                  \
  ((instruction_t)pb_mk_di(pb_literal, dest, 0)), \
  ((instruction_t)((uint64_t)lit)),               \
  ((instruction_t)((uint64_t)(lit) >> 32))
#define pb_mk_bin_add_register(dest, reg1, reg2) pb_mk_drr(pb_bin_add_register, dest, reg1, reg2)
#define pb_mk_bin_add_immediate(dest, reg, imm)  pb_mk_dri(pb_bin_add_immediate, dest, reg, imm)
#define pb_mk_bin_sub_register(dest, reg1, reg2) pb_mk_drr(pb_bin_sub_register, dest, reg1, reg2)
#define pb_mk_bin_sub_immediate(dest, reg, imm)  pb_mk_dri(pb_bin_sub_immediate, dest, reg, imm)
#define pb_mk_bin_mul_register(dest, reg1, reg2) pb_mk_drr(pb_bin_mul_register, dest, reg1, reg2)
#define pb_mk_bin_mul_immediate(dest, reg, imm)  pb_mk_dri(pb_bin_mul_immediate, dest, reg, imm)
#define pb_mk_bin_div_register(dest, reg1, reg2) pb_mk_drr(pb_bin_div_register, dest, reg1, reg2)
#define pb_mk_bin_div_immediate(dest, reg, imm)  pb_mk_dri(pb_bin_div_immediate, dest, reg, imm)
#define pb_mk_bin_rem_register(dest, reg1, reg2) pb_mk_drr(pb_bin_rem_register, dest, reg1, reg2)
#define pb_mk_bin_rem_immediate(dest, reg, imm)  pb_mk_dri(pb_bin_rem_immediate, dest, reg, imm)
#define pb_mk_bin_and_register(dest, reg1, reg2) pb_mk_drr(pb_bin_and_register, dest, reg1, reg2)
#define pb_mk_bin_and_immediate(dest, reg, imm)  pb_mk_dri(pb_bin_and_immediate, dest, reg, imm)
#define pb_mk_bin_ior_register(dest, reg1, reg2) pb_mk_drr(pb_bin_ior_register, dest, reg1, reg2)
#define pb_mk_bin_ior_immediate(dest, reg, imm)  pb_mk_dri(pb_bin_ior_immediate, dest, reg, imm)
#define pb_mk_bin_xor_register(dest, reg1, reg2) pb_mk_drr(pb_bin_xor_register, dest, reg1, reg2)
#define pb_mk_bin_xor_immediate(dest, reg, imm)  pb_mk_dri(pb_bin_xor_immediate, dest, reg, imm)
#define pb_mk_bin_lsl_register(dest, reg1, reg2) pb_mk_drr(pb_bin_lsl_register, dest, reg1, reg2)
#define pb_mk_bin_lsl_immediate(dest, reg, imm)  pb_mk_dri(pb_bin_lsl_immediate, dest, reg, imm)
#define pb_mk_bin_lsr_register(dest, reg1, reg2) pb_mk_drr(pb_bin_lsr_register, dest, reg1, reg2)
#define pb_mk_bin_lsr_immediate(dest, reg, imm)  pb_mk_dri(pb_bin_lsr_immediate, dest, reg, imm)
#define pb_mk_bin_asr_register(dest, reg1, reg2) pb_mk_drr(pb_bin_asr_register, dest, reg1, reg2)
#define pb_mk_bin_asr_immediate(dest, reg, imm)  pb_mk_dri(pb_bin_asr_immediate, dest, reg, imm)
#define pb_mk_un_not_register(dest, reg)         pb_mk_dr(pb_un_not_register, dest, reg)
#define pb_mk_un_not_immediate(dest, imm)        pb_mk_di(pb_un_not_immediate, dest, imm)
#define pb_mk_ld_immediate(dest, reg, imm)       pb_mk_dri(pb_ld_immediate, dest, reg, imm)
#define pb_mk_ld_register(dest, reg1, reg2)      pb_mk_drr(pb_ld_register, dest, reg1, reg2)
#define pb_mk_st_immediate(dest, reg, imm)       pb_mk_dri(pb_st_immediate, dest, reg, imm) 
#define pb_mk_st_register(dest, reg1, reg2)      pb_mk_drr(pb_st_register, dest, reg1, reg2)
#define pb_mk_cmp_eq_register(dest, reg)         pb_mk_dr(pb_cmp_eq_register, dest, reg)
#define pb_mk_cmp_eq_immediate(dest, imm)        pb_mk_di(pb_cmp_eq_immediate, dest, imm)
#define pb_mk_cmp_lt_register(dest, reg)         pb_mk_dr(pb_cmp_lt_register, dest, reg)
#define pb_mk_cmp_lt_immediate(dest, imm)        pb_mk_di(pb_cmp_lt_immediate, dest, imm)
#define pb_mk_cmp_gt_register(dest, reg)         pb_mk_dr(pb_cmp_gt_register, dest, reg)
#define pb_mk_cmp_gt_immediate(dest, imm)        pb_mk_di(pb_cmp_gt_immediate, dest, imm)
#define pb_mk_cmp_le_register(dest, reg)         pb_mk_dr(pb_cmp_le_register, dest, reg)
#define pb_mk_cmp_le_immediate(dest, imm)        pb_mk_di(pb_cmp_le_immediate, dest, imm)
#define pb_mk_cmp_ge_register(dest, reg)         pb_mk_dr(pb_cmp_ge_register, dest, reg)
#define pb_mk_cmp_ge_immediate(dest, imm)        pb_mk_di(pb_cmp_ge_immediate, dest, imm)
#define pb_mk_jmp_true(imm)                      pb_mk_i(pb_jmp_true, imm)
#define pb_mk_jmp_false(imm)                     pb_mk_i(pb_jmp_false, imm)
#define pb_mk_jmp_always(imm)                    pb_mk_i(pb_jmp_always, imm)
#define pb_mk_addr(dest, imm)                    pb_mk_adr(pb_addr, dest, imm)
#define pb_mk_call_immediate(imm)                pb_mk_i(pb_call_immediate, imm)
#define pb_mk_call_register(reg)                 pb_mk_dr(pb_call_register, 0, reg)
#define pb_mk_tcall_immediate(imm)               pb_mk_i(pb_tcall_immediate, imm)
#define pb_mk_tcall_register(reg)                pb_mk_dr(pb_tcall_register, 0, reg)
#define pb_mk_return()                           ((instruction_t)pb_return)
#define pb_mk_jit(n)                            \
  ((instruction_t)pb_jit),                      \
  ((instruction_t)(n)),                         \
  pb_mk_nop()
#define pb_mk_ncall(p)                          \
  ((instruction_t)pb_ncall),                    \
  ((instruction_t)((uptr)(p)),                  \
  ((instruction_t)((uptr)(p) >> 32))

#define pb_reg_count 16

enum {
  pb_allocation_pointer = 0,
  pb_frame_pointer = 1,
  pb_return_value = 2,
};

typedef struct machine_state {
  uptr machine_regs[pb_reg_count];
  char *heap;
  long heapsize;
  char *stack;
  long stacksize;
  char *code;
  char *codesize;
  char *codep;
} machine_state;

extern machine_state ms;

void pb_interp(instruction_t *bytecode);
