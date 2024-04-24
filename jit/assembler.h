/* note: do not use %rsp in *disp or *index!
         do not use %r12 in *disp! */
/* %rax is not treated specially. */

#ifndef ASSEMBLE_H
#define ASSEMBLE_H

#include <stdint.h>

typedef uint8_t asm_reg;

#define asm_rax 0x0
#define asm_rcx 0x1
#define asm_rdx 0x2
#define asm_rbx 0x3
#define asm_rsp 0x4
#define asm_rbp 0x5
#define asm_rsi 0x6
#define asm_rdi 0x7
#define asm_r8  0x8
#define asm_r9  0x9
#define asm_r10 0xa
#define asm_r11 0xb
#define asm_r12 0xc
#define asm_r13 0xd
#define asm_r14 0xe
#define asm_r15 0xf

typedef enum asm_rand_type {
  asm_rand_index,
  asm_rand_disp,
  asm_rand_reg,
  asm_rand_imm
} asm_rand_type;

typedef struct asm_rand {
  asm_rand_type type;
  union {
    struct { asm_reg base; union { asm_reg index; int32_t offset; }; };
    asm_reg reg;
    int64_t imm;
  };
} asm_rand;

typedef enum asm_op {
  asm_mov,
  asm_add,
  asm_sub,
  asm_and,
  asm_or,
  asm_xor,
  asm_cmp,
  asm_imul,
  asm_sar,
  asm_shr,
  asm_shl
} asm_op;

typedef enum asm_jmp_op {
  asm_je,
  asm_jne,
  asm_jg,
  asm_jl,
  asm_jge,
  asm_jle
} asm_jmp_op;

inline asm_rand asm_mk_rand_reg(asm_reg reg) {
  asm_rand ret = { .type = asm_rand_reg, .reg = reg };
  return ret;
}

inline asm_rand asm_mk_rand_imm(int64_t imm) {
  asm_rand ret = { .type = asm_rand_imm, .imm = imm };
  return ret;
}

inline asm_rand asm_mk_rand_index(asm_reg base, asm_reg index) {
  asm_rand ret = { .type = asm_rand_index, .base = base, .index = index };
  return ret;
}

inline asm_rand asm_mk_rand_disp(asm_reg base, int32_t offset) {
  asm_rand ret = { .type = asm_rand_disp, .base = base, .offset = offset };
  return ret;
}

void asm_emit_i8(int8_t i8);
void asm_emit_i32(int32_t i32);
void asm_emit_nop();
void asm_emit_push_reg(asm_reg reg);
void asm_emit_pop_reg(asm_reg reg);
void asm_emit_compute(asm_op op, asm_rand src, asm_rand dest);
void asm_emit_jmp(asm_rand r);
void asm_emit_jcc(asm_jmp_op op, int32_t offset);
void asm_emit_ret();
void asm_emit_call(asm_reg reg);

extern char *asm_cp;
extern void (*asm_emit)(uint8_t);
void asm_emit_fake(uint8_t x);
void asm_emit_real(uint8_t x);

#endif
