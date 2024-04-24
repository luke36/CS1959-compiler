#include "assembler.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define asm_rex   0x40
#define asm_rex_w 0b1000
#define asm_rex_r 0b0100
#define asm_rex_x 0b0010
#define asm_rex_b 0b0001

#define asm_opcode_escape           0x0f

#define asm_opcode_nop              0x90

#define asm_opcode_push             0x50
#define asm_opcode_pop              0x58

#define asm_opcode_mov_imm_rm       0xc7
#define asm_opcode_mov_imm_reg      0xb8
#define asm_opcode_mov_reg_rm       0x89
#define asm_opcode_mov_rm_reg       0x8b

#define asm_opcode_add_imm_8        0x83
#define asm_opcode_add_imm_32       0x81
#define asm_opcode_add_imm_ext      0x0
#define asm_opcode_add_reg_rm       0x01
#define asm_opcode_add_rm_reg       0x03

#define asm_opcode_sub_imm_8        0x83
#define asm_opcode_sub_imm_32       0x81
#define asm_opcode_sub_imm_ext      0x5
#define asm_opcode_sub_reg_rm       0x29
#define asm_opcode_sub_rm_reg       0x2b

#define asm_opcode_and_imm_8        0x83
#define asm_opcode_and_imm_32       0x81
#define asm_opcode_and_imm_ext      0x4
#define asm_opcode_and_reg_rm       0x21
#define asm_opcode_and_rm_reg       0x23

#define asm_opcode_or_imm_8         0x83
#define asm_opcode_or_imm_32        0x81
#define asm_opcode_or_imm_ext       0x1
#define asm_opcode_or_reg_rm        0x09
#define asm_opcode_or_rm_reg        0x0b

#define asm_opcode_xor_imm_8        0x83
#define asm_opcode_xor_imm_32       0x81
#define asm_opcode_xor_imm_ext      0x6
#define asm_opcode_xor_reg_rm       0x31
#define asm_opcode_xor_rm_reg       0x33

#define asm_opcode_cmp_imm_8        0x83
#define asm_opcode_cmp_imm_32       0x81
#define asm_opcode_cmp_imm_ext      0x7
#define asm_opcode_cmp_reg_rm       0x39
#define asm_opcode_cmp_rm_reg       0x3b

#define asm_opcode_imul_imm_8       0x6b
#define asm_opcode_imul_imm_32      0x69
#define asm_opcode_imul_rm_reg_high 0xaf

#define asm_opcode_shift            0xc1
#define asm_opcode_sar_ext          0x7
#define asm_opcode_shr_ext          0x5
#define asm_opcode_shl_ext          0x4

#define asm_opcode_jmp_imm_8        0xeb
#define asm_opcode_jmp_imm_32       0xe9
#define asm_opcode_je_imm_8         0x74
#define asm_opcode_je_imm_32_high   0x84
#define asm_opcode_jne_imm_8        0x75
#define asm_opcode_jne_imm_32_high  0x85
#define asm_opcode_jg_imm_8         0x7f
#define asm_opcode_jg_imm_32_high   0x8f
#define asm_opcode_jl_imm_8         0x7c
#define asm_opcode_jl_imm_32_high   0x8c
#define asm_opcode_jge_imm_8        0x7d
#define asm_opcode_jge_imm_32_high  0x8d
#define asm_opcode_jle_imm_8        0x7e
#define asm_opcode_jle_imm_32_high  0x8e
#define asm_opcode_jmp_reg          0xff
#define asm_opcode_jmp_reg_ext      0x4

#define asm_opcode_call_reg         0xff
#define asm_opcode_call_reg_ext     0x2
#define asm_opcode_ret              0xc3

char *asm_cp;

static void bad_format() {
  fprintf(stderr, "assembler: bad format");
  /* *(int *)NULL = 0; */
}

void asm_emit_real(uint8_t x) {
  *(asm_cp++) = x;
  /* fprintf(stderr, "%02x", x); */
}

void asm_emit_fake(uint8_t x) {
  asm_cp++;
}

void (*asm_emit)(uint8_t);

#define asm_reg_is_ext(reg)  (reg & 0b1000)
#define asm_reg_code(reg)    (reg & 0b111)

#define asm_is_int32(imm)                                   \
  (-2147483648 <= (int64_t)imm && (int64_t)imm <= 2147483647)
#define asm_is_int8(imm)                                   \
  (-128 <= (int32_t)imm && (int32_t)imm <= 127)

void asm_emit_i8(int8_t i8) {
  asm_emit(i8);
}

static void asm_emit_i64(int64_t i64) {
  asm_emit(i64);
  asm_emit(i64 >>= 8);
  asm_emit(i64 >>= 8);
  asm_emit(i64 >>= 8);
  asm_emit(i64 >>= 8);
  asm_emit(i64 >>= 8);
  asm_emit(i64 >>= 8);
  asm_emit(i64 >>= 8);
}

void asm_emit_i32(int32_t i32) {
  asm_emit(i32);
  asm_emit(i32 >>= 8);
  asm_emit(i32 >>= 8);
  asm_emit(i32 >>= 8);
}

static void asm_emit_rex(uint8_t w, asm_reg reg, asm_reg index, asm_reg rm) {
  uint8_t rex = asm_rex;
  rex |= w;
  rex |= asm_reg_is_ext(reg) ? asm_rex_r : 0x0;
  rex |= asm_reg_is_ext(index) ? asm_rex_x : 0x0;
  rex |= asm_reg_is_ext(rm) ? asm_rex_b : 0x0;
  if (rex != asm_rex)
    asm_emit(rex);
}

#define asm_emit_modrm(mod, reg, rm)            \
  asm_emit(((mod) << 6) | ((reg) << 3) | (rm))
#define asm_emit_index(scale, index, base)      \
  asm_emit(((scale) << 6) | ((index) << 3) | (base))

#define asm_emit_operand_reg_reg(reg1, reg2)    \
  asm_emit_modrm(0x3, asm_reg_code(reg1), asm_reg_code(reg2))

static void asm_emit_operand_reg_base_offset(asm_reg reg, asm_reg base, int32_t offset) {
  if (base == asm_rsp || base == asm_r12)
    bad_format();
  bool is_8 = asm_is_int8(offset);
  asm_emit_modrm((is_8 ? 0x1 : 0x2), asm_reg_code(reg), asm_reg_code(base));
  if (is_8) {
    asm_emit_i8(offset);
  } else {
    asm_emit_i32(offset);
  }
}

static void asm_emit_operand_reg_base_index(asm_reg reg, asm_reg base, asm_reg index) {
  if (base == asm_rbp || base == asm_r13) {
    asm_emit_modrm(0x1, asm_reg_code(reg), asm_rsp);
    asm_emit_index(0, asm_reg_code(index), asm_reg_code(base));
    asm_emit_i8(0);
  } else {
    asm_emit_modrm(0x0, asm_reg_code(reg), asm_rsp);
    asm_emit_index(0, asm_reg_code(index), asm_reg_code(base));
  }
}

void asm_emit_nop() {
  asm_emit(asm_opcode_nop);
}

void asm_emit_push_reg(asm_reg reg) {
  asm_emit_rex(0, 0, 0, reg);
  asm_emit(asm_opcode_push + asm_reg_code(reg));
}

void asm_emit_pop_reg(asm_reg reg) {
  asm_emit_rex(0, 0, 0, reg);
  asm_emit(asm_opcode_pop + asm_reg_code(reg));
}

static void emit_arith(asm_rand src, asm_rand dest,
                       uint8_t opcode_8, uint8_t opcode_32, uint8_t opcode_ext,
                       uint8_t opcode_reg_rm, uint8_t opcode_rm_reg) {
  bool is_8;

  switch (src.type) {
  case asm_rand_index:
    switch (dest.type) {
    case asm_rand_reg:

      asm_emit_rex(asm_rex_w, dest.reg, src.index, src.base);
      asm_emit(opcode_rm_reg);
      asm_emit_operand_reg_base_index(dest.reg, src.base, src.index);

      break;
    case asm_rand_index:
    case asm_rand_disp:
    case asm_rand_imm:
      bad_format();
    }
    break;
  case asm_rand_disp:
    switch (dest.type) {
    case asm_rand_reg:

      asm_emit_rex(asm_rex_w, dest.reg, 0, src.base);
      asm_emit(opcode_rm_reg);
      asm_emit_operand_reg_base_offset(dest.reg, src.base, src.offset);

      break;
    case asm_rand_index:
    case asm_rand_disp:
    case asm_rand_imm:
      bad_format();
    }
    break;
  case asm_rand_reg:
    switch (dest.type) {
    case asm_rand_index:

      asm_emit_rex(asm_rex_w, src.reg, dest.index, dest.base);
      asm_emit(opcode_reg_rm);
      asm_emit_operand_reg_base_index(src.reg, dest.base, dest.index);

      break;
    case asm_rand_disp:

      asm_emit_rex(asm_rex_w, src.reg, 0, dest.base);
      asm_emit(opcode_reg_rm);
      asm_emit_operand_reg_base_offset(src.reg, dest.base, dest.offset);

      break;
    case asm_rand_reg:

      asm_emit_rex(asm_rex_w, src.reg, 0, dest.reg);
      asm_emit(opcode_reg_rm);
      asm_emit_operand_reg_reg(src.reg, dest.reg);

      break;
    case asm_rand_imm:
      bad_format();
    }
    break;
  case asm_rand_imm:
    switch (dest.type) {
    case asm_rand_index:

      is_8 = asm_is_int8(src.imm);
      asm_emit_rex(asm_rex_w, 0, dest.index, dest.base);
      asm_emit(is_8 ? opcode_8 : opcode_32);
      asm_emit_operand_reg_base_index(opcode_ext, dest.base, dest.index);
      if (is_8) {
        asm_emit_i8(src.imm);
      } else {
        asm_emit_i32(src.imm);
      }

      break;
    case asm_rand_disp:

      is_8 = asm_is_int8(src.imm);
      asm_emit_rex(asm_rex_w, 0, 0, dest.base);
      asm_emit(is_8 ? opcode_8 : opcode_32);
      asm_emit_operand_reg_base_offset(opcode_ext, dest.base, dest.offset);
      if (is_8) {
        asm_emit_i8(src.imm);
      } else {
        asm_emit_i32(src.imm);
      }

      break;
    case asm_rand_reg:

      is_8 = asm_is_int8(src.imm);
      asm_emit_rex(asm_rex_w, 0, 0, dest.reg);
      asm_emit(is_8 ? opcode_8 : opcode_32);
      asm_emit_operand_reg_reg(opcode_ext, dest.reg);
      if (is_8) {
        asm_emit_i8(src.imm);
      } else {
        asm_emit_i32(src.imm);
      }

      break;
    case asm_rand_imm:
      bad_format();
    }
    break;
  }
}

static void emit_mov(asm_rand src, asm_rand dest) {
  bool is_32;

  switch (src.type) {
  case asm_rand_index:
  case asm_rand_disp:
  case asm_rand_reg:
    emit_arith(src, dest, 0, 0, 0, /* dummy */
             asm_opcode_mov_reg_rm, asm_opcode_mov_rm_reg);
    break;
  case asm_rand_imm:
    switch (dest.type) {
    case asm_rand_index:

      asm_emit_rex(asm_rex_w, 0, dest.index, dest.base);
      asm_emit(asm_opcode_mov_imm_rm);
      asm_emit_operand_reg_base_index(0, dest.base, dest.index);
      asm_emit_i32((int32_t)src.imm);

      break;
    case asm_rand_disp:

      asm_emit_rex(asm_rex_w, 0, 0, dest.base);
      asm_emit(asm_opcode_mov_imm_rm);
      asm_emit_operand_reg_base_offset(0, dest.base, dest.offset);
      asm_emit_i32((int32_t)src.imm);

      break;
    case asm_rand_reg:

      is_32 = asm_is_int32(src.imm);
      asm_emit_rex(asm_rex_w, 0, 0, dest.reg);
      asm_emit(is_32 ? asm_opcode_mov_imm_rm
                     : asm_opcode_mov_imm_reg + asm_reg_code(dest.reg));
      if (is_32) {
        asm_emit_operand_reg_reg(0, dest.reg);
        asm_emit_i32((int32_t)src.imm);
      } else {
        asm_emit_i64((int64_t)src.imm);
      }

      break;
    case asm_rand_imm:
      bad_format();
    }
    break;
  }
}

static void emit_shift(asm_rand src, asm_rand dest, uint8_t opcode_ext) {
  if (src.type != asm_rand_imm)
    bad_format();
  switch (dest.type) {
  case asm_rand_index:

    asm_emit_rex(asm_rex_w, 0, dest.index, dest.base);
    asm_emit(asm_opcode_shift);
    asm_emit_operand_reg_base_index(opcode_ext, dest.base, dest.index);
    asm_emit_i8(src.imm);

    break;
  case asm_rand_disp:

    asm_emit_rex(asm_rex_w, 0, 0, dest.base);
    asm_emit(asm_opcode_shift);
    asm_emit_operand_reg_base_offset(opcode_ext, dest.base, dest.offset);
    asm_emit_i8(src.imm);

    break;
  case asm_rand_reg:

    asm_emit_rex(asm_rex_w, 0, 0, dest.reg);
    asm_emit(asm_opcode_shift);
    asm_emit_operand_reg_reg(opcode_ext, dest.reg);
    asm_emit_i8(src.imm);

    break;
  case asm_rand_imm:
    bad_format();
  }
}

/* imul could have 3 operands though. Now the selector
   does not take that into account */
static void emit_imul(asm_rand src, asm_rand dest) {
  bool is_8;

  if (dest.type != asm_rand_reg)
    bad_format();
  switch (src.type) {
  case asm_rand_index:

    asm_emit_rex(asm_rex_w, dest.reg, src.index, src.base);
    asm_emit(asm_opcode_escape);
    asm_emit(asm_opcode_imul_rm_reg_high);
    asm_emit_operand_reg_base_index(dest.reg, src.base, src.index);

    break;
  case asm_rand_disp:

    asm_emit_rex(asm_rex_w, dest.reg, 0, src.base);
    asm_emit(asm_opcode_escape);
    asm_emit(asm_opcode_imul_rm_reg_high);
    asm_emit_operand_reg_base_offset(dest.reg, src.base, src.offset);

    break;
  case asm_rand_reg:

    asm_emit_rex(asm_rex_w, dest.reg, 0, src.reg);
    asm_emit(asm_opcode_escape);
    asm_emit(asm_opcode_imul_rm_reg_high);
    asm_emit_operand_reg_reg(dest.reg, src.reg);

    break;
  case asm_rand_imm:

    is_8 = asm_is_int8(src.imm);
    asm_emit_rex(asm_rex_w, dest.reg, 0, dest.reg);
    asm_emit(is_8 ? asm_opcode_imul_imm_8 : asm_opcode_imul_imm_32);
    asm_emit_operand_reg_reg(dest.reg, dest.reg);
    if (is_8) {
      asm_emit_i8(src.imm);
    } else {
      asm_emit_i32(src.imm);
    }

    break;
  }
}
#define asm_emit_imul_imm_reg_reg(imm, src, dest)                       \
  __is_int8 = asm_is_int8(imm);                                         \
  asm_emit_rex(asm_rex_w, dest, 0, src)                                 \
  asm_emit(__is_int8 ? asm_opcode_imul_imm_8 : asm_opcode_imul_imm_32)  \
  asm_emit_operand_reg_reg(dest, src)                                   \
  if (__is_int8) {                                                      \
    asm_emit_i8(imm)                                                    \
  } else {                                                              \
    asm_emit_i32(imm)                                                   \
  }
#define asm_emit_imul_imm_disp_reg(imm, base, offset, reg)              \
  __is_int8 = asm_is_int8(imm);                                         \
  asm_emit_rex(asm_rex_w, reg, 0, base)                                 \
  asm_emit(__is_int8 ? asm_opcode_imul_imm_8 : asm_opcode_imul_imm_32)  \
  asm_emit_operand_reg_base_offset(reg, base, offset)                   \
  if (__is_int8) {                                                      \
    asm_emit_i8(imm)                                                    \
  } else {                                                              \
    asm_emit_i32(imm)                                                   \
  }
#define asm_emit_imul_imm_index_reg(imm, base, index, reg)              \
  __is_int8 = asm_is_int8(imm);                                         \
  asm_emit_rex(asm_rex_w, reg, index, base)                             \
  asm_emit(__is_int8 ? asm_opcode_imul_imm_8 : asm_opcode_imul_imm_32)  \
  asm_emit_operand_reg_base_index(reg, base, index)                     \
  if (__is_int8) {                                                      \
    asm_emit_i8(imm)                                                    \
  } else {                                                              \
    asm_emit_i32(imm)                                                   \
  }

#define CASE_ARITH(op)                                                  \
  case asm_##op:                                                        \
  emit_arith(src, dest,                                                 \
             asm_opcode_##op##_imm_8, asm_opcode_##op##_imm_32, asm_opcode_##op##_imm_ext, \
             asm_opcode_##op##_reg_rm, asm_opcode_##op##_rm_reg);       \
  break;

#define CASE_SHIFT(op)                          \
  case asm_##op:                                \
  emit_shift(src, dest, asm_opcode_##op##_ext); \
  break;

void asm_emit_compute(asm_op op, asm_rand src, asm_rand dest) {
  switch (op) {
  case asm_mov:
    emit_mov(src, dest);
    break;

  case asm_imul:
    emit_imul(src, dest);
    break;

  CASE_ARITH(add)
  CASE_ARITH(sub)
  CASE_ARITH(and)
  CASE_ARITH(or)
  CASE_ARITH(xor)
  CASE_ARITH(cmp)

  CASE_SHIFT(sar)
  CASE_SHIFT(shr)
  CASE_SHIFT(shl)
  }
}

void asm_emit_jmp(asm_rand r) {
  bool is_8;

  switch (r.type) {
  case asm_rand_index:
    asm_emit_rex(0, 0, r.index, r.base);
    asm_emit(asm_opcode_jmp_reg);
    asm_emit_operand_reg_base_index(asm_opcode_jmp_reg_ext, r.base, r.index);
    break;
  case asm_rand_disp:
    asm_emit_rex(0, 0, 0, r.base);
    asm_emit(asm_opcode_jmp_reg);
    asm_emit_operand_reg_base_offset(asm_opcode_jmp_reg_ext, r.base, r.offset);
    break;
  case asm_rand_reg:
    asm_emit_rex(0, 0, 0, r.reg);
    asm_emit(asm_opcode_jmp_reg);
    asm_emit_operand_reg_reg(asm_opcode_jmp_reg_ext, r.reg);
    break;
  case asm_rand_imm:
    is_8 = asm_is_int8(r.imm);
    asm_emit(is_8 ? asm_opcode_jmp_imm_8 : asm_opcode_jmp_imm_32);
    if (is_8) {
      asm_emit_i8(r.imm);
    } else {
      asm_emit_i32(r.imm);
    }
    break;
  }
}

static void emit_jcc(int32_t offset, uint8_t opcode_8, uint8_t opcode_32_high) {
  if (asm_is_int8(offset)) {
    asm_emit(opcode_8);
    asm_emit_i8(offset);
  } else {
    asm_emit(asm_opcode_escape);
    asm_emit(opcode_32_high);
    asm_emit_i32(offset);
  }
}

#define CASE_JCC(op)                                                    \
  case asm_##op:                                                        \
  emit_jcc(offset, asm_opcode_##op##_imm_8, asm_opcode_##op##_imm_32_high); \
  break;

void asm_emit_jcc(asm_jmp_op op, int32_t offset) {
  switch (op) {
  CASE_JCC(je)
  CASE_JCC(jne)
  CASE_JCC(jg)
  CASE_JCC(jl)
  CASE_JCC(jge)
  CASE_JCC(jle)
  }
}

void asm_emit_ret() {
  asm_emit(asm_opcode_ret);
}

void asm_emit_call(asm_reg reg) {
  asm_emit_rex(0, 0, 0, reg);
  asm_emit(asm_opcode_call_reg);
  asm_emit_operand_reg_reg(asm_opcode_call_reg_ext, reg);
}

extern asm_rand asm_mk_rand_reg(asm_reg reg);
extern asm_rand asm_mk_rand_imm(int64_t imm);
extern asm_rand asm_mk_rand_index(asm_reg base, asm_reg index);
extern asm_rand asm_mk_rand_disp(asm_reg base, int32_t offset);
