#include "pb.h"
#include "compile.h"
#include <stdio.h>

#define LOAD_UNALIGNED_PTR(addr)                                        \
  ((((uptr)((unsigned *)(addr))[1]) << 32) | ((unsigned *)(addr))[0])
#define SHIFT_MASK(v) ((v) & (64-1))

void pb_interp(machine_state *ms, instruction_t *bytecode) {
  instruction_t *ip = bytecode, *next_ip, instr;

  int flag = 0;

#define regs (ms->machine_regs)

  while (1) {
    instr = *ip;
    next_ip = ip + 1;

    switch (INSTR_op(instr)) {
    case pb_nop:
      break;
    case pb_mov_register:
      regs[INSTR_dr_dest(instr)] = regs[INSTR_dr_reg(instr)];
      break;
    case pb_mov_immediate:
      regs[INSTR_di_dest(instr)] = INSTR_di_imm(instr);
      break;
    case pb_literal:
      regs[INSTR_di_dest(instr)] = LOAD_UNALIGNED_PTR(ip + 1);
      next_ip = ip + 3;
      break;
    case pb_bin_add_register:
      regs[INSTR_drr_dest(instr)] =
        regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)];
      break;
    case pb_bin_add_immediate:
      regs[INSTR_dri_dest(instr)] =
        regs[INSTR_dri_reg(instr)] + (uptr)INSTR_dri_imm(instr);
      break;
    case pb_bin_sub_register:
      regs[INSTR_drr_dest(instr)] =
        regs[INSTR_drr_reg1(instr)] - regs[INSTR_drr_reg2(instr)];
      break;
    case pb_bin_sub_immediate:
      regs[INSTR_dri_dest(instr)] =
        regs[INSTR_dri_reg(instr)] - (uptr)INSTR_dri_imm(instr);
      break;
    case pb_bin_mul_register:
      regs[INSTR_drr_dest(instr)] =
        regs[INSTR_drr_reg1(instr)] * regs[INSTR_drr_reg2(instr)];
      break;
    case pb_bin_mul_immediate:
      regs[INSTR_dri_dest(instr)] =
        regs[INSTR_dri_reg(instr)] * (uptr)INSTR_dri_imm(instr);
      break;
    case pb_bin_div_register:
      regs[INSTR_drr_dest(instr)] =
        (iptr)regs[INSTR_drr_reg1(instr)] / (iptr)regs[INSTR_drr_reg2(instr)];
      break;
    case pb_bin_div_immediate:
      regs[INSTR_dri_dest(instr)] =
        (iptr)regs[INSTR_dri_reg(instr)] / (iptr)INSTR_dri_imm(instr);
      break;
    case pb_bin_rem_register:
      regs[INSTR_drr_dest(instr)] =
        (iptr)regs[INSTR_drr_reg1(instr)] % (iptr)regs[INSTR_drr_reg2(instr)];
      break;
    case pb_bin_rem_immediate:
      regs[INSTR_dri_dest(instr)] =
        (iptr)regs[INSTR_dri_reg(instr)] % (iptr)INSTR_dri_imm(instr);
      break;
    case pb_bin_and_register:
      regs[INSTR_drr_dest(instr)] =
        regs[INSTR_drr_reg1(instr)] & regs[INSTR_drr_reg2(instr)];
      break;
    case pb_bin_and_immediate:
      regs[INSTR_dri_dest(instr)] =
        regs[INSTR_dri_reg(instr)] & (uptr)INSTR_dri_imm(instr);
      break;
    case pb_bin_ior_register:
      regs[INSTR_drr_dest(instr)] =
        regs[INSTR_drr_reg1(instr)] | regs[INSTR_drr_reg2(instr)];
      break;
    case pb_bin_ior_immediate:
      regs[INSTR_dri_dest(instr)] =
        regs[INSTR_dri_reg(instr)] | (uptr)INSTR_dri_imm(instr);
      break;
    case pb_bin_xor_register:
      regs[INSTR_drr_dest(instr)] =
        regs[INSTR_drr_reg1(instr)] ^ regs[INSTR_drr_reg2(instr)];
      break;
    case pb_bin_xor_immediate:
      regs[INSTR_dri_dest(instr)] =
        regs[INSTR_dri_reg(instr)] ^ (uptr)INSTR_dri_imm(instr);
      break;
    case pb_bin_lsl_register:
      regs[INSTR_drr_dest(instr)] =
        regs[INSTR_drr_reg1(instr)] << SHIFT_MASK(regs[INSTR_drr_reg2(instr)]);
      break;
    case pb_bin_lsl_immediate:
      regs[INSTR_dri_dest(instr)] =
        regs[INSTR_dri_reg(instr)] << SHIFT_MASK(INSTR_dri_imm(instr));
      break;
    case pb_bin_lsr_register:
      regs[INSTR_drr_dest(instr)] =
        regs[INSTR_drr_reg1(instr)] >> SHIFT_MASK(regs[INSTR_drr_reg2(instr)]);
      break;
    case pb_bin_lsr_immediate:
      regs[INSTR_dri_dest(instr)] =
        regs[INSTR_dri_reg(instr)] >> SHIFT_MASK(INSTR_dri_imm(instr));
      break;
    case pb_bin_asr_register:
      regs[INSTR_drr_dest(instr)] =
        (iptr)regs[INSTR_drr_reg1(instr)] >> SHIFT_MASK(regs[INSTR_drr_reg2(instr)]);
      break;
    case pb_bin_asr_immediate:
      regs[INSTR_dri_dest(instr)] =
        (iptr)regs[INSTR_dri_reg(instr)] >> SHIFT_MASK(INSTR_dri_imm(instr));
      break;
    case pb_ld_immediate:
      regs[INSTR_dri_dest(instr)] =
        *(uptr *)(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr));
      break;
    case pb_ld_register:
      regs[INSTR_drr_dest(instr)] =
        *(uptr *)(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]);
      break;
    case pb_st_immediate:
      *(uptr *)(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr)) =
        regs[INSTR_dri_dest(instr)];
      break;
    case pb_st_register:
      *(uptr *)(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]) =
        regs[INSTR_drr_dest(instr)];
      break;
    case pb_cmp_eq_register:
      flag = regs[INSTR_dr_dest(instr)] == regs[INSTR_dr_reg(instr)];
      break;
    case pb_cmp_eq_immediate:
      flag = regs[INSTR_di_dest(instr)] == (uptr)INSTR_di_imm(instr);
      break;
    case pb_cmp_lt_register:
      flag = (iptr)regs[INSTR_dr_dest(instr)] < (iptr)regs[INSTR_dr_reg(instr)];
      break;
    case pb_cmp_lt_immediate:
      flag = (iptr)regs[INSTR_di_dest(instr)] < (iptr)INSTR_di_imm(instr);
      break;
    case pb_cmp_gt_register:
      flag = (iptr)regs[INSTR_dr_dest(instr)] > (iptr)regs[INSTR_dr_reg(instr)];
      break;
    case pb_cmp_gt_immediate:
      flag = (iptr)regs[INSTR_di_dest(instr)] > (iptr)INSTR_di_imm(instr);
      break;
    case pb_cmp_le_register:
      flag = (iptr)regs[INSTR_dr_dest(instr)] <= (iptr)regs[INSTR_dr_reg(instr)];
      break;
    case pb_cmp_le_immediate:
      flag = (iptr)regs[INSTR_di_dest(instr)] <= (iptr)INSTR_di_imm(instr);
      break;
    case pb_cmp_ge_register:
      flag = (iptr)regs[INSTR_dr_dest(instr)] >= (iptr)regs[INSTR_dr_reg(instr)];
      break;
    case pb_cmp_ge_immediate:
      flag = (iptr)regs[INSTR_di_dest(instr)] >= (iptr)INSTR_di_imm(instr);
      break;
    case pb_jmp_true:
      if (flag) {
        next_ip = (instruction_t *)((char *)next_ip + INSTR_i_imm(instr));
      }
      break;
    case pb_jmp_false:
      if (!flag) {
        next_ip = (instruction_t *)((char *)next_ip + INSTR_i_imm(instr));
      }
      break;
    case pb_jmp_always:
      next_ip = (instruction_t *)((char *)next_ip + INSTR_i_imm(instr));
      break;
    case pb_addr:
      regs[INSTR_adr_dest(instr)] = (uptr)next_ip + (INSTR_adr_imm(instr) << 2);
      break;
    case pb_call_immediate:
      pb_interp(ms, (instruction_t *)((char *)next_ip + INSTR_i_imm(instr)));
      break;
    case pb_call_register:
      pb_interp(ms, (instruction_t *)regs[INSTR_dr_reg(instr)]);
      break;
    case pb_tcall_immediate:
      return pb_interp(ms, (instruction_t *)((char *)next_ip + INSTR_i_imm(instr)));
    case pb_tcall_register:
      return pb_interp(ms, (instruction_t *)regs[INSTR_dr_reg(instr)]);
    case pb_return:
      return;
    case pb_jit:
      *(int *)(ip + 1) -= 1;
      if (*(int *)(ip + 1) <= 0) {
        *ip = (instruction_t)pb_ntcall;
        *(ip + 1) = (uint64_t)(ms->codep);
        *(ip + 2) = (uint64_t)(ms->codep) >> 32;
        ms->codep = compile(ip + 3, ms->codep);
        next_ip = ip;
      } else {
        next_ip = ip + 3;
      }
      break;
    case pb_ncall:
      ((void (*)(machine_state *))LOAD_UNALIGNED_PTR(ip + 1))(ms);
      next_ip = ip + 3;
      break;
    case pb_ntcall:
      return ((void (*)(machine_state *))LOAD_UNALIGNED_PTR(ip + 1))(ms);
    }

    ip = next_ip;
  }
}

