#include "ir.h"

extern ir_triv ir_mk_triv_const(int64_t c);
extern ir_triv ir_mk_triv_reg(ir_reg r);
extern ir_triv ir_mk_triv_uvar(uvar_id uv);
extern ir_triv ir_mk_triv_fvar(fvar_pos fv);
extern ir_triv ir_mk_triv_svar(svar_pos sv);
extern ir_rhs ir_mk_rhs_triv(ir_triv triv);
extern ir_rhs ir_mk_rhs_bin(ir_bin op, ir_triv x, ir_triv y);
extern ir_rhs ir_mk_rhs_mref(ir_triv base, ir_triv offset);
extern ir_instr ir_mk_instr_set(ir_triv l, ir_rhs r);
extern ir_instr ir_mk_instr_mset(ir_triv base, ir_triv offset, ir_rhs r);
extern ir_instr ir_mk_instr_label(label_id lab);
extern ir_instr ir_mk_instr_jmp(label_id dest);
extern ir_instr ir_mk_instr_jcc(ir_rel op, ir_triv x, ir_triv y, label_id dest);
extern ir_instr ir_mk_instr_call(ir_rhs f, set live);
extern ir_instr ir_mk_instr_tcall(ir_rhs f, set live);
extern ir_instr ir_mk_instr_ret();
extern bool ir_triv_eq(ir_triv x, ir_triv y);
extern bool ir_reg_is_callee_saved(ir_reg r);
extern bool ir_triv_is_const(ir_triv tr);
extern bool ir_triv_is_reg(ir_triv tr);
extern bool ir_triv_is_uvar(ir_triv tr);
extern bool ir_triv_is_fvar(ir_triv tr);
extern bool ir_triv_is_svar(ir_triv tr);
extern bool ir_rhs_is_triv(ir_rhs r);
extern bool ir_rhs_is_bin(ir_rhs r);
extern bool ir_rhs_is_mref(ir_rhs r);
extern ir_code ir_code_cons(ir_instr i, ir_code next);
extern ir_code ir_code_nil();

static void show_triv(ir_triv tr, FILE *fp) {
  switch (tr.type) {
  case ir_triv_const:
    fprintf(fp, "%lu", tr.const_);
    break;
  case ir_triv_uvar:
    fprintf(fp, "x.%lu", tr.uv);
    break;
  case ir_triv_reg:
    switch (tr.reg) {
    case ir_rax: fprintf(fp, "rax"); break;
    case ir_rdi: fprintf(fp, "rdi"); break;
    case ir_rsi: fprintf(fp, "rsi"); break;
    case ir_rdx: fprintf(fp, "rdx"); break;
    case ir_rcx: fprintf(fp, "rcx"); break;
    case ir_r8:  fprintf(fp, "r8"); break;
    case ir_r9:  fprintf(fp, "r9"); break;
    case ir_r10: fprintf(fp, "r10"); break;
    case ir_r11: fprintf(fp, "r11"); break;
    case ir_rbx: fprintf(fp, "rbx"); break;
    case ir_r13: fprintf(fp, "r13"); break;
    case ir_r14: fprintf(fp, "r14"); break;
    case ir_r15: fprintf(fp, "r15"); break;
    }
    break;
  case ir_triv_fvar:
    fprintf(fp, "fv%lu", tr.fv);
    break;
  case ir_triv_svar:
    fprintf(fp, "sv%lu", tr.sv);
    break;
  }
}

static void show_rhs(ir_rhs r, FILE *fp) {
  switch (r.type) {
  case ir_rhs_triv:
    show_triv(r.triv, fp);
    break;
  case ir_rhs_bin:
    fprintf(fp, "(");
    switch (r.bin.op) {
    case ir_bin_add: fprintf(fp, "+"); break;
    case ir_bin_sub: fprintf(fp, "-"); break;
    case ir_bin_mul: fprintf(fp, "*"); break;
    case ir_bin_and: fprintf(fp, "logand"); break;
    case ir_bin_or:  fprintf(fp, "logor"); break;
    case ir_bin_xor: fprintf(fp, "logxor"); break;
    case ir_bin_sar: fprintf(fp, "sar"); break;
    case ir_bin_shr: fprintf(fp, "shr"); break;
    case ir_bin_shl: fprintf(fp, "shl"); break;
    }
    fprintf(fp, " ");
    show_triv(r.bin.x, fp);
    fprintf(fp, " ");
    show_triv(r.bin.y, fp);
    fprintf(fp, ")");
    break;
  case ir_rhs_mref:
    fprintf(fp, "(");
    fprintf(fp, "mref");
    fprintf(fp, " ");
    show_triv(r.mref.base, fp);
    fprintf(fp, " ");
    show_triv(r.mref.offset, fp);
    fprintf(fp, ")");
    break;
  }
}

void ir_show(ir_code c, FILE *fp) {
  for (; c != ir_code_nil(); c = c->next) {
    switch (c->i.type) {
    case ir_instr_set:
      fprintf(fp, "(");
      fprintf(fp, "set!");
      fprintf(fp, " ");
      show_triv(c->i.set.l, fp);
      fprintf(fp, " ");
      show_rhs(c->i.set.r, fp);
      fprintf(fp, ")");
      break;
    case ir_instr_mset:
      fprintf(fp, "(");
      fprintf(fp, "mset!");
      fprintf(fp, " ");
      show_triv(c->i.mset.base, fp);
      fprintf(fp, " ");
      show_triv(c->i.mset.offset, fp);
      fprintf(fp, " ");
      show_rhs(c->i.mset.r, fp);
      fprintf(fp, ")");
      break;
    case ir_instr_label:
      fprintf(fp, "L%lu", c->i.label);
      break;
    case ir_instr_jmp:
      fprintf(fp, "(L%lu)", c->i.jmp.dest);
      break;
    case ir_instr_jcc:
      fprintf(fp, "(");
      fprintf(fp, "if");
      fprintf(fp, " ");
      fprintf(fp, "(");
      switch (c->i.jcc.op) {
      case ir_rel_eq: fprintf(fp, "="); break;
      case ir_rel_ne: fprintf(fp, "<>"); break;
      case ir_rel_gt: fprintf(fp, ">"); break;
      case ir_rel_ge: fprintf(fp, ">="); break;
      case ir_rel_lt: fprintf(fp, "<"); break;
      case ir_rel_le: fprintf(fp, "<="); break;
      }
      fprintf(fp, " ");
      show_triv(c->i.jcc.x, fp);
      fprintf(fp, " ");
      show_triv(c->i.jcc.y, fp);
      fprintf(fp, ")");
      fprintf(fp, " ");
      fprintf(fp, "(L%lu)", c->i.jcc.dest);
      fprintf(fp, ")");
      break;
    case ir_instr_call:
      fprintf(fp, "(");
      fprintf(fp, "call");
      fprintf(fp, " ");
      show_rhs(c->i.call.f, fp);
      fprintf(fp, ")");
      break;
    case ir_instr_tcall:
      fprintf(fp, "(");
      fprintf(fp, "tail-call");
      fprintf(fp, " ");
      show_rhs(c->i.call.f, fp);
      fprintf(fp, ")");
      break;
    case ir_instr_ret:
      fprintf(fp, "(ret)");
      break;
    }
    fprintf(fp, "\n");
  }
}
