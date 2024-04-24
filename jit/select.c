#include "select.h"
#include "common.h"

#include <limits.h>

static set *unspillable;

static ir_code select_many(ir_code c);

static ir_rel flip_rel(ir_rel op) {
  switch (op) {
  case ir_rel_eq: return ir_rel_eq;
  case ir_rel_ne: return ir_rel_eq;
  case ir_rel_gt: return ir_rel_lt;
  case ir_rel_ge: return ir_rel_le;
  case ir_rel_lt: return ir_rel_gt;
  case ir_rel_le: return ir_rel_ge;
  }
}

static bool is_comm(ir_bin op) {
  switch (op) {
  case ir_bin_add:
  case ir_bin_mul:
  case ir_bin_and:
  case ir_bin_or:
  case ir_bin_xor:
    return true;
  case ir_bin_sub:
  case ir_bin_sar:
  case ir_bin_shr:
  case ir_bin_shl:
    return false;
  }
}

static uvar_id get_uvar() {
  uvar_id uv = get_fresh();
  *unspillable = set_add_ex(uv, *unspillable);
  return uv;
}

static bool triv_is_mvar(ir_triv tr) {
  return ir_triv_is_fvar(tr) || ir_triv_is_svar(tr);
}

static bool triv_is_bigint(ir_triv tr) {
  return
    ir_triv_is_const(tr) &&
    (tr.const_ < INT32_MIN || tr.const_ > INT32_MAX);
}

static bool very_trivial(ir_triv tr) {
  return
    ir_triv_is_uvar(tr) ||
    ir_triv_is_reg(tr) ||
    (ir_triv_is_const(tr) && (tr.const_ >= INT32_MIN || tr.const_ < INT32_MAX));
}

static ir_code set_and_replace_rhs(ir_code c, ir_rhs *complex) {
  ir_triv uv = ir_mk_triv_uvar(get_uvar());
  ir_code ret = ir_code_cons(ir_mk_instr_set(uv, *complex), c);
  *complex = ir_mk_rhs_triv(uv);
  return ret;
}

static ir_code set_and_replace_triv(ir_code c, ir_triv *complex) {
  ir_triv uv = ir_mk_triv_uvar(get_uvar());
  ir_code ret = ir_code_cons(ir_mk_instr_set(uv, ir_mk_rhs_triv(*complex)), c);
  *complex = uv;
  return ret;
}

static ir_code replace_and_set_triv(ir_code c, ir_triv *fv) {
  ir_triv uv = ir_mk_triv_uvar(get_uvar());
  ir_code s = ir_code_cons(ir_mk_instr_set(*fv, ir_mk_rhs_triv(uv)), ir_code_nil());
  *fv = uv;
  c->next = s;
  return c;
}

/* c->next == NULL */
static ir_code select_one(ir_code c) {
  struct ir_instr *i = &c->i;
  switch (i->type) {
  case ir_instr_set:
    if (ir_rhs_is_mref(i->set.r)) {
      if (triv_is_mvar(i->set.l)) {
        return select_many(replace_and_set_triv(c, &i->set.l));
      } else if (very_trivial(i->set.r.mref.base) &&
                 very_trivial(i->set.r.mref.offset) &&
                 (!ir_triv_is_const(i->set.r.mref.base) ||
                  !ir_triv_is_const(i->set.r.mref.offset))) {
        return c;
      } else if (very_trivial(i->set.r.mref.base)) {
        return select_many(set_and_replace_triv(c, &i->set.r.mref.offset));
      } else {
        return select_many(set_and_replace_triv(c, &i->set.r.mref.base));
      }
    } else if (ir_rhs_is_bin(i->set.r)) {
      if (ir_triv_eq(i->set.l, i->set.r.bin.x)) {
        if (i->set.r.bin.op == ir_bin_mul && triv_is_mvar(i->set.l)) {
          ir_triv v = i->set.l;
          ir_triv x = i->set.r.bin.y;
          ir_triv uv = ir_mk_triv_uvar(get_uvar());
          i->set.l = i->set.r.bin.x = uv;
          i->set.r.bin.y = v;
          c->next = ir_code_cons(ir_mk_instr_set(v, ir_mk_rhs_triv(uv)), ir_code_nil());
          return ir_code_cons(ir_mk_instr_set(uv, ir_mk_rhs_triv(x)), c);
        } else if ((triv_is_bigint(i->set.r.bin.y)) ||
                   (triv_is_mvar(i->set.l) && triv_is_mvar(i->set.r.bin.y))) {
          return set_and_replace_triv(c, &i->set.r.bin.y);
        } else {
          return c;
        }
      } else if (is_comm(i->set.r.bin.op) && ir_triv_eq(i->set.l, i->set.r.bin.y)) {
        ir_triv t = i->set.r.bin.x;
        i->set.r.bin.x = i->set.r.bin.y;
        i->set.r.bin.y = t;
        return select_one(c);
      } else if (triv_is_mvar(i->set.l)) {
        if (!(i->set.r.bin.op == ir_bin_mul) &&
            very_trivial(i->set.r.bin.x) &&
            !ir_triv_eq(i->set.l, i->set.r.bin.y)) {
          ir_triv x = i->set.r.bin.x;
          i->set.r.bin.x = i->set.l;
          return
            select_many(ir_code_cons(ir_mk_instr_set(i->set.l, ir_mk_rhs_triv(x)), c));
        } else if (!(i->set.r.bin.op == ir_bin_mul) &&
                   is_comm(i->set.r.bin.op) &&
                   very_trivial(i->set.r.bin.y)) {
          ir_triv x = i->set.r.bin.x;
          ir_triv y = i->set.r.bin.y;
          i->set.r.bin.x = i->set.l;
          i->set.r.bin.y = x;
          return
            select_many(ir_code_cons(ir_mk_instr_set(i->set.l, ir_mk_rhs_triv(y)), c));
        } else {
          return select_many(replace_and_set_triv(c, &i->set.l));
        }
      } else {
        if (ir_triv_eq(i->set.l, i->set.r.bin.y)) {
          return select_many(replace_and_set_triv(c, &i->set.l));
        } else {
          ir_triv f, s;
          if (is_comm(i->set.r.bin.op) &&
              (!very_trivial(i->set.r.bin.y) ||
               (very_trivial(i->set.r.bin.x) &&
                (ir_triv_is_uvar(i->set.r.bin.y) ||
                 ir_triv_is_reg(i->set.r.bin.y))))) {
            f = i->set.r.bin.y;
            s = i->set.r.bin.x;
          } else {
            f = i->set.r.bin.x;
            s = i->set.r.bin.y;
          }
          i->set.r.bin.x = i->set.l;
          i->set.r.bin.y = s;
          return
            select_many(ir_code_cons(ir_mk_instr_set(i->set.l, ir_mk_rhs_triv(f)), c));
        }
      }
    } else {
      if (triv_is_mvar(i->set.l) && !very_trivial(i->set.r.triv)) {
        return set_and_replace_triv(c, &i->set.r.triv);
      } else {
        return c;
      }
    }
  case ir_instr_mset:
    if (!ir_rhs_is_triv(i->mset.r) ||
        !very_trivial(i->mset.r.triv)) {
      return select_many(set_and_replace_rhs(c, &i->mset.r));
    } else if (very_trivial(i->mset.base) && very_trivial(i->mset.offset) &&
               (!ir_triv_is_const(i->mset.base) || !ir_triv_is_const(i->mset.offset))) {
      return c;
    } else if (very_trivial(i->mset.base)) {
      return select_many(set_and_replace_triv(c, &i->mset.offset));
    } else {
      return select_many(set_and_replace_triv(c, &i->mset.base));
    }
  case ir_instr_jcc:
    if (triv_is_mvar(i->jcc.x) && triv_is_mvar(i->jcc.y)) {
      return set_and_replace_triv(c, &i->jcc.x);
    } else if (triv_is_bigint(i->jcc.x)) {
      return select_many(set_and_replace_triv(c, &i->jcc.x));
    } else if (triv_is_bigint(i->jcc.y)) {
      return select_many(set_and_replace_triv(c, &i->jcc.y));
    } else if (ir_triv_is_const(i->jcc.x) && ir_triv_is_const(i->jcc.y)) {
      return set_and_replace_triv(c, &i->jcc.x);
    } else if (ir_triv_is_const(i->jcc.x)) {
      i->jcc.op = flip_rel(i->jcc.op);
      ir_triv t = i->jcc.x;
      i->jcc.x = i->jcc.y;
      i->jcc.y = t;
      return c;
    } else {
      return c;
    }
  case ir_instr_call:
  case ir_instr_tcall:
    if (!ir_rhs_is_triv(i->call.f) ||
        !ir_triv_is_reg(i->call.f.triv)) {
      return set_and_replace_rhs(c, &i->call.f);
    } else {
      return c;
    }
  case ir_instr_label:
  case ir_instr_jmp:
  case ir_instr_ret:
    return c;
  }
}

static ir_code select_many(ir_code c) {
  if (c == ir_code_nil()) {
    return ir_code_nil();
  } else {
    ir_code next = c->next;
    c->next = ir_code_nil();
    c = select_one(c);
    next = select_many(next);

    ir_code i;
    for (i = c; i->next != ir_code_nil(); i = i->next)
      ;
    i->next = next;

    return c;
  }
}

ir_code select_x86_64(ir_code c, set *u) {
  unspillable = u;
  return select_many(c);
}
