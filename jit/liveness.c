#include "liveness.h"

/* fatal constraint: only forward jump allowed */

/* locate labels */

static hashtbl label_to_code;

static void collect_label(ir_code c) {
  for (; c != ir_code_nil(); c = c->next) {
    if (c->i.type == ir_instr_label) {
      hashtbl_add(&label_to_code, c->i.label, (uint64_t)c);
    }
  }
}

static ir_code locate_label(label_id lab) {
  return (ir_code)*hashtbl_find(&label_to_code, lab);
}

/* locate labels end */

/* liveness analysis */

#define ms_register ir_rbx
#define Carg1       ir_rdi

static hashtbl *conflict;

static uint64_t embed_uvar(uvar_id uv) {
  return uv + ir_reg_count;
}

static bool embed_is_uvar(uint64_t x) {
  return x >= ir_reg_count;
}

static uvar_id extract_uvar(uint64_t x) {
  return x - ir_reg_count;
}

static set add_triv(ir_triv tr, set s) {
  switch (tr.type) {
  case ir_triv_const: return s;
  case ir_triv_uvar:  return set_add(embed_uvar(tr.uv), s);
  case ir_triv_reg:   return set_add(tr.reg, s);
  case ir_triv_fvar:  return s;
  case ir_triv_svar:  return set_add(ms_register, s);
  }
}

static set remove_triv(ir_triv tr, set s) {
  switch (tr.type) {
  case ir_triv_const: return s;
  case ir_triv_uvar:  return set_remove(s, embed_uvar(tr.uv));
  case ir_triv_reg:   return set_remove(s, tr.reg);
  case ir_triv_fvar:  return s;
  case ir_triv_svar:  return s;
  }
}

static set add_rhs(ir_rhs r, set s) {
  switch (r.type) {
  case ir_rhs_triv: return add_triv(r.triv, s);
  case ir_rhs_bin:  return add_triv(r.bin.x, add_triv(r.bin.y, s));
  case ir_rhs_mref: return add_triv(r.mref.base, add_triv(r.mref.offset, s));
  }
}

static void add_conflict(ir_triv v, set live) {
  if (ir_triv_is_uvar(v)) {
    set *s = (set *)hashtbl_find_or_add(conflict, v.uv, (uint64_t)set_empty());
    *s = set_union(live, *s);
  }
  uint64_t repr;
  if (ir_triv_is_uvar(v)) {
    repr = embed_uvar(v.uv);
  } else if (ir_triv_is_reg(v)) {
    repr = v.reg;
  } else {
    return;
  }
  for (set i = live; i != set_empty(); i = i->next) {
    if (embed_is_uvar(i->x)) {
      set *s = (set *)hashtbl_find_or_add(conflict, extract_uvar(i->x), (uint64_t)set_empty());
      *s = set_add(repr, *s);
    }
  }
}

static set trans(ir_instr *i, set s) {
  switch (i->type) {
  case ir_instr_set:
    switch (i->set.r.type) {
    case ir_rhs_triv:
      s = remove_triv(i->set.l, s);
      add_conflict(i->set.l, remove_triv(i->set.r.triv, s));
      return add_rhs(i->set.r, s);
    case ir_rhs_bin:
    case ir_rhs_mref:
      s = remove_triv(i->set.l, s);
      add_conflict(i->set.l, s);
      return add_rhs(i->set.r, s);
    }
  case ir_instr_mset:
    return
      add_triv(i->mset.base,
      add_triv(i->mset.offset,
      add_rhs(i->mset.r, s)));
  case ir_instr_label:
    return s;
  case ir_instr_jmp:
    return locate_label(i->jmp.dest)->live;
  case ir_instr_jcc:
    return
      add_triv(i->jcc.x,
      add_triv(i->jcc.y,
      set_union(locate_label(i->jcc.dest)->live, s)));
  case ir_instr_call:
  case ir_instr_tcall:
    /* we assume all registers are caller-saved */
    return i->call.live;
  case ir_instr_ret:
    return set_empty();
  }
}

/* liveness analysis end */

/* c != nil */
static void rec(ir_code c) {
  if (c->next != ir_code_nil()) {
    rec(c->next);
    c->live = trans(&c->next->i, c->next->live);
  } else {
    c->live = set_empty(); /* dummy */
  }
}

void uncover_conflict(ir_code c, hashtbl *ret) {
  init_hashtbl(&label_to_code);
  collect_label(c);

  conflict = ret;
  for (ir_code i = c; i != ir_code_nil(); i = i->next) {
    i->live = set_empty();
  }
  rec(c);

  hashtbl_clear(&label_to_code);
}
