#include "color.h"
#include <stdlib.h>
#include <stdio.h>

/* assign homes */

static set all_register;
static set unspillable;
static hashtbl *fv_c;
static hashtbl *fv_a;

static hashtbl assignment;
static set spill;

static uint64_t embed_uvar(uvar_id uv) {
  return uv + ir_reg_count;
}

static bool embed_is_uvar(uint64_t x) {
  return x >= ir_reg_count;
}

static uvar_id extract_uvar(uint64_t x) {
  return x - ir_reg_count;
}

static void remove_node(hashtbl *r_c, uvar_id uv) {
  hashtbl_remove(r_c, uv);
  for (struct hashtbl_blist *i = r_c->top; i != NULL; i = i->down) {
    set c = (set)i->val;
    i->val = (uint64_t)set_remove(c, embed_uvar(uv));
  }
}

static set get_avail(set c) {
  set ok = all_register;
  for (; c != set_empty(); c = c->next) {
    if (embed_is_uvar(c->x)) {
      ir_reg *maybe_home = (ir_reg *)hashtbl_find(&assignment, extract_uvar(c->x));
      if (maybe_home != NULL) {
        ok = set_remove(ok, *maybe_home);
      }
    } else {
      ok = set_remove(ok, c->x);
    }
  }
  return ok;
}

void error_toolong() {
  fprintf(stderr, "register allocator: unspillable variables have long live range");
  exit(1);
}

static void assign(hashtbl *r_c) {
  if (r_c->top == NULL)
    return;

  for (struct hashtbl_blist *i = r_c->top; i != NULL; i = i->down) {
    uvar_id uv = i->key;
    set c = (set)i->val;
    if (set_size(c) < ir_reg_count) {
      remove_node(r_c, uv);
      assign(r_c);
      set a = get_avail(c);
      if (a != set_empty()) {
        hashtbl_add(&assignment, uv, a->x);
      } else {
        spill = set_add_ex(uv, spill);
      }
      return;
    }
  }

  for (struct hashtbl_blist *i = r_c->top; i != NULL; i = i->down) {
    uvar_id uv = i->key;
    set c = (set)i->val;
    if (!set_in(uv, unspillable)) {
      remove_node(r_c, uv);
      assign(r_c);
      set a = get_avail(c);
      if (a != set_empty()) {
        hashtbl_add(&assignment, uv, a->x);
      } else {
        spill = set_add_ex(uv, spill);
      }
      return;
    }
  }

  error_toolong();
}

void assign_fv(set spill) {
  for (set i = spill; i != set_empty(); i = i->next) {
    uvar_id uv = i->x;
    set c = (set)*hashtbl_find(fv_c, uv);
    set used = set_empty();
    for (set j = c; j != set_empty(); j = j->next) {
      if (embed_is_uvar(j->x)) {
        fvar_pos *maybe_pos = hashtbl_find(fv_a, extract_uvar(j->x));
        if (maybe_pos != NULL) {
          used = set_add(*maybe_pos, used);
        }
      }
    }
    for (fvar_pos p = 1; ; p++) {
      if (!set_in(p, used)) {
        hashtbl_add(fv_a, uv, p);
        break;
      }
    }
  }
}

/* assign homes end */

/* finalizing */

static void rep_triv_fv(ir_triv *tr) {
  switch (tr->type) {
  case ir_triv_uvar: {
    fvar_pos *maybe_pos = hashtbl_find(fv_a, tr->uv);
    if (maybe_pos != NULL) {
      *tr = ir_mk_triv_fvar(*maybe_pos);
    }
  }
  case ir_triv_const:
  case ir_triv_reg:
  case ir_triv_fvar:
  case ir_triv_svar:
    break;
  }
}

static void rep_triv_reg(ir_triv *tr) {
  switch (tr->type) {
  case ir_triv_uvar: {
    ir_reg *maybe_reg = (ir_reg *)hashtbl_find(&assignment, tr->uv);
    if (maybe_reg != NULL) {
      *tr = ir_mk_triv_reg(*maybe_reg);
    } else {
      *tr = ir_mk_triv_reg(all_register->x);
    }
  }
  case ir_triv_const:
  case ir_triv_reg:
  case ir_triv_fvar:
  case ir_triv_svar:
    break;
  }
}

static void rep_rhs_fv(ir_rhs *r) {
  switch (r->type) {
  case ir_rhs_triv:
    return rep_triv_fv(&r->triv);
  case ir_rhs_bin:
    rep_triv_fv(&r->bin.x);
    return rep_triv_fv(&r->bin.y);
  case ir_rhs_mref:
    rep_triv_fv(&r->mref.base);
    return rep_triv_fv(&r->mref.offset);
  }
}

static void rep_rhs_reg(ir_rhs *r) {
  switch (r->type) {
  case ir_rhs_triv:
    return rep_triv_reg(&r->triv);
  case ir_rhs_bin:
    rep_triv_reg(&r->bin.x);
    return rep_triv_reg(&r->bin.y);
  case ir_rhs_mref:
    rep_triv_reg(&r->mref.base);
    return rep_triv_reg(&r->mref.offset);
  }
}

static void rep_instr_fv(ir_instr *i) {
  switch (i->type) {
  case ir_instr_set:
    rep_triv_fv(&i->set.l);
    return rep_rhs_fv(&i->set.r);
  case ir_instr_mset:
    rep_triv_fv(&i->mset.base);
    rep_triv_fv(&i->mset.offset);
    return rep_rhs_fv(&i->mset.r);
  case ir_instr_label:
  case ir_instr_jmp:
    break;
  case ir_instr_jcc:
    rep_triv_fv(&i->jcc.x);
    return rep_triv_fv(&i->jcc.y);
  case ir_instr_call:
    return rep_rhs_fv(&i->call.f);
  case ir_instr_tcall:
    return rep_rhs_fv(&i->call.f);
  case ir_instr_ret:
    break;
  }
}

static void rep_instr_reg(ir_instr *i) {
  switch (i->type) {
  case ir_instr_set:
    rep_triv_reg(&i->set.l);
    return rep_rhs_reg(&i->set.r);
  case ir_instr_mset:
    rep_triv_reg(&i->mset.base);
    rep_triv_reg(&i->mset.offset);
    return rep_rhs_reg(&i->mset.r);
  case ir_instr_label:
  case ir_instr_jmp:
    break;
  case ir_instr_jcc:
    rep_triv_reg(&i->jcc.x);
    return rep_triv_reg(&i->jcc.y);
  case ir_instr_call:
    return rep_rhs_reg(&i->call.f);
  case ir_instr_tcall:
    return rep_rhs_reg(&i->call.f);
  case ir_instr_ret:
    break;
  }
}

/* finalizing end */

bool color(set u, hashtbl *fc, hashtbl *fa, hashtbl *c, ir_code code) {
  all_register = set_empty();
  for (int i = ir_reg_count; i > 0; i--) {
    all_register = set_add_ex(i - 1, all_register);
  }
  unspillable = u;
  fv_c = fc;
  fv_a = fa;
  init_hashtbl(&assignment);
  spill = set_empty();

  assign(c);
  bool f;
  if (spill == set_empty()) {
    f = true;
    for (ir_code i = code; i != ir_code_nil(); i = i->next) {
      rep_instr_reg(&i->i);
    }
  } else {
    f = false;
    for (ir_code i = code; i != ir_code_nil(); i = i->next) {
      rep_instr_fv(&i->i);
    }
  }

  hashtbl_clear(&assignment);
  return f;
}
