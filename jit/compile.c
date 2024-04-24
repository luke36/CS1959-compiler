#include "compile.h"
#include "genir.h"
#include "select.h"
#include "liveness.h"
#include "color.h"
#include "gen_x86_64.h"

char *compile(instruction_t *start, char *cp) {
  ir_code ir = gen_ir(start);

  hashtbl frame_conflict;
  hashtbl frame_assignment;
  init_hashtbl(&frame_conflict);
  init_hashtbl(&frame_assignment);
  set u = set_empty();

  uncover_conflict(ir, &frame_conflict);
  do {
    ir = select_x86_64(ir, &u);
    hashtbl register_conflict;
    init_hashtbl(&register_conflict);
    uncover_conflict(ir, &register_conflict);
    if (color(u, &frame_conflict, &frame_assignment, &register_conflict, ir))
      break;
    hashtbl_clear(&register_conflict);
  } while (1);
  char *p = gen_x86_64(ir, cp);

  hashtbl_clear(&frame_conflict);
  hashtbl_clear(&frame_assignment);

  return p;
}
