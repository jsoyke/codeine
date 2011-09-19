#ifndef BUFFER_H_
#define BUFFER_H_

#include <stdlib.h>
#include <stdbool.h>

typedef struct {
  bool auto_indent;
  int tab_stop;
  bool use_tabs;
} buf_opt_t;

typedef struct {
  int x, y;
} buf_pos_t;

typedef struct buf {
  buf_options_t options;
  size_t num_lines;
  char **lines;
  buf_pos_t cursor_pos;
  buf_pos_t _last_edit_pos;
  struct buf *_next_state;
  struct buf *_prev_state;
} buf_t;

buf_t *buf_create(buf_opt_t opts, const char *text);

void buf_destroy(buf_t *buf);

buf_t *buf_undo(buf_t *buf);

buf_t *buf_redo(buf_t *buf);

buf_t *buf_insert_tab(buf_t *buf);

#endif // BUFFER_H_
