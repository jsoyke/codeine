#ifndef BUFFER_H_
#define BUFFER_H_

#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace codeine {

class Buf {
public:
  typedef std::vector<std::string> Lines;

  struct Options {
    bool auto_indent;
    int tab_stop;
    bool use_tabs;
  };

  struct Pos {
    int row;
    int col;
    Pos(int r, int c)
      : row(r)
      , col(c) { }
  };

  class Storage {
  public:
    virtual ~Storage() { }
    virtual bool read(Lines*) = 0;
    virtual bool save(const Lines&) = 0;
  };

  Buf(Options, Storage* = NULL);
  ~Buf();

  void erase(int);
  void insert_char(char);
  void insert_string(const string&);
  void insert_newline();
  void insert_tab();
  void move_cursor_horiz(int);
  void move_cursor_vert(int);
  void redo();
  void undo();

  struct _Impl;
private:
  std::unique_ptr<_Impl> _storage;
};

} // namespace codeine

#endif // BUFFER_H_
