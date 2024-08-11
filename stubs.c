#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <sys/ioctl.h>
#include <unistd.h>

CAMLprim value metaedit_terminal_size(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  struct winsize ws;
  int res = ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws);
  if (res < 0) {
    uerror("ioctl", Nothing);
  }
  result = caml_alloc(2, 0);
  Store_field(result, 0, Val_int(ws.ws_row));
  Store_field(result, 1, Val_int(ws.ws_col));
  CAMLreturn(result);
}
