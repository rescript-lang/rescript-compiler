#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

CAMLprim value belt_makemutablelist(value a, value l) {
  CAMLparam2(a, l);
  CAMLlocal1(box);
  box = caml_alloc_small(2, 0);
  Field(box, 0) = a;
  Field(box, 1) = l;
  CAMLreturn(box);
}
