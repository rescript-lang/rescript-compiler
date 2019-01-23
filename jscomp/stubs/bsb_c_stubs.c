#include "caml/alloc.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"

#ifdef __MINGW32__

CAMLprim value bsb_uname() {
  CAMLparam0();
  CAMLlocal1(ret);
  ret = caml_alloc_small(1, 0);
  Field(ret, 0) = caml_copy_string("Windows");
  CAMLreturn(ret);
}

#else

#include <sys/utsname.h>

CAMLprim value bsb_uname() {
  CAMLparam0();
  CAMLlocal1(ret);
  
  struct utsname unameData;
  
  int err = uname(&unameData);
  if (err) {
    CAMLreturn(Val_int(0));
  }
  
  ret = caml_alloc_small(1, 0);
  Field(ret, 0) = caml_copy_string(unameData.sysname);
  CAMLreturn(ret);
}


#endif
