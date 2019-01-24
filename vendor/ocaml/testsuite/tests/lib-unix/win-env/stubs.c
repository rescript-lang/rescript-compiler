#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/osdeps.h>

#include <windows.h>

CAMLprim value caml_SetEnvironmentVariable(value s1, value s2)
{
  WCHAR *w1, *w2;
  w1 = caml_stat_strdup_to_utf16(String_val(s1));
  w2 = caml_stat_strdup_to_utf16(String_val(s2));
  SetEnvironmentVariableW(w1, w2);
  caml_stat_free(w1);
  caml_stat_free(w2);
  return Val_unit;
}
