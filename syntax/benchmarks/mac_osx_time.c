#include <caml/mlvalues.h>
#include <caml/alloc.h>
/* #include <caml/memory.h> */
#include <mach/mach_time.h>

static mach_timebase_info_data_t info;

CAMLprim value caml_mach_initialize(value unit) {
  mach_timebase_info(&info);
  return Val_unit;
}

CAMLprim value caml_mach_absolute_time(value unit) {
  uint64_t now = mach_absolute_time ();
  return caml_copy_int64((now * info.numer) / info.denom);
}
