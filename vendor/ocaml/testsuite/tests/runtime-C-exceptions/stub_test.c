#include <string.h>
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/mlvalues.h"
#include "caml/fail.h"

char *some_dynamic_string_that_should_be_freed()
{
    return strdup("bar");
}

CAMLexport value dynamic_invalid_argument(value unit)
{
    CAMLparam1(unit);
    char *dynamic_msg = some_dynamic_string_that_should_be_freed();
    value msg = caml_copy_string(dynamic_msg);
    free(dynamic_msg);
    caml_invalid_argument_value(msg);
    CAMLnoreturn;
}
