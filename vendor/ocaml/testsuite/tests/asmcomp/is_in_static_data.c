#include "caml/address_class.h"

value caml_is_in_static_data(value v) {
  return(Val_bool(Is_in_static_data(v)));
}
