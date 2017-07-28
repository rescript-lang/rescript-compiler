/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Alain Frisch, projet Gallium, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2007 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "stack.h"
#include "caml/callback.h"
#include "caml/alloc.h"
#include "caml/intext.h"
#include "caml/osdeps.h"
#include "caml/fail.h"
#include "caml/signals.h"

#include <stdio.h>
#include <string.h>

static void *getsym(void *handle, char *module, char *name){
  char *fullname = caml_strconcat(3, "caml", module, name);
  void *sym;
  sym = caml_dlsym (handle, fullname);
  /*  printf("%s => %lx\n", fullname, (uintnat) sym); */
  caml_stat_free(fullname);
  return sym;
}

extern char caml_globals_map[];

CAMLprim value caml_natdynlink_getmap(value unit)
{
  return (value)caml_globals_map;
}

CAMLprim value caml_natdynlink_globals_inited(value unit)
{
  return Val_int(caml_globals_inited);
}

CAMLprim value caml_natdynlink_open(value filename, value global)
{
  CAMLparam1 (filename);
  CAMLlocal1 (res);
  void *sym;
  void *handle;
  char *p;

  /* TODO: dlclose in case of error... */

  p = caml_strdup(String_val(filename));
  caml_enter_blocking_section();
  handle = caml_dlopen(p, 1, Int_val(global));
  caml_leave_blocking_section();
  caml_stat_free(p);

  if (NULL == handle)
    CAMLreturn(caml_copy_string(caml_dlerror()));

  sym = caml_dlsym(handle, "caml_plugin_header");
  if (NULL == sym)
    CAMLreturn(caml_copy_string("not an OCaml plugin"));

  res = caml_alloc_tuple(2);
  Field(res, 0) = (value) handle;
  Field(res, 1) = (value) (sym);
  CAMLreturn(res);
}

CAMLprim value caml_natdynlink_run(void *handle, value symbol) {
  CAMLparam1 (symbol);
  CAMLlocal1 (result);
  void *sym,*sym2;
  struct code_fragment * cf;

#define optsym(n) getsym(handle,unit,n)
  char *unit;
  void (*entrypoint)(void);

  unit = String_val(symbol);

  sym = optsym("__frametable");
  if (NULL != sym) caml_register_frametable(sym);

  sym = optsym("");
  if (NULL != sym) caml_register_dyn_global(sym);

  sym = optsym("__data_begin");
  sym2 = optsym("__data_end");
  if (NULL != sym && NULL != sym2)
    caml_page_table_add(In_static_data, sym, sym2);

  sym = optsym("__code_begin");
  sym2 = optsym("__code_end");
  if (NULL != sym && NULL != sym2) {
    caml_page_table_add(In_code_area, sym, sym2);
    cf = caml_stat_alloc(sizeof(struct code_fragment));
    cf->code_start = (char *) sym;
    cf->code_end = (char *) sym2;
    cf->digest_computed = 0;
    caml_ext_table_add(&caml_code_fragments_table, cf);
  }

  entrypoint = optsym("__entry");
  if (NULL != entrypoint) result = caml_callback((value)(&entrypoint), 0);
  else result = Val_unit;

#undef optsym

  CAMLreturn (result);
}

CAMLprim value caml_natdynlink_run_toplevel(value filename, value symbol)
{
  CAMLparam2 (filename, symbol);
  CAMLlocal2 (res, v);
  void *handle;
  char *p;

  /* TODO: dlclose in case of error... */

  p = caml_strdup(String_val(filename));
  caml_enter_blocking_section();
  handle = caml_dlopen(p, 1, 1);
  caml_leave_blocking_section();
  caml_stat_free(p);

  if (NULL == handle) {
    res = caml_alloc(1,1);
    v = caml_copy_string(caml_dlerror());
    Store_field(res, 0, v);
  } else {
    res = caml_alloc(1,0);
    v = caml_natdynlink_run(handle, symbol);
    Store_field(res, 0, v);
  }
  CAMLreturn(res);
}

CAMLprim value caml_natdynlink_loadsym(value symbol)
{
  CAMLparam1 (symbol);
  CAMLlocal1 (sym);

  sym = (value) caml_globalsym(String_val(symbol));
  if (!sym) caml_failwith(String_val(symbol));
  CAMLreturn(sym);
}
