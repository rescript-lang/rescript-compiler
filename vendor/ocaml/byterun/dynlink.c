/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Dynamic loading of C primitives. */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "caml/config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include "caml/alloc.h"
#include "caml/dynlink.h"
#include "caml/fail.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/osdeps.h"
#include "caml/prims.h"
#include "caml/signals.h"

#ifndef NATIVE_CODE

/* The table of primitives */
struct ext_table caml_prim_table;

#ifdef DEBUG
/* The names of primitives (for instrtrace.c) */
struct ext_table caml_prim_name_table;
#endif

/* The table of shared libraries currently opened */
static struct ext_table shared_libs;

/* The search path for shared libraries */
struct ext_table caml_shared_libs_path;

/* Look up the given primitive name in the built-in primitive table,
   then in the opened shared libraries (shared_libs) */
static c_primitive lookup_primitive(char * name)
{
  int i;
  void * res;

  for (i = 0; caml_names_of_builtin_cprim[i] != NULL; i++) {
    if (strcmp(name, caml_names_of_builtin_cprim[i]) == 0)
      return caml_builtin_cprim[i];
  }
  for (i = 0; i < shared_libs.size; i++) {
    res = caml_dlsym(shared_libs.contents[i], name);
    if (res != NULL) return (c_primitive) res;
  }
  return NULL;
}

/* Parse the OCAML_STDLIB_DIR/ld.conf file and add the directories
   listed there to the search path */

#define LD_CONF_NAME "ld.conf"

static char * parse_ld_conf(void)
{
  char * stdlib, * ldconfname, * config, * p, * q;
  struct stat st;
  int ldconf, nread;

  stdlib = getenv("OCAMLLIB");
  if (stdlib == NULL) stdlib = getenv("CAMLLIB");
  if (stdlib == NULL) stdlib = OCAML_STDLIB_DIR;
  ldconfname = caml_strconcat(3, stdlib, "/", LD_CONF_NAME);
  if (stat(ldconfname, &st) == -1) {
    caml_stat_free(ldconfname);
    return NULL;
  }
  ldconf = open(ldconfname, O_RDONLY, 0);
  if (ldconf == -1)
    caml_fatal_error_arg("Fatal error: cannot read loader config file %s\n",
                         ldconfname);
  config = caml_stat_alloc(st.st_size + 1);
  nread = read(ldconf, config, st.st_size);
  if (nread == -1)
    caml_fatal_error_arg
      ("Fatal error: error while reading loader config file %s\n",
       ldconfname);
  config[nread] = 0;
  q = config;
  for (p = config; *p != 0; p++) {
    if (*p == '\n') {
      *p = 0;
      caml_ext_table_add(&caml_shared_libs_path, q);
      q = p + 1;
    }
  }
  if (q < p) caml_ext_table_add(&caml_shared_libs_path, q);
  close(ldconf);
  caml_stat_free(ldconfname);
  return config;
}

/* Open the given shared library and add it to shared_libs.
   Abort on error. */
static void open_shared_lib(char * name)
{
  char * realname;
  void * handle;

  realname = caml_search_dll_in_path(&caml_shared_libs_path, name);
  caml_gc_message(0x100, "Loading shared library %s\n",
                  (uintnat) realname);
  caml_enter_blocking_section();
  handle = caml_dlopen(realname, 1, 1);
  caml_leave_blocking_section();
  if (handle == NULL)
    caml_fatal_error_arg2("Fatal error: cannot load shared library %s\n", name,
                          "Reason: %s\n", caml_dlerror());
  caml_ext_table_add(&shared_libs, handle);
  caml_stat_free(realname);
}

/* Build the table of primitives, given a search path and a list
   of shared libraries (both 0-separated in a char array).
   Abort the runtime system on error. */
void caml_build_primitive_table(char * lib_path,
                                char * libs,
                                char * req_prims)
{
  char * tofree1, * tofree2;
  char * p;

  /* Initialize the search path for dynamic libraries:
     - directories specified on the command line with the -I option
     - directories specified in the CAML_LD_LIBRARY_PATH
     - directories specified in the executable
     - directories specified in the file <stdlib>/ld.conf */
  tofree1 = caml_decompose_path(&caml_shared_libs_path,
                                getenv("CAML_LD_LIBRARY_PATH"));
  if (lib_path != NULL)
    for (p = lib_path; *p != 0; p += strlen(p) + 1)
      caml_ext_table_add(&caml_shared_libs_path, p);
  tofree2 = parse_ld_conf();
  /* Open the shared libraries */
  caml_ext_table_init(&shared_libs, 8);
  if (libs != NULL)
    for (p = libs; *p != 0; p += strlen(p) + 1)
      open_shared_lib(p);
  /* Build the primitive table */
  caml_ext_table_init(&caml_prim_table, 0x180);
#ifdef DEBUG
  caml_ext_table_init(&caml_prim_name_table, 0x180);
#endif
  for (p = req_prims; *p != 0; p += strlen(p) + 1) {
    c_primitive prim = lookup_primitive(p);
    if (prim == NULL)
          caml_fatal_error_arg("Fatal error: unknown C primitive `%s'\n", p);
    caml_ext_table_add(&caml_prim_table, (void *) prim);
#ifdef DEBUG
    caml_ext_table_add(&caml_prim_name_table, strdup(p));
#endif
  }
  /* Clean up */
  caml_stat_free(tofree1);
  caml_stat_free(tofree2);
  caml_ext_table_free(&caml_shared_libs_path, 0);
}

/* Build the table of primitives as a copy of the builtin primitive table.
   Used for executables generated by ocamlc -output-obj. */

void caml_build_primitive_table_builtin(void)
{
  int i;
  caml_ext_table_init(&caml_prim_table, 0x180);
#ifdef DEBUG
  caml_ext_table_init(&caml_prim_name_table, 0x180);
#endif
  for (i = 0; caml_builtin_cprim[i] != 0; i++) {
    caml_ext_table_add(&caml_prim_table, (void *) caml_builtin_cprim[i]);
#ifdef DEBUG
    caml_ext_table_add(&caml_prim_name_table,
                       strdup(caml_names_of_builtin_cprim[i]));
#endif
}
}

#endif /* NATIVE_CODE */

/** dlopen interface for the bytecode linker **/

#define Handle_val(v) (*((void **) (v)))

CAMLprim value caml_dynlink_open_lib(value mode, value filename)
{
  void * handle;
  value result;
  char * p;

  caml_gc_message(0x100, "Opening shared library %s\n",
                  (uintnat) String_val(filename));
  p = caml_strdup(String_val(filename));
  caml_enter_blocking_section();
  handle = caml_dlopen(p, Int_val(mode), 1);
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (handle == NULL) caml_failwith(caml_dlerror());
  result = caml_alloc_small(1, Abstract_tag);
  Handle_val(result) = handle;
  return result;
}

CAMLprim value caml_dynlink_close_lib(value handle)
{
  caml_dlclose(Handle_val(handle));
  return Val_unit;
}

/*#include <stdio.h>*/
CAMLprim value caml_dynlink_lookup_symbol(value handle, value symbolname)
{
  void * symb;
  value result;
  symb = caml_dlsym(Handle_val(handle), String_val(symbolname));
  /* printf("%s = 0x%lx\n", String_val(symbolname), symb);
     fflush(stdout); */
  if (symb == NULL) return Val_unit /*caml_failwith(caml_dlerror())*/;
  result = caml_alloc_small(1, Abstract_tag);
  Handle_val(result) = symb;
  return result;
}

#ifndef NATIVE_CODE

CAMLprim value caml_dynlink_add_primitive(value handle)
{
  return Val_int(caml_ext_table_add(&caml_prim_table, Handle_val(handle)));
}

CAMLprim value caml_dynlink_get_current_libs(value unit)
{
  CAMLparam0();
  CAMLlocal1(res);
  int i;

  res = caml_alloc_tuple(shared_libs.size);
  for (i = 0; i < shared_libs.size; i++) {
    value v = caml_alloc_small(1, Abstract_tag);
    Handle_val(v) = shared_libs.contents[i];
    Store_field(res, i, v);
  }
  CAMLreturn(res);
}

#else

value caml_dynlink_add_primitive(value handle)
{
  caml_invalid_argument("dynlink_add_primitive");
  return Val_unit; /* not reached */
}

value caml_dynlink_get_current_libs(value unit)
{
  caml_invalid_argument("dynlink_get_current_libs");
  return Val_unit; /* not reached */
}

#endif /* NATIVE_CODE */
