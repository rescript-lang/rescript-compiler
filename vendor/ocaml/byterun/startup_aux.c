/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Some runtime initialization functions that are common to bytecode
   and native code. */

#include <stdio.h>
#include "caml/backtrace.h"
#include "caml/memory.h"
#include "caml/callback.h"
#include "caml/major_gc.h"
#ifndef NATIVE_CODE
#include "caml/dynlink.h"
#endif
#include "caml/osdeps.h"
#include "caml/startup_aux.h"


/* Initialize the atom table */

CAMLexport header_t caml_atom_table[256];
void caml_init_atom_table(void)
{
  int i;
  for(i = 0; i < 256; i++) {
#ifdef NATIVE_CODE
    caml_atom_table[i] = Make_header_allocated_here(0, i, Caml_white);
#else
    caml_atom_table[i] = Make_header(0, i, Caml_white);
#endif
  }
  if (caml_page_table_add(In_static_data,
                          caml_atom_table, caml_atom_table + 256) != 0) {
    caml_fatal_error("Fatal error: not enough memory for initial page table");
  }
}


/* Parse the OCAMLRUNPARAM environment variable. */

uintnat caml_init_percent_free = Percent_free_def;
uintnat caml_init_max_percent_free = Max_percent_free_def;
uintnat caml_init_minor_heap_wsz = Minor_heap_def;
uintnat caml_init_heap_chunk_sz = Heap_chunk_def;
uintnat caml_init_heap_wsz = Init_heap_def;
uintnat caml_init_max_stack_wsz = Max_stack_def;
uintnat caml_init_major_window = Major_window_def;
extern int caml_parser_trace;
uintnat caml_trace_level = 0;
uintnat caml_cleanup_on_exit = 0;


static void scanmult (char_os *opt, uintnat *var)
{
  char_os mult = _T(' ');
  unsigned int val = 1;
  sscanf_os (opt, _T("=%u%c"), &val, &mult);
  sscanf_os (opt, _T("=0x%x%c"), &val, &mult);
  switch (mult) {
  case _T('k'):   *var = (uintnat) val * 1024; break;
  case _T('M'):   *var = (uintnat) val * (1024 * 1024); break;
  case _T('G'):   *var = (uintnat) val * (1024 * 1024 * 1024); break;
  default:    *var = (uintnat) val; break;
  }
}

void caml_parse_ocamlrunparam(void)
{
  char_os *opt = caml_secure_getenv (_T("OCAMLRUNPARAM"));
  uintnat p;

  if (opt == NULL) opt = caml_secure_getenv (_T("CAMLRUNPARAM"));

  if (opt != NULL){
    while (*opt != _T('\0')){
      switch (*opt++){
      case _T('a'): scanmult (opt, &p); caml_set_allocation_policy (p); break;
      case _T('b'): scanmult (opt, &p); caml_record_backtrace(Val_bool (p)); break;
      case _T('c'): scanmult (opt, &p); caml_cleanup_on_exit = p; break;
      case _T('h'): scanmult (opt, &caml_init_heap_wsz); break;
      case _T('H'): scanmult (opt, &caml_use_huge_pages); break;
      case _T('i'): scanmult (opt, &caml_init_heap_chunk_sz); break;
      case _T('l'): scanmult (opt, &caml_init_max_stack_wsz); break;
      case _T('o'): scanmult (opt, &caml_init_percent_free); break;
      case _T('O'): scanmult (opt, &caml_init_max_percent_free); break;
      case _T('p'): scanmult (opt, &p); caml_parser_trace = p; break;
      case _T('R'): break; /*  see stdlib/hashtbl.mli */
      case _T('s'): scanmult (opt, &caml_init_minor_heap_wsz); break;
      case _T('t'): scanmult (opt, &caml_trace_level); break;
      case _T('v'): scanmult (opt, &caml_verb_gc); break;
      case _T('w'): scanmult (opt, &caml_init_major_window); break;
      case _T('W'): scanmult (opt, &caml_runtime_warnings); break;
      }
      while (*opt != _T('\0')){
        if (*opt++ == ',') break;
      }
    }
  }
}


/* The number of outstanding calls to caml_startup */
static int startup_count = 0;

/* Has the runtime been shut down already? */
static int shutdown_happened = 0;


int caml_startup_aux(int pooling)
{
  if (shutdown_happened == 1)
    caml_fatal_error("Fatal error: caml_startup was called after the runtime "
                     "was shut down with caml_shutdown");

  /* Second and subsequent calls are ignored,
     since the runtime has already started */
  startup_count++;
  if (startup_count > 1)
    return 0;

  if (pooling)
    caml_stat_create_pool();

  return 1;
}

static void call_registered_value(char* name)
{
  value *f = caml_named_value(name);
  if (f != NULL)
    caml_callback_exn(*f, Val_unit);
}

CAMLexport void caml_shutdown(void)
{
  if (startup_count <= 0)
    caml_fatal_error("Fatal error: a call to caml_shutdown has no "
                     "corresponding call to caml_startup");

  /* Do nothing unless it's the last call remaining */
  startup_count--;
  if (startup_count > 0)
    return;

  call_registered_value("Pervasives.do_at_exit");
  call_registered_value("Thread.at_shutdown");
  caml_finalise_heap();
#ifndef NATIVE_CODE
  caml_free_shared_libs();
#endif
  caml_stat_destroy_pool();

  shutdown_happened = 1;
}
