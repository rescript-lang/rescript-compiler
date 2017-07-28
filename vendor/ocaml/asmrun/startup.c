/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Start-up code */

#include <stdio.h>
#include <stdlib.h>
#include "caml/callback.h"
#include "caml/backtrace.h"
#include "caml/custom.h"
#include "caml/debugger.h"
#include "caml/fail.h"
#include "caml/freelist.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/intext.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/printexc.h"
#include "stack.h"
#include "caml/sys.h"
#ifdef HAS_UI
#include "caml/ui.h"
#endif

extern int caml_parser_trace;
CAMLexport header_t caml_atom_table[256];
char * caml_code_area_start, * caml_code_area_end;

/* Initialize the atom table and the static data and code area limits. */

struct segment { char * begin; char * end; };

static void init_atoms(void)
{
  extern struct segment caml_data_segments[], caml_code_segments[];
  int i;
  struct code_fragment * cf;

  for (i = 0; i < 256; i++) {
    caml_atom_table[i] = Make_header(0, i, Caml_white);
  }
  if (caml_page_table_add(In_static_data,
                          caml_atom_table, caml_atom_table + 256) != 0)
    caml_fatal_error("Fatal error: not enough memory for initial page table");

  for (i = 0; caml_data_segments[i].begin != 0; i++) {
    /* PR#5509: we must include the zero word at end of data segment,
       because pointers equal to caml_data_segments[i].end are static data. */
    if (caml_page_table_add(In_static_data,
                            caml_data_segments[i].begin,
                            caml_data_segments[i].end + sizeof(value)) != 0)
      caml_fatal_error("Fatal error: not enough memory for initial page table");
  }

  caml_code_area_start = caml_code_segments[0].begin;
  caml_code_area_end = caml_code_segments[0].end;
  for (i = 1; caml_code_segments[i].begin != 0; i++) {
    if (caml_code_segments[i].begin < caml_code_area_start)
      caml_code_area_start = caml_code_segments[i].begin;
    if (caml_code_segments[i].end > caml_code_area_end)
      caml_code_area_end = caml_code_segments[i].end;
  }
  /* Register the code in the table of code fragments */
  cf = caml_stat_alloc(sizeof(struct code_fragment));
  cf->code_start = caml_code_area_start;
  cf->code_end = caml_code_area_end;
  cf->digest_computed = 0;
  caml_ext_table_init(&caml_code_fragments_table, 8);
  caml_ext_table_add(&caml_code_fragments_table, cf);
}

/* Configuration parameters and flags */

static uintnat percent_free_init = Percent_free_def;
static uintnat max_percent_free_init = Max_percent_free_def;
static uintnat minor_heap_init = Minor_heap_def;
static uintnat heap_chunk_init = Heap_chunk_def;
static uintnat heap_size_init = Init_heap_def;
static uintnat max_stack_init = Max_stack_def;

/* Parse the CAMLRUNPARAM variable */
/* The option letter for each runtime option is the first letter of the
   last word of the ML name of the option (see [stdlib/gc.mli]).
   Except for l (maximum stack size) and h (initial heap size).
*/
/* Note: option l is irrelevant to the native-code runtime. */

/* If you change these functions, see also their copy in byterun/startup.c */

static void scanmult (char *opt, uintnat *var)
{
  char mult = ' ';
  int val;
  sscanf (opt, "=%u%c", &val, &mult);
  sscanf (opt, "=0x%x%c", &val, &mult);
  switch (mult) {
  case 'k':   *var = (uintnat) val * 1024; break;
  case 'M':   *var = (uintnat) val * 1024 * 1024; break;
  case 'G':   *var = (uintnat) val * 1024 * 1024 * 1024; break;
  default:    *var = (uintnat) val; break;
  }
}

static void parse_camlrunparam(void)
{
  char *opt = getenv ("OCAMLRUNPARAM");
  uintnat p;

  if (opt == NULL) opt = getenv ("CAMLRUNPARAM");

  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      case 's': scanmult (opt, &minor_heap_init); break;
      case 'i': scanmult (opt, &heap_chunk_init); break;
      case 'h': scanmult (opt, &heap_size_init); break;
      case 'l': scanmult (opt, &max_stack_init); break;
      case 'o': scanmult (opt, &percent_free_init); break;
      case 'O': scanmult (opt, &max_percent_free_init); break;
      case 'v': scanmult (opt, &caml_verb_gc); break;
      case 'b': caml_record_backtrace(Val_true); break;
      case 'p': caml_parser_trace = 1; break;
      case 'a': scanmult (opt, &p); caml_set_allocation_policy (p); break;
      }
    }
  }
}

/* These are termination hooks used by the systhreads library */
struct longjmp_buffer caml_termination_jmpbuf;
void (*caml_termination_hook)(void *) = NULL;

extern value caml_start_program (void);
extern void caml_init_ieee_floats (void);
extern void caml_init_signals (void);

#ifdef _MSC_VER

/* PR 4887: avoid crash box of windows runtime on some system calls */
extern void caml_install_invalid_parameter_handler();

#endif


void caml_main(char **argv)
{
  char * exe_name;
  static char proc_self_exe[256];
  value res;
  char tos;

  caml_init_ieee_floats();
#ifdef _MSC_VER
  caml_install_invalid_parameter_handler();
#endif
  caml_init_custom_operations();
#ifdef DEBUG
  caml_verb_gc = 63;
#endif
  caml_top_of_stack = &tos;
  parse_camlrunparam();
  caml_init_gc (minor_heap_init, heap_size_init, heap_chunk_init,
                percent_free_init, max_percent_free_init);
  init_atoms();
  caml_init_signals();
  caml_debugger_init (); /* force debugger.o stub to be linked */
  exe_name = argv[0];
  if (exe_name == NULL) exe_name = "";
  if (caml_executable_name(proc_self_exe, sizeof(proc_self_exe)) == 0)
    exe_name = proc_self_exe;
  else
    exe_name = caml_search_exe_in_path(exe_name);
  caml_sys_init(exe_name, argv);
  if (sigsetjmp(caml_termination_jmpbuf.buf, 0)) {
    if (caml_termination_hook != NULL) caml_termination_hook(NULL);
    return;
  }
  res = caml_start_program();
  if (Is_exception_result(res))
    caml_fatal_uncaught_exception(Extract_exception(res));
}

void caml_startup(char **argv)
{
  caml_main(argv);
}
