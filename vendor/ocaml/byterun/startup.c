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

/* Start-up code */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include "caml/config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef _WIN32
#include <process.h>
#endif
#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/debugger.h"
#include "caml/dynlink.h"
#include "caml/exec.h"
#include "caml/fail.h"
#include "caml/fix_code.h"
#include "caml/freelist.h"
#include "caml/gc_ctrl.h"
#include "caml/instrtrace.h"
#include "caml/interp.h"
#include "caml/intext.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/prims.h"
#include "caml/printexc.h"
#include "caml/reverse.h"
#include "caml/signals.h"
#include "caml/stacks.h"
#include "caml/sys.h"
#include "caml/startup.h"
#include "caml/startup_aux.h"
#include "caml/version.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifndef SEEK_END
#define SEEK_END 2
#endif

/* Read the trailer of a bytecode file */

static void fixup_endianness_trailer(uint32_t * p)
{
#ifndef ARCH_BIG_ENDIAN
  Reverse_32(p, p);
#endif
}

static int read_trailer(int fd, struct exec_trailer *trail)
{
  if (lseek(fd, (long) -TRAILER_SIZE, SEEK_END) == -1)
    return BAD_BYTECODE;
  if (read(fd, (char *) trail, TRAILER_SIZE) < TRAILER_SIZE)
    return BAD_BYTECODE;
  fixup_endianness_trailer(&trail->num_sections);
  if (strncmp(trail->magic, EXEC_MAGIC, 12) == 0)
    return 0;
  else
    return BAD_BYTECODE;
}

int caml_attempt_open(char_os **name, struct exec_trailer *trail,
                      int do_open_script)
{
  char_os * truename;
  int fd;
  int err;
  char buf [2], * u8;

  truename = caml_search_exe_in_path(*name);
  u8 = caml_stat_strdup_of_os(truename);
  caml_gc_message(0x100, "Opening bytecode executable %s\n", u8);
  caml_stat_free(u8);
  fd = open_os(truename, O_RDONLY | O_BINARY);
  if (fd == -1) {
    caml_stat_free(truename);
    caml_gc_message(0x100, "Cannot open file\n");
    return FILE_NOT_FOUND;
  }
  if (!do_open_script) {
    err = read (fd, buf, 2);
    if (err < 2 || (buf [0] == '#' && buf [1] == '!')) {
      close(fd);
      caml_stat_free(truename);
      caml_gc_message(0x100, "Rejected #! script\n");
      return BAD_BYTECODE;
    }
  }
  err = read_trailer(fd, trail);
  if (err != 0) {
    close(fd);
    caml_stat_free(truename);
    caml_gc_message(0x100, "Not a bytecode executable\n");
    return err;
  }
  *name = truename;
  return fd;
}

/* Read the section descriptors */

void caml_read_section_descriptors(int fd, struct exec_trailer *trail)
{
  int toc_size, i;

  toc_size = trail->num_sections * 8;
  trail->section = caml_stat_alloc(toc_size);
  lseek(fd, - (long) (TRAILER_SIZE + toc_size), SEEK_END);
  if (read(fd, (char *) trail->section, toc_size) != toc_size)
    caml_fatal_error("Fatal error: cannot read section table\n");
  /* Fixup endianness of lengths */
  for (i = 0; i < trail->num_sections; i++)
    fixup_endianness_trailer(&(trail->section[i].len));
}

/* Position fd at the beginning of the section having the given name.
   Return the length of the section data in bytes, or -1 if no section
   found with that name. */

int32_t caml_seek_optional_section(int fd, struct exec_trailer *trail,
                                   char *name)
{
  long ofs;
  int i;

  ofs = TRAILER_SIZE + trail->num_sections * 8;
  for (i = trail->num_sections - 1; i >= 0; i--) {
    ofs += trail->section[i].len;
    if (strncmp(trail->section[i].name, name, 4) == 0) {
      lseek(fd, -ofs, SEEK_END);
      return trail->section[i].len;
    }
  }
  return -1;
}

/* Position fd at the beginning of the section having the given name.
   Return the length of the section data in bytes. */

int32_t caml_seek_section(int fd, struct exec_trailer *trail, char *name)
{
  int32_t len = caml_seek_optional_section(fd, trail, name);
  if (len == -1)
    caml_fatal_error_arg("Fatal_error: section `%s' is missing\n", name);
  return len;
}

/* Read and return the contents of the section having the given name.
   Add a terminating 0.  Return NULL if no such section. */

static char * read_section(int fd, struct exec_trailer *trail, char *name)
{
  int32_t len;
  char * data;

  len = caml_seek_optional_section(fd, trail, name);
  if (len == -1) return NULL;
  data = caml_stat_alloc(len + 1);
  if (read(fd, data, len) != len)
    caml_fatal_error_arg("Fatal error: error reading section %s\n", name);
  data[len] = 0;
  return data;
}

#ifdef _WIN32

static char_os * read_section_to_os(int fd, struct exec_trailer *trail, char *name)
{
  int32_t len, wlen;
  char * data;
  wchar_t * wdata;

  len = caml_seek_optional_section(fd, trail, name);
  if (len == -1) return NULL;
  data = caml_stat_alloc(len + 1);
  if (read(fd, data, len) != len)
    caml_fatal_error_arg("Fatal error: error reading section %s\n", name);
  data[len] = 0;
  wlen = win_multi_byte_to_wide_char(data, len, NULL, 0);
  wdata = caml_stat_alloc((wlen + 1)*sizeof(wchar_t));
  win_multi_byte_to_wide_char(data, len, wdata, wlen);
  wdata[wlen] = 0;
  caml_stat_free(data);
  return wdata;
}

#else

#define read_section_to_os read_section

#endif

/* Invocation of ocamlrun: 4 cases.

   1.  runtime + bytecode
       user types:  ocamlrun [options] bytecode args...
       arguments:  ocamlrun [options] bytecode args...

   2.  bytecode script
       user types:  bytecode args...
   2a  (kernel 1) arguments:  ocamlrun ./bytecode args...
   2b  (kernel 2) arguments:  bytecode bytecode args...

   3.  concatenated runtime and bytecode
       user types:  composite args...
       arguments:  composite args...

Algorithm:
  1-  If argument 0 is a valid byte-code file that does not start with #!,
      then we are in case 3 and we pass the same command line to the
      OCaml program.
  2-  In all other cases, we parse the command line as:
        (whatever) [options] bytecode args...
      and we strip "(whatever) [options]" from the command line.

*/

/* Parse options on the command line */

static int parse_command_line(char_os **argv)
{
  int i, j;

  for(i = 1; argv[i] != NULL && argv[i][0] == _T('-'); i++) {
    switch(argv[i][1]) {
    case _T('t'):
      ++ caml_trace_level; /* ignored unless DEBUG mode */
      break;
    case _T('v'):
      if (!strcmp_os (argv[i], _T("-version"))){
        printf ("The OCaml runtime, version " OCAML_VERSION_STRING "\n");
        exit (0);
      }else if (!strcmp_os (argv[i], _T("-vnum"))){
        printf (OCAML_VERSION_STRING "\n");
        exit (0);
      }else{
        caml_verb_gc = 0x001+0x004+0x008+0x010+0x020;
      }
      break;
    case _T('p'):
      for (j = 0; caml_names_of_builtin_cprim[j] != NULL; j++)
        printf("%s\n", caml_names_of_builtin_cprim[j]);
      exit(0);
      break;
    case _T('b'):
      caml_record_backtrace(Val_true);
      break;
    case _T('I'):
      if (argv[i + 1] != NULL) {
        caml_ext_table_add(&caml_shared_libs_path, argv[i + 1]);
        i++;
      }
      break;
    default:
      caml_fatal_error_arg("Unknown option %s.\n", caml_stat_strdup_of_os(argv[i]));
    }
  }
  return i;
}

extern void caml_init_ieee_floats (void);

#ifdef _WIN32
extern void caml_signal_thread(void * lpParam);
#endif

#if defined(_MSC_VER) && __STDC_SECURE_LIB__ >= 200411L

/* PR 4887: avoid crash box of windows runtime on some system calls */
extern void caml_install_invalid_parameter_handler();

#endif

extern int caml_ensure_spacetime_dot_o_is_included;

/* Main entry point when loading code from a file */

CAMLexport void caml_main(char_os **argv)
{
  int fd, pos;
  struct exec_trailer trail;
  struct channel * chan;
  value res;
  char * req_prims;
  char_os * shared_lib_path, * shared_libs;
  char_os * exe_name, * proc_self_exe;

  caml_ensure_spacetime_dot_o_is_included++;

  /* Determine options */
#ifdef DEBUG
  caml_verb_gc = 0x3F;
#endif
  caml_parse_ocamlrunparam();
#ifdef DEBUG
  caml_gc_message (-1, "### OCaml runtime: debug mode ###\n");
#endif
  if (!caml_startup_aux(/* pooling */ caml_cleanup_on_exit))
    return;

  /* Machine-dependent initialization of the floating-point hardware
     so that it behaves as much as possible as specified in IEEE */
  caml_init_ieee_floats();
#if defined(_MSC_VER) && __STDC_SECURE_LIB__ >= 200411L
  caml_install_invalid_parameter_handler();
#endif
  caml_init_custom_operations();
  caml_ext_table_init(&caml_shared_libs_path, 8);
  caml_external_raise = NULL;

  /* Determine position of bytecode file */
  pos = 0;

  /* First, try argv[0] (when ocamlrun is called by a bytecode program) */
  exe_name = argv[0];
  fd = caml_attempt_open(&exe_name, &trail, 0);

  /* Little grasshopper wonders why we do that at all, since
     "The current executable is ocamlrun itself, it's never a bytecode
     program".  Little grasshopper "ocamlc -custom" in mind should keep.
     With -custom, we have an executable that is ocamlrun itself
     concatenated with the bytecode.  So, if the attempt with argv[0]
     failed, it is worth trying again with executable_name. */
  if (fd < 0 && (proc_self_exe = caml_executable_name()) != NULL) {
    exe_name = proc_self_exe;
    fd = caml_attempt_open(&exe_name, &trail, 0);
  }

  if (fd < 0) {
    pos = parse_command_line(argv);
    if (argv[pos] == 0)
      caml_fatal_error("No bytecode file specified.\n");
    exe_name = argv[pos];
    fd = caml_attempt_open(&exe_name, &trail, 1);
    switch(fd) {
    case FILE_NOT_FOUND:
      caml_fatal_error_arg("Fatal error: cannot find file '%s'\n", caml_stat_strdup_of_os(argv[pos]));
      break;
    case BAD_BYTECODE:
      caml_fatal_error_arg(
        "Fatal error: the file '%s' is not a bytecode executable file\n",
        caml_stat_strdup_of_os(exe_name));
      break;
    }
  }
  /* Read the table of contents (section descriptors) */
  caml_read_section_descriptors(fd, &trail);
  /* Initialize the abstract machine */
  caml_init_gc (caml_init_minor_heap_wsz, caml_init_heap_wsz,
                caml_init_heap_chunk_sz, caml_init_percent_free,
                caml_init_max_percent_free, caml_init_major_window);
  caml_init_stack (caml_init_max_stack_wsz);
  caml_init_atom_table();
  caml_init_backtrace();
  /* Initialize the interpreter */
  caml_interprete(NULL, 0);
  /* Initialize the debugger, if needed */
  caml_debugger_init();
  /* Load the code */
  caml_code_size = caml_seek_section(fd, &trail, "CODE");
  caml_load_code(fd, caml_code_size);
  caml_init_debug_info();
  /* Build the table of primitives */
  shared_lib_path = read_section_to_os(fd, &trail, "DLPT");
  shared_libs = read_section_to_os(fd, &trail, "DLLS");
  req_prims = read_section(fd, &trail, "PRIM");
  if (req_prims == NULL) caml_fatal_error("Fatal error: no PRIM section\n");
  caml_build_primitive_table(shared_lib_path, shared_libs, req_prims);
  caml_stat_free(shared_lib_path);
  caml_stat_free(shared_libs);
  caml_stat_free(req_prims);
  /* Load the globals */
  caml_seek_section(fd, &trail, "DATA");
  chan = caml_open_descriptor_in(fd);
  caml_global_data = caml_input_val(chan);
  caml_close_channel(chan); /* this also closes fd */
  caml_stat_free(trail.section);
  /* Ensure that the globals are in the major heap. */
  caml_oldify_one (caml_global_data, &caml_global_data);
  caml_oldify_mopup ();
  /* Initialize system libraries */
  caml_sys_init(exe_name, argv + pos);
#ifdef _WIN32
  /* Start a thread to handle signals */
  if (caml_secure_getenv(_T("CAMLSIGPIPE")))
    _beginthread(caml_signal_thread, 4096, NULL);
#endif
  /* Execute the program */
  caml_debugger(PROGRAM_START);
  res = caml_interprete(caml_start_code, caml_code_size);
  if (Is_exception_result(res)) {
    caml_exn_bucket = Extract_exception(res);
    if (caml_debugger_in_use) {
      caml_extern_sp = &caml_exn_bucket; /* The debugger needs the
                                            exception value.*/
      caml_debugger(UNCAUGHT_EXC);
    }
    caml_fatal_uncaught_exception(caml_exn_bucket);
  }
}

/* Main entry point when code is linked in as initialized data */

CAMLexport value caml_startup_code_exn(
           code_t code, asize_t code_size,
           char *data, asize_t data_size,
           char *section_table, asize_t section_table_size,
           int pooling,
           char_os **argv)
{
  char_os * cds_file;
  char_os * exe_name;

  /* Determine options */
#ifdef DEBUG
  caml_verb_gc = 0x3F;
#endif
  caml_parse_ocamlrunparam();
#ifdef DEBUG
  caml_gc_message (-1, "### OCaml runtime: debug mode ###\n");
#endif
  if (caml_cleanup_on_exit)
    pooling = 1;
  if (!caml_startup_aux(pooling))
    return Val_unit;

  caml_init_ieee_floats();
#if defined(_MSC_VER) && __STDC_SECURE_LIB__ >= 200411L
  caml_install_invalid_parameter_handler();
#endif
  caml_init_custom_operations();
  cds_file = caml_secure_getenv(_T("CAML_DEBUG_FILE"));
  if (cds_file != NULL) {
    caml_cds_file = caml_stat_strdup_os(cds_file);
  }
  exe_name = caml_executable_name();
  if (exe_name == NULL) exe_name = caml_search_exe_in_path(argv[0]);
  caml_external_raise = NULL;
  /* Initialize the abstract machine */
  caml_init_gc (caml_init_minor_heap_wsz, caml_init_heap_wsz,
                caml_init_heap_chunk_sz, caml_init_percent_free,
                caml_init_max_percent_free, caml_init_major_window);
  caml_init_stack (caml_init_max_stack_wsz);
  caml_init_atom_table();
  caml_init_backtrace();
  /* Initialize the interpreter */
  caml_interprete(NULL, 0);
  /* Initialize the debugger, if needed */
  caml_debugger_init();
  /* Load the code */
  caml_start_code = code;
  caml_code_size = code_size;
  caml_init_code_fragments();
  caml_init_debug_info();
  if (caml_debugger_in_use) {
    int len, i;
    len = code_size / sizeof(opcode_t);
    caml_saved_code = (unsigned char *) caml_stat_alloc(len);
    for (i = 0; i < len; i++) caml_saved_code[i] = caml_start_code[i];
  }
#ifdef THREADED_CODE
  caml_thread_code(caml_start_code, code_size);
#endif
  /* Use the builtin table of primitives */
  caml_build_primitive_table_builtin();
  /* Load the globals */
  caml_global_data = caml_input_value_from_block(data, data_size);
  /* Ensure that the globals are in the major heap. */
  caml_oldify_one (caml_global_data, &caml_global_data);
  caml_oldify_mopup ();
  /* Record the sections (for caml_get_section_table in meta.c) */
  caml_section_table = section_table;
  caml_section_table_size = section_table_size;
  /* Initialize system libraries */
  caml_sys_init(exe_name, argv);
  /* Execute the program */
  caml_debugger(PROGRAM_START);
  return caml_interprete(caml_start_code, caml_code_size);
}

CAMLexport void caml_startup_code(
           code_t code, asize_t code_size,
           char *data, asize_t data_size,
           char *section_table, asize_t section_table_size,
           int pooling,
           char_os **argv)
{
  value res;

  res = caml_startup_code_exn(code, code_size, data, data_size,
                              section_table, section_table_size,
                              pooling, argv);
  if (Is_exception_result(res)) {
    caml_exn_bucket = Extract_exception(res);
    if (caml_debugger_in_use) {
      caml_extern_sp = &caml_exn_bucket; /* The debugger needs the
                                            exception value.*/
      caml_debugger(UNCAUGHT_EXC);
    }
    caml_fatal_uncaught_exception(caml_exn_bucket);
  }
}
