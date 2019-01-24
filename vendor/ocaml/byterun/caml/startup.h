/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2001 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_STARTUP_H
#define CAML_STARTUP_H

#ifdef CAML_INTERNALS

#include "mlvalues.h"
#include "exec.h"

CAMLextern void caml_main(char_os **argv);

CAMLextern void caml_startup_code(
           code_t code, asize_t code_size,
           char *data, asize_t data_size,
           char *section_table, asize_t section_table_size,
           int pooling,
           char_os **argv);

CAMLextern value caml_startup_code_exn(
  code_t code, asize_t code_size,
  char *data, asize_t data_size,
  char *section_table, asize_t section_table_size,
  int pooling,
  char_os **argv);

enum { FILE_NOT_FOUND = -1, BAD_BYTECODE  = -2 };

extern int caml_attempt_open(char_os **name, struct exec_trailer *trail,
                             int do_open_script);
extern void caml_read_section_descriptors(int fd, struct exec_trailer *trail);
extern int32_t caml_seek_optional_section(int fd, struct exec_trailer *trail,
                                        char *name);
extern int32_t caml_seek_section(int fd, struct exec_trailer *trail,
                                 char *name);

#endif /* CAML_INTERNALS */

#endif /* CAML_STARTUP_H */
