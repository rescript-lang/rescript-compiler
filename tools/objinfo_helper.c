/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*        Mehdi Dogguy, PPS laboratory, University Paris Diderot       */
/*                                                                     */
/*  Copyright 2010 Mehdi Dogguy.  Used and distributed as part of      */
/*  OCaml by permission from the author.   This file is                */
/*  distributed under the terms of the Q Public License version 1.0.   */
/***********************************************************************/

#include "../config/s.h"
#include "../byterun/caml/mlvalues.h"
#include "../byterun/caml/alloc.h"
#include <stdio.h>

#ifdef HAS_LIBBFD
#include <stdlib.h>
#include <string.h>

// PACKAGE: protect against binutils change
//   https://sourceware.org/bugzilla/show_bug.cgi?id=14243
#define PACKAGE "ocamlobjinfo"
#include <bfd.h>
#undef PACKAGE

#ifdef __APPLE__
#define plugin_header_sym "_caml_plugin_header"
#else
#define plugin_header_sym "caml_plugin_header"
#endif

int main(int argc, char ** argv)
{
  bfd *fd;
  asection *sec;
  file_ptr offset;
  long st_size;
  asymbol ** symbol_table;
  long sym_count, i;

  if (argc != 2) {
    fprintf(stderr, "Usage: objinfo_helper <dynamic library>\n");
    return 2;
  }

  fd = bfd_openr(argv[1], "default");
  if (!fd) {
    fprintf(stderr, "Error opening file %s\n", argv[1]);
    return 2;
  }
  if (! bfd_check_format (fd, bfd_object)) {
    fprintf(stderr, "Error: wrong format\n");
    bfd_close(fd);
    return 2;
  }

  sec = bfd_get_section_by_name(fd, ".data");
  if (! sec) {
    fprintf(stderr, "Error: section .data not found\n");
    bfd_close(fd);
    return 2;
  }

  offset = sec->filepos;
  st_size = bfd_get_dynamic_symtab_upper_bound (fd);
  if (st_size <= 0) {
    fprintf(stderr, "Error: size of section .data unknown\n");
    bfd_close(fd);
    return 2;
  }

  symbol_table = malloc(st_size);
  if (! symbol_table) {
    fprintf(stderr, "Error: out of memory\n");
    bfd_close(fd);
    return 2;
  }

  sym_count = bfd_canonicalize_dynamic_symtab (fd, symbol_table);

  for (i = 0; i < sym_count; i++) {
    if (strcmp(symbol_table[i]->name, plugin_header_sym) == 0) {
      printf("%ld\n", (long) (offset + symbol_table[i]->value));
      bfd_close(fd);
      return 0;
    }
  }

  fprintf(stderr, "Error: missing symbol %s\n", plugin_header_sym);
  bfd_close(fd);
  return 2;
}

#else

int main(int argc, char ** argv)
{
  fprintf(stderr,"BFD library unavailable, cannot print info on .cmxs files\n");
  return 2;
}

#endif
