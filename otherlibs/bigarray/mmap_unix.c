/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*         Manuel Serrano and Xavier Leroy, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* Needed (under Linux at least) to get pwrite's prototype in unistd.h.
   Must be defined before the first system .h is included. */
#define _XOPEN_SOURCE 500

#include <stddef.h>
#include <string.h>
#include "bigarray.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/io.h"
#include "caml/mlvalues.h"
#include "caml/sys.h"
#include "caml/signals.h"

extern int caml_ba_element_size[];  /* from bigarray_stubs.c */

#include <errno.h>
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef HAS_MMAP
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#endif

#if defined(HAS_MMAP)

#ifndef MAP_FAILED
#define MAP_FAILED ((void *) -1)
#endif

/* [caml_grow_file] function contributed by Gerd Stolpmann (PR#5543). */

static int caml_grow_file(int fd, file_offset size)
{
  char c;
  int p;

  /* First use pwrite for growing - it is a conservative method, as it
     can never happen that we shrink by accident
   */
#ifdef HAS_PWRITE
  c = 0;
  p = pwrite(fd, &c, 1, size - 1);
#else

  /* Emulate pwrite with lseek. This should only be necessary on ancient
     systems nowadays
   */
  file_offset currpos;
  currpos = lseek(fd, 0, SEEK_CUR);
  if (currpos != -1) {
    p = lseek(fd, size - 1, SEEK_SET);
    if (p != -1) {
      c = 0;
      p = write(fd, &c, 1);
      if (p != -1)
        p = lseek(fd, currpos, SEEK_SET);
    }
  }
  else p=-1;
#endif
#ifdef HAS_TRUNCATE
  if (p == -1 && errno == ESPIPE) {
    /* Plan B. Check if at least ftruncate is possible. There are
       some non-seekable descriptor types that do not support pwrite
       but ftruncate, like shared memory. We never get into this case
       for real files, so there is no danger of truncating persistent
       data by accident
     */
    p = ftruncate(fd, size);
  }
#endif
  return p;
}


CAMLprim value caml_ba_map_file(value vfd, value vkind, value vlayout,
                                value vshared, value vdim, value vstart)
{
  int fd, flags, major_dim, shared;
  intnat num_dims, i;
  intnat dim[CAML_BA_MAX_NUM_DIMS];
  file_offset startpos, file_size, data_size;
  struct stat st;
  uintnat array_size, page, delta;
  void * addr;

  fd = Int_val(vfd);
  flags = Caml_ba_kind_val(vkind) | Caml_ba_layout_val(vlayout);
  startpos = File_offset_val(vstart);
  num_dims = Wosize_val(vdim);
  major_dim = flags & CAML_BA_FORTRAN_LAYOUT ? num_dims - 1 : 0;
  /* Extract dimensions from OCaml array */
  num_dims = Wosize_val(vdim);
  if (num_dims < 1 || num_dims > CAML_BA_MAX_NUM_DIMS)
    caml_invalid_argument("Bigarray.mmap: bad number of dimensions");
  for (i = 0; i < num_dims; i++) {
    dim[i] = Long_val(Field(vdim, i));
    if (dim[i] == -1 && i == major_dim) continue;
    if (dim[i] < 0)
      caml_invalid_argument("Bigarray.create: negative dimension");
  }
  /* Determine file size. We avoid lseek here because it is fragile,
     and because some mappable file types do not support it
   */
  caml_enter_blocking_section();
  if (fstat(fd, &st) == -1) {
    caml_leave_blocking_section();
    caml_sys_error(NO_ARG);
  }
  file_size = st.st_size;
  /* Determine array size in bytes (or size of array without the major
     dimension if that dimension wasn't specified) */
  array_size = caml_ba_element_size[flags & CAML_BA_KIND_MASK];
  for (i = 0; i < num_dims; i++)
    if (dim[i] != -1) array_size *= dim[i];
  /* Check if the major dimension is unknown */
  if (dim[major_dim] == -1) {
    /* Determine major dimension from file size */
    if (file_size < startpos) {
      caml_leave_blocking_section();
      caml_failwith("Bigarray.mmap: file position exceeds file size");
    }
    data_size = file_size - startpos;
    dim[major_dim] = (uintnat) (data_size / array_size);
    array_size = dim[major_dim] * array_size;
    if (array_size != data_size) {
      caml_leave_blocking_section();
      caml_failwith("Bigarray.mmap: file size doesn't match array dimensions");
    }
  } else {
    /* Check that file is large enough, and grow it otherwise */
    if (file_size < startpos + array_size) {
      if (caml_grow_file(fd, startpos + array_size) == -1) { /* PR#5543 */
        caml_leave_blocking_section();
        caml_sys_error(NO_ARG);
      }
    }
  }
  /* Determine offset so that the mapping starts at the given file pos */
  page = sysconf(_SC_PAGESIZE);
  delta = (uintnat) startpos % page;
  /* Do the mmap */
  shared = Bool_val(vshared) ? MAP_SHARED : MAP_PRIVATE;
  if (array_size > 0)
    addr = mmap(NULL, array_size + delta, PROT_READ | PROT_WRITE,
                shared, fd, startpos - delta);
  else
    addr = NULL;                /* PR#5463 - mmap fails on empty region */
  caml_leave_blocking_section();
  if (addr == (void *) MAP_FAILED) caml_sys_error(NO_ARG);
  addr = (void *) ((uintnat) addr + delta);
  /* Build and return the OCaml bigarray */
  return caml_ba_alloc(flags | CAML_BA_MAPPED_FILE, num_dims, addr, dim);
}

#else

CAMLprim value caml_ba_map_file(value vfd, value vkind, value vlayout,
                                value vshared, value vdim, value vpos)
{
  caml_invalid_argument("Bigarray.map_file: not supported");
  return Val_unit;
}

#endif

CAMLprim value caml_ba_map_file_bytecode(value * argv, int argn)
{
  return caml_ba_map_file(argv[0], argv[1], argv[2],
                          argv[3], argv[4], argv[5]);
}

void caml_ba_unmap_file(void * addr, uintnat len)
{
#if defined(HAS_MMAP)
  uintnat page = sysconf(_SC_PAGESIZE);
  uintnat delta = (uintnat) addr % page;
  if (len == 0) return;         /* PR#5463 */
  addr = (void *)((uintnat)addr - delta);
  len  = len + delta;
#if defined(_POSIX_SYNCHRONIZED_IO)
  msync(addr, len, MS_ASYNC);   /* PR#3571 */
#endif
  munmap(addr, len);
#endif
}
