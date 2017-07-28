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

#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include "bigarray.h"
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/mlvalues.h"
#include "caml/sys.h"
#include "unixsupport.h"

extern int caml_ba_element_size[];  /* from bigarray_stubs.c */

static void caml_ba_sys_error(void);

#ifndef INVALID_SET_FILE_POINTER
#define INVALID_SET_FILE_POINTER (-1)
#endif

static __int64 caml_ba_set_file_pointer(HANDLE h, __int64 dist, DWORD mode)
{
  LARGE_INTEGER i;
  DWORD err;

  i.QuadPart = dist;
  i.LowPart = SetFilePointer(h, i.LowPart, &i.HighPart, mode);
  if (i.LowPart == INVALID_SET_FILE_POINTER) return -1;
  return i.QuadPart;
}

CAMLprim value caml_ba_map_file(value vfd, value vkind, value vlayout,
                                value vshared, value vdim, value vstart)
{
  HANDLE fd, fmap;
  int flags, major_dim, mode, perm;
  intnat num_dims, i;
  intnat dim[CAML_BA_MAX_NUM_DIMS];
  __int64 currpos, startpos, file_size, data_size;
  uintnat array_size, page, delta;
  char c;
  void * addr;
  LARGE_INTEGER li;
  SYSTEM_INFO sysinfo;

  fd = Handle_val(vfd);
  flags = Caml_ba_kind_val(vkind) | Caml_ba_layout_val(vlayout);
  startpos = Int64_val(vstart);
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
  /* Determine file size */
  currpos = caml_ba_set_file_pointer(fd, 0, FILE_CURRENT);
  if (currpos == -1) caml_ba_sys_error();
  file_size = caml_ba_set_file_pointer(fd, 0, FILE_END);
  if (file_size == -1) caml_ba_sys_error();
  /* Determine array size in bytes (or size of array without the major
     dimension if that dimension wasn't specified) */
  array_size = caml_ba_element_size[flags & CAML_BA_KIND_MASK];
  for (i = 0; i < num_dims; i++)
    if (dim[i] != -1) array_size *= dim[i];
  /* Check if the first/last dimension is unknown */
  if (dim[major_dim] == -1) {
    /* Determine first/last dimension from file size */
    if (file_size < startpos)
      caml_failwith("Bigarray.mmap: file position exceeds file size");
    data_size = file_size - startpos;
    dim[major_dim] = (uintnat) (data_size / array_size);
    array_size = dim[major_dim] * array_size;
    if (array_size != data_size)
      caml_failwith("Bigarray.mmap: file size doesn't match array dimensions");
  }
  /* Restore original file position */
  caml_ba_set_file_pointer(fd, currpos, FILE_BEGIN);
  /* Create the file mapping */
  if (Bool_val(vshared)) {
    perm = PAGE_READWRITE;
    mode = FILE_MAP_WRITE;
  } else {
    perm = PAGE_READONLY;       /* doesn't work under Win98 */
    mode = FILE_MAP_COPY;
  }
  li.QuadPart = startpos + array_size;
  fmap = CreateFileMapping(fd, NULL, perm, li.HighPart, li.LowPart, NULL);
  if (fmap == NULL) caml_ba_sys_error();
  /* Determine offset so that the mapping starts at the given file pos */
  GetSystemInfo(&sysinfo);
  delta = (uintnat) (startpos % sysinfo.dwAllocationGranularity);
  /* Map the mapping in memory */
  li.QuadPart = startpos - delta;
  addr =
    MapViewOfFile(fmap, mode, li.HighPart, li.LowPart, array_size + delta);
  if (addr == NULL) caml_ba_sys_error();
  addr = (void *) ((uintnat) addr + delta);
  /* Close the file mapping */
  CloseHandle(fmap);
  /* Build and return the OCaml bigarray */
  return caml_ba_alloc(flags | CAML_BA_MAPPED_FILE, num_dims, addr, dim);
}

CAMLprim value caml_ba_map_file_bytecode(value * argv, int argn)
{
  return caml_ba_map_file(argv[0], argv[1], argv[2],
                          argv[3], argv[4], argv[5]);
}

void caml_ba_unmap_file(void * addr, uintnat len)
{
  SYSTEM_INFO sysinfo;
  uintnat delta;

  GetSystemInfo(&sysinfo);
  delta = (uintnat) addr % sysinfo.dwAllocationGranularity;
  UnmapViewOfFile((void *)((uintnat)addr - delta));
}

static void caml_ba_sys_error(void)
{
  char buffer[512];
  DWORD errnum;

  errnum = GetLastError();
  if (!FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
                     NULL,
                     errnum,
                     0,
                     buffer,
                     sizeof(buffer),
                     NULL))
    sprintf(buffer, "Unknown error %ld\n", errnum);
  caml_raise_sys_error(caml_copy_string(buffer));
}
