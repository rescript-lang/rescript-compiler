/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Manuel Serrano and Xavier Leroy, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2000 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <stddef.h>
#include "caml/alloc.h"
#include "caml/bigarray.h"
#include "caml/fail.h"
#include "caml/io.h"
#include "caml/mlvalues.h"
#include "caml/signals.h"
#include "caml/sys.h"
#include "caml/osdeps.h"
#include "unixsupport.h"

/* Temporary compatibility stuff so that this file can also be compiled
   from otherlibs/bigarray/ and included in the bigarray library. */

#ifdef IN_OCAML_BIGARRAY
#define MAP_FILE_FUNCTION caml_ba_map_file
#define MAP_FILE_FUNCTION_BYTECODE caml_ba_map_file_bytecode
#define UNMAP_FILE_FUNCTION caml_ba_unmap_file
#define ALLOC_FUNCTION caml_ba_mapped_alloc
#define CAML_MAP_FILE "Bigarray.map_file"
static void caml_ba_sys_error(void);
#define MAP_FILE_ERROR() caml_ba_sys_error()
#else
#define MAP_FILE_FUNCTION caml_unix_map_file
#define MAP_FILE_FUNCTION_BYTECODE caml_unix_map_file_bytecode
#define UNMAP_FILE_FUNCTION caml_unix_unmap_file
#define ALLOC_FUNCTION caml_unix_mapped_alloc
#define MAP_FILE_FUNCTION caml_unix_map_file
#define CAML_MAP_FILE "Unix.map_file"
#define MAP_FILE_ERROR() \
  do { win32_maperr(GetLastError()); uerror("map_file", Nothing); } while(0)
#endif

/* Defined in [mmap_ba.c] */
CAMLextern value
ALLOC_FUNCTION(int flags, int num_dims, void * data, intnat * dim);

#ifndef INVALID_SET_FILE_POINTER
#define INVALID_SET_FILE_POINTER (-1)
#endif

static __int64 caml_set_file_pointer(HANDLE h, __int64 dist, DWORD mode)
{
  LARGE_INTEGER i;
  DWORD err;

  i.QuadPart = dist;
  i.LowPart = SetFilePointer(h, i.LowPart, &i.HighPart, mode);
  if (i.LowPart == INVALID_SET_FILE_POINTER) return -1;
  return i.QuadPart;
}

CAMLprim value MAP_FILE_FUNCTION(value vfd, value vkind, value vlayout,
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
    caml_invalid_argument(CAML_MAP_FILE ": bad number of dimensions");
  for (i = 0; i < num_dims; i++) {
    dim[i] = Long_val(Field(vdim, i));
    if (dim[i] == -1 && i == major_dim) continue;
    if (dim[i] < 0)
      caml_invalid_argument(CAML_MAP_FILE ": negative dimension");
  }
  /* Determine file size */
  currpos = caml_set_file_pointer(fd, 0, FILE_CURRENT);
  if (currpos == -1) MAP_FILE_ERROR();
  file_size = caml_set_file_pointer(fd, 0, FILE_END);
  if (file_size == -1) MAP_FILE_ERROR();
  /* Determine array size in bytes (or size of array without the major
     dimension if that dimension wasn't specified) */
  array_size = caml_ba_element_size[flags & CAML_BA_KIND_MASK];
  for (i = 0; i < num_dims; i++)
    if (dim[i] != -1) array_size *= dim[i];
  /* Check if the first/last dimension is unknown */
  if (dim[major_dim] == -1) {
    /* Determine first/last dimension from file size */
    if (file_size < startpos)
      caml_failwith(CAML_MAP_FILE ": file position exceeds file size");
    data_size = file_size - startpos;
    dim[major_dim] = (uintnat) (data_size / array_size);
    array_size = dim[major_dim] * array_size;
    if (array_size != data_size)
      caml_failwith(CAML_MAP_FILE ": file size doesn't match array dimensions");
  }
  /* Restore original file position */
  caml_set_file_pointer(fd, currpos, FILE_BEGIN);
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
  if (fmap == NULL) MAP_FILE_ERROR();
  /* Determine offset so that the mapping starts at the given file pos */
  GetSystemInfo(&sysinfo);
  delta = (uintnat) (startpos % sysinfo.dwAllocationGranularity);
  /* Map the mapping in memory */
  li.QuadPart = startpos - delta;
  addr =
    MapViewOfFile(fmap, mode, li.HighPart, li.LowPart, array_size + delta);
  if (addr == NULL) MAP_FILE_ERROR();
  addr = (void *) ((uintnat) addr + delta);
  /* Close the file mapping */
  CloseHandle(fmap);
  /* Build and return the OCaml bigarray */
  return ALLOC_FUNCTION(flags, num_dims, addr, dim);
}

CAMLprim value MAP_FILE_FUNCTION_BYTECODE(value * argv, int argn)
{
  return MAP_FILE_FUNCTION(argv[0], argv[1], argv[2],
                           argv[3], argv[4], argv[5]);
}

void UNMAP_FILE_FUNCTION(void * addr, uintnat len)
{
  SYSTEM_INFO sysinfo;
  uintnat delta;

  GetSystemInfo(&sysinfo);
  delta = (uintnat) addr % sysinfo.dwAllocationGranularity;
  UnmapViewOfFile((void *)((uintnat)addr - delta));
}

#ifdef IN_OCAML_BIGARRAY

/* This function reports a Win32 error as a Sys_error exception.
   It is included for backward compatibility with the old
   Bigarray.*.map_file implementation.  */

static void caml_ba_sys_error(void)
{
  wchar_t buffer[512];
  DWORD errnum;

  errnum = GetLastError();
  if (!FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
                     NULL,
                     errnum,
                     0,
                     buffer,
                     sizeof(buffer)/sizeof(wchar_t),
                     NULL))
    swprintf(buffer, sizeof(buffer)/sizeof(wchar_t), L"Unknown error %ld\n", errnum);
  caml_raise_sys_error(caml_copy_string_of_utf16(buffer));
}

#endif
