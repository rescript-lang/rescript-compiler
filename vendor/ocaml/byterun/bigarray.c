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

/* This file is an intermediate step in making the bigarray library
   (in otherlibs/bigarray) a part of the standard library.
   This file defines the basic allocation functions for bigarrays,
   as well as the comparison, hashing and marshaling methods for
   bigarrays.  The other bigarray primitives are still defined
   in otherlibs/bigarray.  Memory-mapping a file as a bigarray
   is being migrated to otherlibs/unix and otherlibs/win32unix. */

#define CAML_INTERNALS

#include <stddef.h>
#include <stdarg.h>
#include "caml/alloc.h"
#include "caml/bigarray.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/intext.h"
#include "caml/hash.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

/* Compute the number of elements of a big array */

CAMLexport uintnat caml_ba_num_elts(struct caml_ba_array * b)
{
  uintnat num_elts;
  int i;
  num_elts = 1;
  for (i = 0; i < b->num_dims; i++) num_elts = num_elts * b->dim[i];
  return num_elts;
}

/* Size in bytes of a bigarray element, indexed by bigarray kind */

CAMLexport int caml_ba_element_size[] =
{ 4 /*FLOAT32*/, 8 /*FLOAT64*/,
  1 /*SINT8*/, 1 /*UINT8*/,
  2 /*SINT16*/, 2 /*UINT16*/,
  4 /*INT32*/, 8 /*INT64*/,
  sizeof(value) /*CAML_INT*/, sizeof(value) /*NATIVE_INT*/,
  8 /*COMPLEX32*/, 16 /*COMPLEX64*/,
  1 /*CHAR*/
};

/* Compute the number of bytes for the elements of a big array */

CAMLexport uintnat caml_ba_byte_size(struct caml_ba_array * b)
{
  return caml_ba_num_elts(b)
         * caml_ba_element_size[b->flags & CAML_BA_KIND_MASK];
}

/* Operation table for bigarrays */

CAMLexport struct custom_operations caml_ba_ops = {
  "_bigarray",
  caml_ba_finalize,
  caml_ba_compare,
  caml_ba_hash,
  caml_ba_serialize,
  caml_ba_deserialize,
  custom_compare_ext_default
};

/* Allocation of a big array */

#define CAML_BA_MAX_MEMORY 1024*1024*1024
/* 1 Gb -- after allocating that much, it's probably worth speeding
   up the major GC */

/* [caml_ba_alloc] will allocate a new bigarray object in the heap.
   If [data] is NULL, the memory for the contents is also allocated
   (with [malloc]) by [caml_ba_alloc].
   [data] cannot point into the OCaml heap.
   [dim] may point into an object in the OCaml heap.
*/
CAMLexport value
caml_ba_alloc(int flags, int num_dims, void * data, intnat * dim)
{
  uintnat num_elts, asize, size;
  int i;
  value res;
  struct caml_ba_array * b;
  intnat dimcopy[CAML_BA_MAX_NUM_DIMS];

  CAMLassert(num_dims >= 0 && num_dims <= CAML_BA_MAX_NUM_DIMS);
  CAMLassert((flags & CAML_BA_KIND_MASK) <= CAML_BA_CHAR);
  for (i = 0; i < num_dims; i++) dimcopy[i] = dim[i];
  size = 0;
  if (data == NULL) {
    num_elts = 1;
    for (i = 0; i < num_dims; i++) {
      if (caml_umul_overflow(num_elts, dimcopy[i], &num_elts))
        caml_raise_out_of_memory();
    }
    if (caml_umul_overflow(num_elts,
                           caml_ba_element_size[flags & CAML_BA_KIND_MASK],
                           &size))
      caml_raise_out_of_memory();
    data = malloc(size);
    if (data == NULL && size != 0) caml_raise_out_of_memory();
    flags |= CAML_BA_MANAGED;
  }
  asize = SIZEOF_BA_ARRAY + num_dims * sizeof(intnat);
  res = caml_alloc_custom(&caml_ba_ops, asize, size, CAML_BA_MAX_MEMORY);
  b = Caml_ba_array_val(res);
  b->data = data;
  b->num_dims = num_dims;
  b->flags = flags;
  b->proxy = NULL;
  for (i = 0; i < num_dims; i++) b->dim[i] = dimcopy[i];
  return res;
}

/* Same as caml_ba_alloc, but dimensions are passed as a list of
   arguments */

CAMLexport value caml_ba_alloc_dims(int flags, int num_dims, void * data, ...)
{
  va_list ap;
  intnat dim[CAML_BA_MAX_NUM_DIMS];
  int i;
  value res;

  CAMLassert(num_dims <= CAML_BA_MAX_NUM_DIMS);
  va_start(ap, data);
  for (i = 0; i < num_dims; i++) dim[i] = va_arg(ap, intnat);
  va_end(ap);
  res = caml_ba_alloc(flags, num_dims, data, dim);
  return res;
}

/* Finalization of a big array */

CAMLexport void caml_ba_finalize(value v)
{
  struct caml_ba_array * b = Caml_ba_array_val(v);

  switch (b->flags & CAML_BA_MANAGED_MASK) {
  case CAML_BA_EXTERNAL:
    break;
  case CAML_BA_MANAGED:
    if (b->proxy == NULL) {
      free(b->data);
    } else {
      if (-- b->proxy->refcount == 0) {
        free(b->proxy->data);
        free(b->proxy);
      }
    }
    break;
  case CAML_BA_MAPPED_FILE:
    /* Bigarrays for mapped files use a different finalization method */
  default:
    CAMLassert(0);
  }
}

/* Comparison of two big arrays */

CAMLexport int caml_ba_compare(value v1, value v2)
{
  struct caml_ba_array * b1 = Caml_ba_array_val(v1);
  struct caml_ba_array * b2 = Caml_ba_array_val(v2);
  uintnat n, num_elts;
  intnat flags1, flags2;
  int i;

  /* Compare kind & layout in case the arguments are of different types */
  flags1 = b1->flags & (CAML_BA_KIND_MASK | CAML_BA_LAYOUT_MASK);
  flags2 = b2->flags & (CAML_BA_KIND_MASK | CAML_BA_LAYOUT_MASK);
  if (flags1 != flags2) return flags2 - flags1;
  /* Compare number of dimensions */
  if (b1->num_dims != b2->num_dims) return b2->num_dims - b1->num_dims;
  /* Same number of dimensions: compare dimensions lexicographically */
  for (i = 0; i < b1->num_dims; i++) {
    intnat d1 = b1->dim[i];
    intnat d2 = b2->dim[i];
    if (d1 != d2) return d1 < d2 ? -1 : 1;
  }
  /* Same dimensions: compare contents lexicographically */
  num_elts = caml_ba_num_elts(b1);

#define DO_INTEGER_COMPARISON(type) \
  { type * p1 = b1->data; type * p2 = b2->data; \
    for (n = 0; n < num_elts; n++) { \
      type e1 = *p1++; type e2 = *p2++; \
      if (e1 < e2) return -1; \
      if (e1 > e2) return 1; \
    } \
    return 0; \
  }
#define DO_FLOAT_COMPARISON(type) \
  { type * p1 = b1->data; type * p2 = b2->data; \
    for (n = 0; n < num_elts; n++) { \
      type e1 = *p1++; type e2 = *p2++; \
      if (e1 < e2) return -1; \
      if (e1 > e2) return 1; \
      if (e1 != e2) { \
        caml_compare_unordered = 1; \
        if (e1 == e1) return 1; \
        if (e2 == e2) return -1; \
      } \
    } \
    return 0; \
  }

  switch (b1->flags & CAML_BA_KIND_MASK) {
  case CAML_BA_COMPLEX32:
    num_elts *= 2; /*fallthrough*/
  case CAML_BA_FLOAT32:
    DO_FLOAT_COMPARISON(float);
  case CAML_BA_COMPLEX64:
    num_elts *= 2; /*fallthrough*/
  case CAML_BA_FLOAT64:
    DO_FLOAT_COMPARISON(double);
  case CAML_BA_CHAR:
    DO_INTEGER_COMPARISON(caml_ba_uint8);
  case CAML_BA_SINT8:
    DO_INTEGER_COMPARISON(caml_ba_int8);
  case CAML_BA_UINT8:
    DO_INTEGER_COMPARISON(caml_ba_uint8);
  case CAML_BA_SINT16:
    DO_INTEGER_COMPARISON(caml_ba_int16);
  case CAML_BA_UINT16:
    DO_INTEGER_COMPARISON(caml_ba_uint16);
  case CAML_BA_INT32:
    DO_INTEGER_COMPARISON(int32_t);
  case CAML_BA_INT64:
    DO_INTEGER_COMPARISON(int64_t);
  case CAML_BA_CAML_INT:
  case CAML_BA_NATIVE_INT:
    DO_INTEGER_COMPARISON(intnat);
  default:
    CAMLassert(0);
    return 0;                   /* should not happen */
  }
#undef DO_INTEGER_COMPARISON
#undef DO_FLOAT_COMPARISON
}

/* Hashing of a bigarray */

CAMLexport intnat caml_ba_hash(value v)
{
  struct caml_ba_array * b = Caml_ba_array_val(v);
  intnat num_elts, n;
  uint32_t h, w;
  int i;

  num_elts = 1;
  for (i = 0; i < b->num_dims; i++) num_elts = num_elts * b->dim[i];
  h = 0;

  switch (b->flags & CAML_BA_KIND_MASK) {
  case CAML_BA_CHAR:
  case CAML_BA_SINT8:
  case CAML_BA_UINT8: {
    caml_ba_uint8 * p = b->data;
    if (num_elts > 256) num_elts = 256;
    for (n = 0; n + 4 <= num_elts; n += 4, p += 4) {
      w = p[0] | (p[1] << 8) | (p[2] << 16) | (p[3] << 24);
      h = caml_hash_mix_uint32(h, w);
    }
    w = 0;
    switch (num_elts & 3) {
    case 3: w  = p[2] << 16;    /* fallthrough */
    case 2: w |= p[1] << 8;     /* fallthrough */
    case 1: w |= p[0];
            h = caml_hash_mix_uint32(h, w);
    }
    break;
  }
  case CAML_BA_SINT16:
  case CAML_BA_UINT16: {
    caml_ba_uint16 * p = b->data;
    if (num_elts > 128) num_elts = 128;
    for (n = 0; n + 2 <= num_elts; n += 2, p += 2) {
      w = p[0] | (p[1] << 16);
      h = caml_hash_mix_uint32(h, w);
    }
    if ((num_elts & 1) != 0)
      h = caml_hash_mix_uint32(h, p[0]);
    break;
  }
  case CAML_BA_INT32:
  {
    uint32_t * p = b->data;
    if (num_elts > 64) num_elts = 64;
    for (n = 0; n < num_elts; n++, p++) h = caml_hash_mix_uint32(h, *p);
    break;
  }
  case CAML_BA_CAML_INT:
  case CAML_BA_NATIVE_INT:
  {
    intnat * p = b->data;
    if (num_elts > 64) num_elts = 64;
    for (n = 0; n < num_elts; n++, p++) h = caml_hash_mix_intnat(h, *p);
    break;
  }
  case CAML_BA_INT64:
  {
    int64_t * p = b->data;
    if (num_elts > 32) num_elts = 32;
    for (n = 0; n < num_elts; n++, p++) h = caml_hash_mix_int64(h, *p);
    break;
  }
  case CAML_BA_COMPLEX32:
    num_elts *= 2;              /* fallthrough */
  case CAML_BA_FLOAT32:
  {
    float * p = b->data;
    if (num_elts > 64) num_elts = 64;
    for (n = 0; n < num_elts; n++, p++) h = caml_hash_mix_float(h, *p);
    break;
  }
  case CAML_BA_COMPLEX64:
    num_elts *= 2;              /* fallthrough */
  case CAML_BA_FLOAT64:
  {
    double * p = b->data;
    if (num_elts > 32) num_elts = 32;
    for (n = 0; n < num_elts; n++, p++) h = caml_hash_mix_double(h, *p);
    break;
  }
  }
  return h;
}

static void caml_ba_serialize_longarray(void * data,
                                        intnat num_elts,
                                        intnat min_val, intnat max_val)
{
#ifdef ARCH_SIXTYFOUR
  int overflow_32 = 0;
  intnat * p, n;
  for (n = 0, p = data; n < num_elts; n++, p++) {
    if (*p < min_val || *p > max_val) { overflow_32 = 1; break; }
  }
  if (overflow_32) {
    caml_serialize_int_1(1);
    caml_serialize_block_8(data, num_elts);
  } else {
    caml_serialize_int_1(0);
    for (n = 0, p = data; n < num_elts; n++, p++)
      caml_serialize_int_4((int32_t) *p);
  }
#else
  caml_serialize_int_1(0);
  caml_serialize_block_4(data, num_elts);
#endif
}

CAMLexport void caml_ba_serialize(value v,
                              uintnat * wsize_32,
                              uintnat * wsize_64)
{
  struct caml_ba_array * b = Caml_ba_array_val(v);
  intnat num_elts;
  int i;

  /* Serialize header information */
  caml_serialize_int_4(b->num_dims);
  caml_serialize_int_4(b->flags & (CAML_BA_KIND_MASK | CAML_BA_LAYOUT_MASK));
  /* On a 64-bit machine, if any of the dimensions is >= 2^32,
     the size of the marshaled data will be >= 2^32 and
     extern_value() will fail.  So, it is safe to write the dimensions
     as 32-bit unsigned integers. */
  for (i = 0; i < b->num_dims; i++) caml_serialize_int_4(b->dim[i]);
  /* Compute total number of elements */
  num_elts = 1;
  for (i = 0; i < b->num_dims; i++) num_elts = num_elts * b->dim[i];
  /* Serialize elements */
  switch (b->flags & CAML_BA_KIND_MASK) {
  case CAML_BA_CHAR:
  case CAML_BA_SINT8:
  case CAML_BA_UINT8:
    caml_serialize_block_1(b->data, num_elts); break;
  case CAML_BA_SINT16:
  case CAML_BA_UINT16:
    caml_serialize_block_2(b->data, num_elts); break;
  case CAML_BA_FLOAT32:
  case CAML_BA_INT32:
    caml_serialize_block_4(b->data, num_elts); break;
  case CAML_BA_COMPLEX32:
    caml_serialize_block_4(b->data, num_elts * 2); break;
  case CAML_BA_FLOAT64:
  case CAML_BA_INT64:
    caml_serialize_block_8(b->data, num_elts); break;
  case CAML_BA_COMPLEX64:
    caml_serialize_block_8(b->data, num_elts * 2); break;
  case CAML_BA_CAML_INT:
    caml_ba_serialize_longarray(b->data, num_elts, -0x40000000, 0x3FFFFFFF);
    break;
  case CAML_BA_NATIVE_INT:
    caml_ba_serialize_longarray(b->data, num_elts, -0x80000000, 0x7FFFFFFF);
    break;
  }
  /* Compute required size in OCaml heap.  Assumes struct caml_ba_array
     is exactly 4 + num_dims words */
  CAMLassert(SIZEOF_BA_ARRAY == 4 * sizeof(value));
  *wsize_32 = (4 + b->num_dims) * 4;
  *wsize_64 = (4 + b->num_dims) * 8;
}

static void caml_ba_deserialize_longarray(void * dest, intnat num_elts)
{
  int sixty = caml_deserialize_uint_1();
#ifdef ARCH_SIXTYFOUR
  if (sixty) {
    caml_deserialize_block_8(dest, num_elts);
  } else {
    intnat * p, n;
    for (n = 0, p = dest; n < num_elts; n++, p++)
      *p = caml_deserialize_sint_4();
  }
#else
  if (sixty)
    caml_deserialize_error("input_value: cannot read bigarray "
                      "with 64-bit OCaml ints");
  caml_deserialize_block_4(dest, num_elts);
#endif
}

CAMLexport uintnat caml_ba_deserialize(void * dst)
{
  struct caml_ba_array * b = dst;
  int i, elt_size;
  uintnat num_elts;

  /* Read back header information */
  b->num_dims = caml_deserialize_uint_4();
  b->flags = caml_deserialize_uint_4() | CAML_BA_MANAGED;
  b->proxy = NULL;
  for (i = 0; i < b->num_dims; i++) b->dim[i] = caml_deserialize_uint_4();
  /* Compute total number of elements */
  num_elts = caml_ba_num_elts(b);
  /* Determine element size in bytes */
  if ((b->flags & CAML_BA_KIND_MASK) > CAML_BA_CHAR)
    caml_deserialize_error("input_value: bad bigarray kind");
  elt_size = caml_ba_element_size[b->flags & CAML_BA_KIND_MASK];
  /* Allocate room for data */
  b->data = malloc(elt_size * num_elts);
  if (b->data == NULL)
    caml_deserialize_error("input_value: out of memory for bigarray");
  /* Read data */
  switch (b->flags & CAML_BA_KIND_MASK) {
  case CAML_BA_CHAR:
  case CAML_BA_SINT8:
  case CAML_BA_UINT8:
    caml_deserialize_block_1(b->data, num_elts); break;
  case CAML_BA_SINT16:
  case CAML_BA_UINT16:
    caml_deserialize_block_2(b->data, num_elts); break;
  case CAML_BA_FLOAT32:
  case CAML_BA_INT32:
    caml_deserialize_block_4(b->data, num_elts); break;
  case CAML_BA_COMPLEX32:
    caml_deserialize_block_4(b->data, num_elts * 2); break;
  case CAML_BA_FLOAT64:
  case CAML_BA_INT64:
    caml_deserialize_block_8(b->data, num_elts); break;
  case CAML_BA_COMPLEX64:
    caml_deserialize_block_8(b->data, num_elts * 2); break;
  case CAML_BA_CAML_INT:
  case CAML_BA_NATIVE_INT:
    caml_ba_deserialize_longarray(b->data, num_elts); break;
  }
  /* PR#5516: use C99's flexible array types if possible */
  return SIZEOF_BA_ARRAY + b->num_dims * sizeof(intnat);
}
