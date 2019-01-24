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
#include <stdarg.h>
#include <string.h>
#include "caml/alloc.h"
#include "caml/bigarray.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/intext.h"
#include "caml/hash.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/signals.h"

#define int8 caml_ba_int8
#define uint8 caml_ba_uint8
#define int16 caml_ba_int16
#define uint16 caml_ba_uint16

/* Allocate a bigarray from OCaml */

CAMLprim value caml_ba_create(value vkind, value vlayout, value vdim)
{
  intnat dim[CAML_BA_MAX_NUM_DIMS];
  mlsize_t num_dims;
  int i, flags;

  num_dims = Wosize_val(vdim);
  /* here num_dims is unsigned (mlsize_t) so no need to check (num_dims >= 0) */
  if (num_dims > CAML_BA_MAX_NUM_DIMS)
    caml_invalid_argument("Bigarray.create: bad number of dimensions");
  for (i = 0; i < num_dims; i++) {
    dim[i] = Long_val(Field(vdim, i));
    if (dim[i] < 0)
      caml_invalid_argument("Bigarray.create: negative dimension");
  }
  flags = Caml_ba_kind_val(vkind) | Caml_ba_layout_val(vlayout);
  return caml_ba_alloc(flags, num_dims, NULL, dim);
}

/* Given a big array and a vector of indices, check that the indices
   are within the bounds and return the offset of the corresponding
   array element in the data part of the array. */

static long caml_ba_offset(struct caml_ba_array * b, intnat * index)
{
  intnat offset;
  int i;

  offset = 0;
  if ((b->flags & CAML_BA_LAYOUT_MASK) == CAML_BA_C_LAYOUT) {
    /* C-style layout: row major, indices start at 0 */
    for (i = 0; i < b->num_dims; i++) {
      if ((uintnat) index[i] >= (uintnat) b->dim[i])
        caml_array_bound_error();
      offset = offset * b->dim[i] + index[i];
    }
  } else {
    /* Fortran-style layout: column major, indices start at 1 */
    for (i = b->num_dims - 1; i >= 0; i--) {
      if ((uintnat) (index[i] - 1) >= (uintnat) b->dim[i])
        caml_array_bound_error();
      offset = offset * b->dim[i] + (index[i] - 1);
    }
  }
  return offset;
}

/* Helper function to allocate a record of two double floats */

static value copy_two_doubles(double d0, double d1)
{
  value res = caml_alloc_small(2 * Double_wosize, Double_array_tag);
  Store_double_field(res, 0, d0);
  Store_double_field(res, 1, d1);
  return res;
}

/* Generic code to read from a big array */

value caml_ba_get_N(value vb, value * vind, int nind)
{
  struct caml_ba_array * b = Caml_ba_array_val(vb);
  intnat index[CAML_BA_MAX_NUM_DIMS];
  int i;
  intnat offset;

  /* Check number of indices = number of dimensions of array
     (maybe not necessary if ML typing guarantees this) */
  if (nind != b->num_dims)
    caml_invalid_argument("Bigarray.get: wrong number of indices");
  /* Compute offset and check bounds */
  for (i = 0; i < b->num_dims; i++) index[i] = Long_val(vind[i]);
  offset = caml_ba_offset(b, index);
  /* Perform read */
  switch ((b->flags) & CAML_BA_KIND_MASK) {
  default:
    CAMLassert(0);
  case CAML_BA_FLOAT32:
    return caml_copy_double(((float *) b->data)[offset]);
  case CAML_BA_FLOAT64:
    return caml_copy_double(((double *) b->data)[offset]);
  case CAML_BA_SINT8:
    return Val_int(((int8 *) b->data)[offset]);
  case CAML_BA_UINT8:
    return Val_int(((uint8 *) b->data)[offset]);
  case CAML_BA_SINT16:
    return Val_int(((int16 *) b->data)[offset]);
  case CAML_BA_UINT16:
    return Val_int(((uint16 *) b->data)[offset]);
  case CAML_BA_INT32:
    return caml_copy_int32(((int32_t *) b->data)[offset]);
  case CAML_BA_INT64:
    return caml_copy_int64(((int64_t *) b->data)[offset]);
  case CAML_BA_NATIVE_INT:
    return caml_copy_nativeint(((intnat *) b->data)[offset]);
  case CAML_BA_CAML_INT:
    return Val_long(((intnat *) b->data)[offset]);
  case CAML_BA_COMPLEX32:
    { float * p = ((float *) b->data) + offset * 2;
      return copy_two_doubles(p[0], p[1]); }
  case CAML_BA_COMPLEX64:
    { double * p = ((double *) b->data) + offset * 2;
      return copy_two_doubles(p[0], p[1]); }
  case CAML_BA_CHAR:
    return Val_int(((unsigned char *) b->data)[offset]);
  }
}

CAMLprim value caml_ba_get_1(value vb, value vind1)
{
  return caml_ba_get_N(vb, &vind1, 1);
}

CAMLprim value caml_ba_get_2(value vb, value vind1, value vind2)
{
  value vind[2];
  vind[0] = vind1; vind[1] = vind2;
  return caml_ba_get_N(vb, vind, 2);
}

CAMLprim value caml_ba_get_3(value vb, value vind1, value vind2, value vind3)
{
  value vind[3];
  vind[0] = vind1; vind[1] = vind2; vind[2] = vind3;
  return caml_ba_get_N(vb, vind, 3);
}

#if 0
CAMLprim value caml_ba_get_4(value vb, value vind1, value vind2,
                     value vind3, value vind4)
{
  value vind[4];
  vind[0] = vind1; vind[1] = vind2; vind[2] = vind3; vind[3] = vind4;
  return caml_ba_get_N(vb, vind, 4);
}

CAMLprim value caml_ba_get_5(value vb, value vind1, value vind2,
                     value vind3, value vind4, value vind5)
{
  value vind[5];
  vind[0] = vind1; vind[1] = vind2; vind[2] = vind3;
  vind[3] = vind4; vind[4] = vind5;
  return caml_ba_get_N(vb, vind, 5);
}

CAMLprim value caml_ba_get_6(value vb, value vind1, value vind2,
                     value vind3, value vind4, value vind5, value vind6)
{
  value vind[6];
  vind[0] = vind1; vind[1] = vind2; vind[2] = vind3;
  vind[3] = vind4; vind[4] = vind5; vind[5] = vind6;
  return caml_ba_get_N(vb, vind, 6);
}
#endif

CAMLprim value caml_ba_get_generic(value vb, value vind)
{
  return caml_ba_get_N(vb, &Field(vind, 0), Wosize_val(vind));
}


CAMLprim value caml_ba_uint8_get16(value vb, value vind)
{
  intnat res;
  unsigned char b1, b2;
  intnat idx = Long_val(vind);
  struct caml_ba_array * b = Caml_ba_array_val(vb);
  if (idx < 0 || idx >= b->dim[0] - 1) caml_array_bound_error();
  b1 = ((unsigned char*) b->data)[idx];
  b2 = ((unsigned char*) b->data)[idx+1];
#ifdef ARCH_BIG_ENDIAN
  res = b1 << 8 | b2;
#else
  res = b2 << 8 | b1;
#endif
  return Val_int(res);
}

CAMLprim value caml_ba_uint8_get32(value vb, value vind)
{
  intnat res;
  unsigned char b1, b2, b3, b4;
  intnat idx = Long_val(vind);
  struct caml_ba_array * b = Caml_ba_array_val(vb);
  if (idx < 0 || idx >= b->dim[0] - 3) caml_array_bound_error();
  b1 = ((unsigned char*) b->data)[idx];
  b2 = ((unsigned char*) b->data)[idx+1];
  b3 = ((unsigned char*) b->data)[idx+2];
  b4 = ((unsigned char*) b->data)[idx+3];
#ifdef ARCH_BIG_ENDIAN
  res = b1 << 24 | b2 << 16 | b3 << 8 | b4;
#else
  res = b4 << 24 | b3 << 16 | b2 << 8 | b1;
#endif
  return caml_copy_int32(res);
}

CAMLprim value caml_ba_uint8_get64(value vb, value vind)
{
  uint64_t res;
  unsigned char b1, b2, b3, b4, b5, b6, b7, b8;
  intnat idx = Long_val(vind);
  struct caml_ba_array * b = Caml_ba_array_val(vb);
  if (idx < 0 || idx >= b->dim[0] - 7) caml_array_bound_error();
  b1 = ((unsigned char*) b->data)[idx];
  b2 = ((unsigned char*) b->data)[idx+1];
  b3 = ((unsigned char*) b->data)[idx+2];
  b4 = ((unsigned char*) b->data)[idx+3];
  b5 = ((unsigned char*) b->data)[idx+4];
  b6 = ((unsigned char*) b->data)[idx+5];
  b7 = ((unsigned char*) b->data)[idx+6];
  b8 = ((unsigned char*) b->data)[idx+7];
#ifdef ARCH_BIG_ENDIAN
  res = (uint64_t) b1 << 56 | (uint64_t) b2 << 48
        | (uint64_t) b3 << 40 | (uint64_t) b4 << 32
        | (uint64_t) b5 << 24 | (uint64_t) b6 << 16
        | (uint64_t) b7 << 8 | (uint64_t) b8;
#else
  res = (uint64_t) b8 << 56 | (uint64_t) b7 << 48
        | (uint64_t) b6 << 40 | (uint64_t) b5 << 32
        | (uint64_t) b4 << 24 | (uint64_t) b3 << 16
        | (uint64_t) b2 << 8 | (uint64_t) b1;
#endif
  return caml_copy_int64(res);
}

/* Generic write to a big array */

static value caml_ba_set_aux(value vb, value * vind, intnat nind, value newval)
{
  struct caml_ba_array * b = Caml_ba_array_val(vb);
  intnat index[CAML_BA_MAX_NUM_DIMS];
  int i;
  intnat offset;

  /* Check number of indices = number of dimensions of array
     (maybe not necessary if ML typing guarantees this) */
  if (nind != b->num_dims)
    caml_invalid_argument("Bigarray.set: wrong number of indices");
  /* Compute offset and check bounds */
  for (i = 0; i < b->num_dims; i++) index[i] = Long_val(vind[i]);
  offset = caml_ba_offset(b, index);
  /* Perform write */
  switch (b->flags & CAML_BA_KIND_MASK) {
  default:
    CAMLassert(0);
  case CAML_BA_FLOAT32:
    ((float *) b->data)[offset] = Double_val(newval); break;
  case CAML_BA_FLOAT64:
    ((double *) b->data)[offset] = Double_val(newval); break;
  case CAML_BA_CHAR:
  case CAML_BA_SINT8:
  case CAML_BA_UINT8:
    ((int8 *) b->data)[offset] = Int_val(newval); break;
  case CAML_BA_SINT16:
  case CAML_BA_UINT16:
    ((int16 *) b->data)[offset] = Int_val(newval); break;
  case CAML_BA_INT32:
    ((int32_t *) b->data)[offset] = Int32_val(newval); break;
  case CAML_BA_INT64:
    ((int64_t *) b->data)[offset] = Int64_val(newval); break;
  case CAML_BA_NATIVE_INT:
    ((intnat *) b->data)[offset] = Nativeint_val(newval); break;
  case CAML_BA_CAML_INT:
    ((intnat *) b->data)[offset] = Long_val(newval); break;
  case CAML_BA_COMPLEX32:
    { float * p = ((float *) b->data) + offset * 2;
      p[0] = Double_field(newval, 0);
      p[1] = Double_field(newval, 1);
      break; }
  case CAML_BA_COMPLEX64:
    { double * p = ((double *) b->data) + offset * 2;
      p[0] = Double_field(newval, 0);
      p[1] = Double_field(newval, 1);
      break; }
  }
  return Val_unit;
}

CAMLprim value caml_ba_set_1(value vb, value vind1, value newval)
{
  return caml_ba_set_aux(vb, &vind1, 1, newval);
}

CAMLprim value caml_ba_set_2(value vb, value vind1, value vind2, value newval)
{
  value vind[2];
  vind[0] = vind1; vind[1] = vind2;
  return caml_ba_set_aux(vb, vind, 2, newval);
}

CAMLprim value caml_ba_set_3(value vb, value vind1, value vind2, value vind3,
                     value newval)
{
  value vind[3];
  vind[0] = vind1; vind[1] = vind2; vind[2] = vind3;
  return caml_ba_set_aux(vb, vind, 3, newval);
}

#if 0
CAMLprim value caml_ba_set_4(value vb, value vind1, value vind2,
                     value vind3, value vind4, value newval)
{
  value vind[4];
  vind[0] = vind1; vind[1] = vind2; vind[2] = vind3; vind[3] = vind4;
  return caml_ba_set_aux(vb, vind, 4, newval);
}

CAMLprim value caml_ba_set_5(value vb, value vind1, value vind2,
                     value vind3, value vind4, value vind5, value newval)
{
  value vind[5];
  vind[0] = vind1; vind[1] = vind2; vind[2] = vind3;
  vind[3] = vind4; vind[4] = vind5;
  return caml_ba_set_aux(vb, vind, 5, newval);
}

CAMLprim value caml_ba_set_6(value vb, value vind1, value vind2,
                     value vind3, value vind4, value vind5,
                     value vind6, value newval)
{
  value vind[6];
  vind[0] = vind1; vind[1] = vind2; vind[2] = vind3;
  vind[3] = vind4; vind[4] = vind5; vind[5] = vind6;
  return caml_ba_set_aux(vb, vind, 6, newval);
}

value caml_ba_set_N(value vb, value * vind, int nargs)
{
  return caml_ba_set_aux(vb, vind, nargs - 1, vind[nargs - 1]);
}
#endif

CAMLprim value caml_ba_set_generic(value vb, value vind, value newval)
{
  return caml_ba_set_aux(vb, &Field(vind, 0), Wosize_val(vind), newval);
}

CAMLprim value caml_ba_uint8_set16(value vb, value vind, value newval)
{
  unsigned char b1, b2;
  intnat val;
  intnat idx = Long_val(vind);
  struct caml_ba_array * b = Caml_ba_array_val(vb);
  if (idx < 0 || idx >= b->dim[0] - 1) caml_array_bound_error();
  val = Long_val(newval);
#ifdef ARCH_BIG_ENDIAN
  b1 = 0xFF & val >> 8;
  b2 = 0xFF & val;
#else
  b2 = 0xFF & val >> 8;
  b1 = 0xFF & val;
#endif
  ((unsigned char*) b->data)[idx] = b1;
  ((unsigned char*) b->data)[idx+1] = b2;
  return Val_unit;
}

CAMLprim value caml_ba_uint8_set32(value vb, value vind, value newval)
{
  unsigned char b1, b2, b3, b4;
  intnat idx = Long_val(vind);
  intnat val;
  struct caml_ba_array * b = Caml_ba_array_val(vb);
  if (idx < 0 || idx >= b->dim[0] - 3) caml_array_bound_error();
  val = Int32_val(newval);
#ifdef ARCH_BIG_ENDIAN
  b1 = 0xFF & val >> 24;
  b2 = 0xFF & val >> 16;
  b3 = 0xFF & val >> 8;
  b4 = 0xFF & val;
#else
  b4 = 0xFF & val >> 24;
  b3 = 0xFF & val >> 16;
  b2 = 0xFF & val >> 8;
  b1 = 0xFF & val;
#endif
  ((unsigned char*) b->data)[idx] = b1;
  ((unsigned char*) b->data)[idx+1] = b2;
  ((unsigned char*) b->data)[idx+2] = b3;
  ((unsigned char*) b->data)[idx+3] = b4;
  return Val_unit;
}

CAMLprim value caml_ba_uint8_set64(value vb, value vind, value newval)
{
  unsigned char b1, b2, b3, b4, b5, b6, b7, b8;
  intnat idx = Long_val(vind);
  int64_t val;
  struct caml_ba_array * b = Caml_ba_array_val(vb);
  if (idx < 0 || idx >= b->dim[0] - 7) caml_array_bound_error();
  val = Int64_val(newval);
#ifdef ARCH_BIG_ENDIAN
  b1 = 0xFF & val >> 56;
  b2 = 0xFF & val >> 48;
  b3 = 0xFF & val >> 40;
  b4 = 0xFF & val >> 32;
  b5 = 0xFF & val >> 24;
  b6 = 0xFF & val >> 16;
  b7 = 0xFF & val >> 8;
  b8 = 0xFF & val;
#else
  b8 = 0xFF & val >> 56;
  b7 = 0xFF & val >> 48;
  b6 = 0xFF & val >> 40;
  b5 = 0xFF & val >> 32;
  b4 = 0xFF & val >> 24;
  b3 = 0xFF & val >> 16;
  b2 = 0xFF & val >> 8;
  b1 = 0xFF & val;
#endif
  ((unsigned char*) b->data)[idx] = b1;
  ((unsigned char*) b->data)[idx+1] = b2;
  ((unsigned char*) b->data)[idx+2] = b3;
  ((unsigned char*) b->data)[idx+3] = b4;
  ((unsigned char*) b->data)[idx+4] = b5;
  ((unsigned char*) b->data)[idx+5] = b6;
  ((unsigned char*) b->data)[idx+6] = b7;
  ((unsigned char*) b->data)[idx+7] = b8;
  return Val_unit;
}

/* Return the number of dimensions of a big array */

CAMLprim value caml_ba_num_dims(value vb)
{
  struct caml_ba_array * b = Caml_ba_array_val(vb);
  return Val_long(b->num_dims);
}

/* Return the n-th dimension of a big array */

CAMLprim value caml_ba_dim(value vb, value vn)
{
  struct caml_ba_array * b = Caml_ba_array_val(vb);
  intnat n = Long_val(vn);
  if (n < 0 || n >= b->num_dims) caml_invalid_argument("Bigarray.dim");
  return Val_long(b->dim[n]);
}

CAMLprim value caml_ba_dim_1(value vb)
{
  return caml_ba_dim(vb, Val_int(0));
}

CAMLprim value caml_ba_dim_2(value vb)
{
  return caml_ba_dim(vb, Val_int(1));
}

CAMLprim value caml_ba_dim_3(value vb)
{
  return caml_ba_dim(vb, Val_int(2));
}

/* Return the kind of a big array */

CAMLprim value caml_ba_kind(value vb)
{
  return Val_caml_ba_kind(Caml_ba_array_val(vb)->flags & CAML_BA_KIND_MASK);
}

/* Return the layout of a big array */

CAMLprim value caml_ba_layout(value vb)
{
  int layout = Caml_ba_array_val(vb)->flags & CAML_BA_LAYOUT_MASK;
  return Val_caml_ba_layout(layout);
}

/* Create / update proxy to indicate that b2 is a sub-array of b1 */

static void caml_ba_update_proxy(struct caml_ba_array * b1,
                                 struct caml_ba_array * b2)
{
  struct caml_ba_proxy * proxy;
  /* Nothing to do for un-managed arrays */
  if ((b1->flags & CAML_BA_MANAGED_MASK) == CAML_BA_EXTERNAL) return;
  if (b1->proxy != NULL) {
    /* If b1 is already a proxy for a larger array, increment refcount of
       proxy */
    b2->proxy = b1->proxy;
    ++ b1->proxy->refcount;
  } else {
    /* Otherwise, create proxy and attach it to both b1 and b2 */
    proxy = malloc(sizeof(struct caml_ba_proxy));
    if (proxy == NULL) caml_raise_out_of_memory();
    proxy->refcount = 2;      /* original array + sub array */
    proxy->data = b1->data;
    proxy->size =
      b1->flags & CAML_BA_MAPPED_FILE ? caml_ba_byte_size(b1) : 0;
    b1->proxy = proxy;
    b2->proxy = proxy;
  }
}

/* Slicing */

CAMLprim value caml_ba_slice(value vb, value vind)
{
  CAMLparam2 (vb, vind);
  #define b ((struct caml_ba_array *) Caml_ba_array_val(vb))
  CAMLlocal1 (res);
  intnat index[CAML_BA_MAX_NUM_DIMS];
  int num_inds, i;
  intnat offset;
  intnat * sub_dims;
  char * sub_data;

  /* Check number of indices <= number of dimensions of array */
  num_inds = Wosize_val(vind);
  if (num_inds > b->num_dims)
    caml_invalid_argument("Bigarray.slice: too many indices");
  /* Compute offset and check bounds */
  if ((b->flags & CAML_BA_LAYOUT_MASK) == CAML_BA_C_LAYOUT) {
    /* We slice from the left */
    for (i = 0; i < num_inds; i++) index[i] = Long_val(Field(vind, i));
    for (/*nothing*/; i < b->num_dims; i++) index[i] = 0;
    offset = caml_ba_offset(b, index);
    sub_dims = b->dim + num_inds;
  } else {
    /* We slice from the right */
    for (i = 0; i < num_inds; i++)
      index[b->num_dims - num_inds + i] = Long_val(Field(vind, i));
    for (i = 0; i < b->num_dims - num_inds; i++) index[i] = 1;
    offset = caml_ba_offset(b, index);
    sub_dims = b->dim;
  }
  sub_data =
    (char *) b->data +
    offset * caml_ba_element_size[b->flags & CAML_BA_KIND_MASK];
  /* Allocate an OCaml bigarray to hold the result */
  res = caml_ba_alloc(b->flags, b->num_dims - num_inds, sub_data, sub_dims);
  /* Create or update proxy in case of managed bigarray */
  caml_ba_update_proxy(b, Caml_ba_array_val(res));
  /* Return result */
  CAMLreturn (res);

  #undef b
}

/* Changing the layout of an array (memory is shared) */

CAMLprim value caml_ba_change_layout(value vb, value vlayout)
{
  CAMLparam2 (vb, vlayout);
  CAMLlocal1 (res);
  #define b ((struct caml_ba_array *) Caml_ba_array_val(vb))
  /* if the layout is different, change the flags and reverse the dimensions */
  if (Caml_ba_layout_val(vlayout) != (b->flags & CAML_BA_LAYOUT_MASK)) {
    /* change the flags to reflect the new layout */
    int flags = (b->flags & (CAML_BA_KIND_MASK | CAML_BA_MANAGED_MASK))
                 | Caml_ba_layout_val(vlayout);
    /* reverse the dimensions */
    intnat new_dim[CAML_BA_MAX_NUM_DIMS];
    unsigned int i;
    for(i = 0; i < b->num_dims; i++) new_dim[i] = b->dim[b->num_dims - i - 1];
    res = caml_ba_alloc(flags, b->num_dims, b->data, new_dim);
    caml_ba_update_proxy(b, Caml_ba_array_val(res));
    CAMLreturn(res);
  } else {
    /* otherwise, do nothing */
    CAMLreturn(vb);
  }
  #undef b
}


/* Extracting a sub-array of same number of dimensions */

CAMLprim value caml_ba_sub(value vb, value vofs, value vlen)
{
  CAMLparam3 (vb, vofs, vlen);
  CAMLlocal1 (res);
  #define b ((struct caml_ba_array *) Caml_ba_array_val(vb))
  intnat ofs = Long_val(vofs);
  intnat len = Long_val(vlen);
  int i, changed_dim;
  intnat mul;
  char * sub_data;

  /* Compute offset and check bounds */
  if ((b->flags & CAML_BA_LAYOUT_MASK) == CAML_BA_C_LAYOUT) {
    /* We reduce the first dimension */
    mul = 1;
    for (i = 1; i < b->num_dims; i++) mul *= b->dim[i];
    changed_dim = 0;
  } else {
    /* We reduce the last dimension */
    mul = 1;
    for (i = 0; i < b->num_dims - 1; i++) mul *= b->dim[i];
    changed_dim = b->num_dims - 1;
    ofs--;                      /* Fortran arrays start at 1 */
  }
  if (ofs < 0 || len < 0 || ofs + len > b->dim[changed_dim])
    caml_invalid_argument("Bigarray.sub: bad sub-array");
  sub_data =
    (char *) b->data +
    ofs * mul * caml_ba_element_size[b->flags & CAML_BA_KIND_MASK];
  /* Allocate an OCaml bigarray to hold the result */
  res = caml_ba_alloc(b->flags, b->num_dims, sub_data, b->dim);
  /* Doctor the changed dimension */
  Caml_ba_array_val(res)->dim[changed_dim] = len;
  /* Create or update proxy in case of managed bigarray */
  caml_ba_update_proxy(b, Caml_ba_array_val(res));
  /* Return result */
  CAMLreturn (res);

  #undef b
}

/* Copying a big array into another one */

#define LEAVE_RUNTIME_OP_CUTOFF 4096
#define is_mmapped(ba) ((ba)->flags & CAML_BA_MAPPED_FILE)

CAMLprim value caml_ba_blit(value vsrc, value vdst)
{
  CAMLparam2(vsrc, vdst);
  struct caml_ba_array * src = Caml_ba_array_val(vsrc);
  struct caml_ba_array * dst = Caml_ba_array_val(vdst);
  void *src_data = src->data;
  void *dst_data = dst->data;
  int i;
  intnat num_bytes;
  int leave_runtime;

  /* Check same numbers of dimensions and same dimensions */
  if (src->num_dims != dst->num_dims) goto blit_error;
  for (i = 0; i < src->num_dims; i++)
    if (src->dim[i] != dst->dim[i]) goto blit_error;
  /* Compute number of bytes in array data */
  num_bytes =
    caml_ba_num_elts(src)
    * caml_ba_element_size[src->flags & CAML_BA_KIND_MASK];
  leave_runtime =
    (
      (num_bytes >= LEAVE_RUNTIME_OP_CUTOFF*sizeof(long))
      || is_mmapped(src)
      || is_mmapped(dst)
    );
  /* Do the copying */
  if (leave_runtime) caml_enter_blocking_section();
  memmove (dst_data, src_data, num_bytes);
  if (leave_runtime) caml_leave_blocking_section();
  CAMLreturn (Val_unit);
 blit_error:
  caml_invalid_argument("Bigarray.blit: dimension mismatch");
  CAMLreturn (Val_unit);              /* not reached */
}

/* Filling a big array with a given value */

#define FILL_GEN_LOOP(n_ops, loop) do{                                       \
  int leave_runtime = ((n_ops >= LEAVE_RUNTIME_OP_CUTOFF) || is_mmapped(b)); \
  if (leave_runtime) caml_enter_blocking_section();                          \
  loop;                                                                      \
  if (leave_runtime) caml_leave_blocking_section();                          \
}while(0)

#define FILL_SCALAR_LOOP                                        \
  FILL_GEN_LOOP(num_elts,                                       \
    for (p = data; num_elts > 0; p++, num_elts--) *p = init)

#define FILL_COMPLEX_LOOP                                                    \
  FILL_GEN_LOOP(num_elts + num_elts,                                         \
    for (p = data; num_elts > 0; num_elts--) { *p++ = init0; *p++ = init1; })

CAMLprim value caml_ba_fill(value vb, value vinit)
{
  CAMLparam1(vb);
  struct caml_ba_array * b = Caml_ba_array_val(vb);
  void *data = b->data;
  intnat num_elts = caml_ba_num_elts(b);

  switch (b->flags & CAML_BA_KIND_MASK) {
  default:
    CAMLassert(0);
  case CAML_BA_FLOAT32: {
    float init = Double_val(vinit);
    float * p;
    FILL_SCALAR_LOOP;
    break;
  }
  case CAML_BA_FLOAT64: {
    double init = Double_val(vinit);
    double * p;
    FILL_SCALAR_LOOP;
    break;
  }
  case CAML_BA_CHAR:
  case CAML_BA_SINT8:
  case CAML_BA_UINT8: {
    int init = Int_val(vinit);
    unsigned char * p;
    FILL_SCALAR_LOOP;
    break;
  }
  case CAML_BA_SINT16:
  case CAML_BA_UINT16: {
    int init = Int_val(vinit);
    int16 * p;
    FILL_SCALAR_LOOP;
    break;
  }
  case CAML_BA_INT32: {
    int32_t init = Int32_val(vinit);
    int32_t * p;
    FILL_SCALAR_LOOP;
    break;
  }
  case CAML_BA_INT64: {
    int64_t init = Int64_val(vinit);
    int64_t * p;
    FILL_SCALAR_LOOP;
    break;
  }
  case CAML_BA_NATIVE_INT: {
    intnat init = Nativeint_val(vinit);
    intnat * p;
    FILL_SCALAR_LOOP;
    break;
  }
  case CAML_BA_CAML_INT: {
    intnat init = Long_val(vinit);
    intnat * p;
    FILL_SCALAR_LOOP;
    break;
  }
  case CAML_BA_COMPLEX32: {
    float init0 = Double_field(vinit, 0);
    float init1 = Double_field(vinit, 1);
    float * p;
    FILL_COMPLEX_LOOP;
    break;
  }
  case CAML_BA_COMPLEX64: {
    double init0 = Double_field(vinit, 0);
    double init1 = Double_field(vinit, 1);
    double * p;
    FILL_COMPLEX_LOOP;
    break;
  }
  }
  CAMLreturn (Val_unit);
}

/* Reshape an array: change dimensions and number of dimensions, preserving
   array contents */

CAMLprim value caml_ba_reshape(value vb, value vdim)
{
  CAMLparam2 (vb, vdim);
  CAMLlocal1 (res);
#define b ((struct caml_ba_array *) Caml_ba_array_val(vb))
  intnat dim[CAML_BA_MAX_NUM_DIMS];
  mlsize_t num_dims;
  uintnat num_elts;
  int i;

  num_dims = Wosize_val(vdim);
  /* here num_dims is unsigned (mlsize_t) so no need to check (num_dims >= 0) */
  if (num_dims > CAML_BA_MAX_NUM_DIMS)
    caml_invalid_argument("Bigarray.reshape: bad number of dimensions");
  num_elts = 1;
  for (i = 0; i < num_dims; i++) {
    dim[i] = Long_val(Field(vdim, i));
    if (dim[i] < 0)
      caml_invalid_argument("Bigarray.reshape: negative dimension");
    num_elts *= dim[i];
  }
  /* Check that sizes agree */
  if (num_elts != caml_ba_num_elts(b))
    caml_invalid_argument("Bigarray.reshape: size mismatch");
  /* Create bigarray with same data and new dimensions */
  res = caml_ba_alloc(b->flags, num_dims, b->data, dim);
  /* Create or update proxy in case of managed bigarray */
  caml_ba_update_proxy(b, Caml_ba_array_val(res));
  /* Return result */
  CAMLreturn (res);

#undef b
}
