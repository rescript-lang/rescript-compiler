/***********************************************************************/
/*                                                                     */
/*                           OCaml                                     */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Operations on arrays */

#include <string.h>
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

CAMLexport mlsize_t caml_array_length(value array)
{
  if (Tag_val(array) == Double_array_tag)
    return Wosize_val(array) / Double_wosize;
  else
    return Wosize_val(array);
}

CAMLexport int caml_is_double_array(value array)
{
  return (Tag_val(array) == Double_array_tag);
}

CAMLprim value caml_array_get_addr(value array, value index)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array)) caml_array_bound_error();
  return Field(array, idx);
}

CAMLprim value caml_array_get_float(value array, value index)
{
  intnat idx = Long_val(index);
  double d;
  value res;

  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    caml_array_bound_error();
  d = Double_field(array, idx);
#define Setup_for_gc
#define Restore_after_gc
  Alloc_small(res, Double_wosize, Double_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  Store_double_val(res, d);
  return res;
}

CAMLprim value caml_array_get(value array, value index)
{
  if (Tag_val(array) == Double_array_tag)
    return caml_array_get_float(array, index);
  else
    return caml_array_get_addr(array, index);
}

CAMLprim value caml_array_set_addr(value array, value index, value newval)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array)) caml_array_bound_error();
  Modify(&Field(array, idx), newval);
  return Val_unit;
}

CAMLprim value caml_array_set_float(value array, value index, value newval)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    caml_array_bound_error();
  Store_double_field(array, idx, Double_val(newval));
  return Val_unit;
}

CAMLprim value caml_array_set(value array, value index, value newval)
{
  if (Tag_val(array) == Double_array_tag)
    return caml_array_set_float(array, index, newval);
  else
    return caml_array_set_addr(array, index, newval);
}

CAMLprim value caml_array_unsafe_get_float(value array, value index)
{
  double d;
  value res;

  d = Double_field(array, Long_val(index));
#define Setup_for_gc
#define Restore_after_gc
  Alloc_small(res, Double_wosize, Double_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  Store_double_val(res, d);
  return res;
}

CAMLprim value caml_array_unsafe_get(value array, value index)
{
  if (Tag_val(array) == Double_array_tag)
    return caml_array_unsafe_get_float(array, index);
  else
    return Field(array, Long_val(index));
}

CAMLprim value caml_array_unsafe_set_addr(value array, value index,value newval)
{
  intnat idx = Long_val(index);
  Modify(&Field(array, idx), newval);
  return Val_unit;
}

CAMLprim value caml_array_unsafe_set_float(value array,value index,value newval)
{
  Store_double_field(array, Long_val(index), Double_val(newval));
  return Val_unit;
}

CAMLprim value caml_array_unsafe_set(value array, value index, value newval)
{
  if (Tag_val(array) == Double_array_tag)
    return caml_array_unsafe_set_float(array, index, newval);
  else
    return caml_array_unsafe_set_addr(array, index, newval);
}

CAMLprim value caml_make_float_vect(value len)
{
  mlsize_t wosize = Long_val(len) * Double_wosize;
  value result;
  if (wosize == 0)
    return Atom(0);
  else if (wosize <= Max_young_wosize){
#define Setup_for_gc
#define Restore_after_gc
    Alloc_small (result, wosize, Double_array_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  }else if (wosize > Max_wosize)
    caml_invalid_argument("Array.make_float");
  else {
    result = caml_alloc_shr (wosize, Double_array_tag);
    result = caml_check_urgent_gc (result);
  }
  return result;
}

CAMLprim value caml_make_vect(value len, value init)
{
  CAMLparam2 (len, init);
  CAMLlocal1 (res);
  mlsize_t size, wsize, i;
  double d;

  size = Long_val(len);
  if (size == 0) {
    res = Atom(0);
  }
  else if (Is_block(init)
           && Is_in_value_area(init)
           && Tag_val(init) == Double_tag) {
    d = Double_val(init);
    wsize = size * Double_wosize;
    if (wsize > Max_wosize) caml_invalid_argument("Array.make");
    res = caml_alloc(wsize, Double_array_tag);
    for (i = 0; i < size; i++) {
      Store_double_field(res, i, d);
    }
  } else {
    if (size > Max_wosize) caml_invalid_argument("Array.make");
    if (size < Max_young_wosize) {
      res = caml_alloc_small(size, 0);
      for (i = 0; i < size; i++) Field(res, i) = init;
    }
    else if (Is_block(init) && Is_young(init)) {
      caml_minor_collection();
      res = caml_alloc_shr(size, 0);
      for (i = 0; i < size; i++) Field(res, i) = init;
      res = caml_check_urgent_gc (res);
    }
    else {
      res = caml_alloc_shr(size, 0);
      for (i = 0; i < size; i++) caml_initialize(&Field(res, i), init);
      res = caml_check_urgent_gc (res);
    }
  }
  CAMLreturn (res);
}

CAMLprim value caml_make_array(value init)
{
  CAMLparam1 (init);
  mlsize_t wsize, size, i;
  CAMLlocal2 (v, res);

  size = Wosize_val(init);
  if (size == 0) {
    CAMLreturn (init);
  } else {
    v = Field(init, 0);
    if (Is_long(v)
        || ! Is_in_value_area(v)
        || Tag_val(v) != Double_tag) {
      CAMLreturn (init);
    } else {
      wsize = size * Double_wosize;
      if (wsize <= Max_young_wosize) {
        res = caml_alloc_small(wsize, Double_array_tag);
      } else {
        res = caml_alloc_shr(wsize, Double_array_tag);
        res = caml_check_urgent_gc(res);
      }
      for (i = 0; i < size; i++) {
        Store_double_field(res, i, Double_val(Field(init, i)));
      }
      CAMLreturn (res);
    }
  }
}

/* Blitting */

CAMLprim value caml_array_blit(value a1, value ofs1, value a2, value ofs2,
                               value n)
{
  value * src, * dst;
  intnat count;

  if (Tag_val(a2) == Double_array_tag) {
    /* Arrays of floats.  The values being copied are floats, not
       pointer, so we can do a direct copy.  memmove takes care of
       potential overlap between the copied areas. */
    memmove((double *)a2 + Long_val(ofs2),
            (double *)a1 + Long_val(ofs1),
            Long_val(n) * sizeof(double));
    return Val_unit;
  }
  if (Is_young(a2)) {
    /* Arrays of values, destination is in young generation.
       Here too we can do a direct copy since this cannot create
       old-to-young pointers, nor mess up with the incremental major GC.
       Again, memmove takes care of overlap. */
    memmove(&Field(a2, Long_val(ofs2)),
            &Field(a1, Long_val(ofs1)),
            Long_val(n) * sizeof(value));
    return Val_unit;
  }
  /* Array of values, destination is in old generation.
     We must use caml_modify.  */
  count = Long_val(n);
  if (a1 == a2 && Long_val(ofs1) < Long_val(ofs2)) {
    /* Copy in descending order */
    for (dst = &Field(a2, Long_val(ofs2) + count - 1),
           src = &Field(a1, Long_val(ofs1) + count - 1);
         count > 0;
         count--, src--, dst--) {
      caml_modify(dst, *src);
    }
  } else {
    /* Copy in ascending order */
    for (dst = &Field(a2, Long_val(ofs2)), src = &Field(a1, Long_val(ofs1));
         count > 0;
         count--, src++, dst++) {
      caml_modify(dst, *src);
    }
  }
  /* Many caml_modify in a row can create a lot of old-to-young refs.
     Give the minor GC a chance to run if it needs to. */
  caml_check_urgent_gc(Val_unit);
  return Val_unit;
}

/* A generic function for extraction and concatenation of sub-arrays */

static value caml_array_gather(intnat num_arrays,
                               value arrays[/*num_arrays*/],
                               intnat offsets[/*num_arrays*/],
                               intnat lengths[/*num_arrays*/])
{
  CAMLparamN(arrays, num_arrays);
  value res;                    /* no need to register it as a root */
  int isfloat;
  mlsize_t i, size, wsize, count, pos;
  value * src;

  /* Determine total size and whether result array is an array of floats */
  size = 0;
  isfloat = 0;
  for (i = 0; i < num_arrays; i++) {
    size += lengths[i];
    if (Tag_val(arrays[i]) == Double_array_tag) isfloat = 1;
  }
  if (size == 0) {
    /* If total size = 0, just return empty array */
    res = Atom(0);
  }
  else if (isfloat) {
    /* This is an array of floats.  We can use memcpy directly. */
    wsize = size * Double_wosize;
    if (wsize > Max_wosize) caml_invalid_argument("Array.concat");
    res = caml_alloc(wsize, Double_array_tag);
    for (i = 0, pos = 0; i < num_arrays; i++) {
      memcpy((double *)res + pos,
             (double *)arrays[i] + offsets[i],
             lengths[i] * sizeof(double));
      pos += lengths[i];
    }
    Assert(pos == size);
  }
  else if (size > Max_wosize) {
    /* Array of values, too big. */
    caml_invalid_argument("Array.concat");
  }
  else if (size < Max_young_wosize) {
    /* Array of values, small enough to fit in young generation.
       We can use memcpy directly. */
    res = caml_alloc_small(size, 0);
    for (i = 0, pos = 0; i < num_arrays; i++) {
      memcpy(&Field(res, pos),
             &Field(arrays[i], offsets[i]),
             lengths[i] * sizeof(value));
      pos += lengths[i];
    }
    Assert(pos == size);
  } else {
    /* Array of values, must be allocated in old generation and filled
       using caml_initialize. */
    res = caml_alloc_shr(size, 0);
    pos = 0;
    for (i = 0, pos = 0; i < num_arrays; i++) {
      for (src = &Field(arrays[i], offsets[i]), count = lengths[i];
           count > 0;
           count--, src++, pos++) {
        caml_initialize(&Field(res, pos), *src);
      }
    }
    Assert(pos == size);

    /* Many caml_initialize in a row can create a lot of old-to-young
       refs.  Give the minor GC a chance to run if it needs to. */
    res = caml_check_urgent_gc(res);
  }
  CAMLreturn (res);
}

CAMLprim value caml_array_sub(value a, value ofs, value len)
{
  value arrays[1] = { a };
  intnat offsets[1] = { Long_val(ofs) };
  intnat lengths[1] = { Long_val(len) };
  return caml_array_gather(1, arrays, offsets, lengths);
}

CAMLprim value caml_array_append(value a1, value a2)
{
  value arrays[2] = { a1, a2 };
  intnat offsets[2] = { 0, 0 };
  intnat lengths[2] = { caml_array_length(a1), caml_array_length(a2) };
  return caml_array_gather(2, arrays, offsets, lengths);
}

CAMLprim value caml_array_concat(value al)
{
#define STATIC_SIZE 16
  value static_arrays[STATIC_SIZE], * arrays;
  intnat static_offsets[STATIC_SIZE], * offsets;
  intnat static_lengths[STATIC_SIZE], * lengths;
  intnat n, i;
  value l, res;

  /* Length of list = number of arrays */
  for (n = 0, l = al; l != Val_int(0); l = Field(l, 1)) n++;
  /* Allocate extra storage if too many arrays */
  if (n <= STATIC_SIZE) {
    arrays = static_arrays;
    offsets = static_offsets;
    lengths = static_lengths;
  } else {
    arrays = caml_stat_alloc(n * sizeof(value));
    offsets = caml_stat_alloc(n * sizeof(intnat));
    lengths = caml_stat_alloc(n * sizeof(value));
  }
  /* Build the parameters to caml_array_gather */
  for (i = 0, l = al; l != Val_int(0); l = Field(l, 1), i++) {
    arrays[i] = Field(l, 0);
    offsets[i] = 0;
    lengths[i] = caml_array_length(Field(l, 0));
  }
  /* Do the concatenation */
  res = caml_array_gather(n, arrays, offsets, lengths);
  /* Free the extra storage if needed */
  if (n > STATIC_SIZE) {
    caml_stat_free(arrays);
    caml_stat_free(offsets);
    caml_stat_free(lengths);
  }
  return res;
}
