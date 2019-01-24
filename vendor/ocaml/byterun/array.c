/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
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

/* Operations on arrays */
#include <string.h>
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/signals.h"
/* Why is caml/spacetime.h included conditionnally sometimes and not here ? */
#include "caml/spacetime.h"

static const mlsize_t mlsize_t_max = -1;

/* returns number of elements (either fields or floats) */
/* [ 'a array -> int ] */
CAMLexport mlsize_t caml_array_length(value array)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return Wosize_val(array) / Double_wosize;
  else
#endif
    return Wosize_val(array);
}

CAMLexport int caml_is_double_array(value array)
{
  return (Tag_val(array) == Double_array_tag);
}

/* Note: the OCaml types on the following primitives will work both with
   and without the -no-flat-float-array configure-time option. If you
   respect them, your C code should work in both configurations.
*/

/* [ 'a array -> int -> 'a ] where 'a != float */
CAMLprim value caml_array_get_addr(value array, value index)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array)) caml_array_bound_error();
  return Field(array, idx);
}

/* [ float array -> int -> float ] */
CAMLprim value caml_array_get_float(value array, value index)
{
  intnat idx = Long_val(index);
#ifdef FLAT_FLOAT_ARRAY
  double d;
  value res;

  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    caml_array_bound_error();
  d = Double_flat_field(array, idx);
#define Setup_for_gc
#define Restore_after_gc
  Alloc_small(res, Double_wosize, Double_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  Store_double_val(res, d);
  return res;
#else
  CAMLassert (Tag_val (array) != Double_array_tag);
  if (idx < 0 || idx >= Wosize_val(array)) caml_array_bound_error();
  return Field(array, idx);
#endif /* FLAT_FLOAT_ARRAY */
}

/* [ 'a array -> int -> 'a ] */
CAMLprim value caml_array_get(value array, value index)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return caml_array_get_float(array, index);
#else
  CAMLassert (Tag_val(array) != Double_array_tag);
#endif
  return caml_array_get_addr(array, index);
}

/* [ floatarray -> int -> float ] */
CAMLprim value caml_floatarray_get(value array, value index)
{
  intnat idx = Long_val(index);
  double d;
  value res;

  CAMLassert (Tag_val(array) == Double_array_tag);
  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    caml_array_bound_error();
  d = Double_flat_field(array, idx);
#define Setup_for_gc
#define Restore_after_gc
  Alloc_small(res, Double_wosize, Double_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  Store_double_val(res, d);
  return res;
}

/* [ 'a array -> int -> 'a -> unit ] where 'a != float */
CAMLprim value caml_array_set_addr(value array, value index, value newval)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array)) caml_array_bound_error();
  Modify(&Field(array, idx), newval);
  return Val_unit;
}

/* [ float array -> int -> float -> unit ] */
CAMLprim value caml_array_set_float(value array, value index, value newval)
{
  intnat idx = Long_val(index);
#ifdef FLAT_FLOAT_ARRAY
  double d = Double_val (newval);
  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    caml_array_bound_error();
  Store_double_flat_field(array, idx, d);
#else
  CAMLassert (Tag_val (array) != Double_array_tag);
  if (idx < 0 || idx >= Wosize_val(array)) caml_array_bound_error();
  Modify(&Field(array, idx), newval);
#endif
  return Val_unit;
}

/* [ 'a array -> int -> 'a -> unit ] */
CAMLprim value caml_array_set(value array, value index, value newval)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return caml_array_set_float(array, index, newval);
#else
  CAMLassert (Tag_val(array) != Double_array_tag);
#endif
  return caml_array_set_addr(array, index, newval);
}

/* [ floatarray -> int -> float -> unit ] */
CAMLprim value caml_floatarray_set(value array, value index, value newval)
{
  intnat idx = Long_val(index);
  double d = Double_val (newval);
  CAMLassert (Tag_val(array) == Double_array_tag);
  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    caml_array_bound_error();
  Store_double_flat_field(array, idx, d);
  return Val_unit;
}

/* [ float array -> int -> float ] */
CAMLprim value caml_array_unsafe_get_float(value array, value index)
{
  intnat idx = Long_val (index);
#ifdef FLAT_FLOAT_ARRAY
  double d;
  value res;

  d = Double_flat_field(array, idx);
#define Setup_for_gc
#define Restore_after_gc
  Alloc_small(res, Double_wosize, Double_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  Store_double_val(res, d);
  return res;
#else /* FLAT_FLOAT_ARRAY */
  CAMLassert (Tag_val(array) != Double_array_tag);
  return Field(array, idx);
#endif /* FLAT_FLOAT_ARRAY */
}

/* [ 'a array -> int -> 'a ] */
CAMLprim value caml_array_unsafe_get(value array, value index)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return caml_array_unsafe_get_float(array, index);
#else
  CAMLassert (Tag_val(array) != Double_array_tag);
#endif
  return Field(array, Long_val(index));
}

/* [ floatarray -> int -> float ] */
CAMLprim value caml_floatarray_unsafe_get(value array, value index)
{
  intnat idx = Long_val(index);
  double d;
  value res;

  CAMLassert (Tag_val(array) == Double_array_tag);
  d = Double_flat_field(array, idx);
#define Setup_for_gc
#define Restore_after_gc
  Alloc_small(res, Double_wosize, Double_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  Store_double_val(res, d);
  return res;
}

/* [ 'a array -> int -> 'a -> unit ] where 'a != float */
CAMLprim value caml_array_unsafe_set_addr(value array, value index,value newval)
{
  intnat idx = Long_val(index);
  Modify(&Field(array, idx), newval);
  return Val_unit;
}

/* [ float array -> int -> float -> unit ] */
CAMLprim value caml_array_unsafe_set_float(value array,value index,value newval)
{
  intnat idx = Long_val(index);
#ifdef FLAT_FLOAT_ARRAY
  double d = Double_val (newval);
  Store_double_flat_field(array, idx, d);
#else
  Modify(&Field(array, idx), newval);
#endif
  return Val_unit;
}

/* [ 'a array -> int -> 'a -> unit ] */
CAMLprim value caml_array_unsafe_set(value array, value index, value newval)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return caml_array_unsafe_set_float(array, index, newval);
#else
  CAMLassert (Tag_val(array) != Double_array_tag);
#endif
  return caml_array_unsafe_set_addr(array, index, newval);
}

/* [ floatarray -> int -> float -> unit ] */
CAMLprim value caml_floatarray_unsafe_set(value array, value index,value newval)
{
  intnat idx = Long_val(index);
  double d = Double_val (newval);
  Store_double_flat_field(array, idx, d);
  return Val_unit;
}

/* [len] is a [value] representing number of floats. */
/* [ int -> floatarray ] */
CAMLprim value caml_floatarray_create(value len)
{
  mlsize_t wosize = Long_val(len) * Double_wosize;
  value result;
  if (wosize <= Max_young_wosize){
    if (wosize == 0)
      return Atom(0);
    else
#define Setup_for_gc
#define Restore_after_gc
      Alloc_small (result, wosize, Double_array_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  }else if (wosize > Max_wosize)
    caml_invalid_argument("Array.Floatarray.create");
  else {
    result = caml_alloc_shr (wosize, Double_array_tag);
    result = caml_check_urgent_gc (result);
  }
  return result;
}

/* [len] is a [value] representing number of floats */
/* [ int -> float array ] */
CAMLprim value caml_make_float_vect(value len)
{
#ifdef FLAT_FLOAT_ARRAY
  return caml_floatarray_create (len);
#else
  return caml_alloc (Long_val (len), 0);
#endif
}

/* [len] is a [value] representing number of words or floats */
/* Spacetime profiling assumes that this function is only called from OCaml. */
CAMLprim value caml_make_vect(value len, value init)
{
  CAMLparam2 (len, init);
  CAMLlocal1 (res);
  mlsize_t size, i;

  size = Long_val(len);
  if (size == 0) {
    res = Atom(0);
#ifdef FLAT_FLOAT_ARRAY
  } else if (Is_block(init)
           && Is_in_value_area(init)
           && Tag_val(init) == Double_tag) {
    mlsize_t wsize;
    double d;
    d = Double_val(init);
    wsize = size * Double_wosize;
    if (wsize > Max_wosize) caml_invalid_argument("Array.make");
    res = caml_alloc(wsize, Double_array_tag);
    for (i = 0; i < size; i++) {
      Store_double_flat_field(res, i, d);
    }
#endif
  } else {
    if (size <= Max_young_wosize) {
      uintnat profinfo;
      Get_my_profinfo_with_cached_backtrace(profinfo, size);
      res = caml_alloc_small_with_my_or_given_profinfo(size, 0, profinfo);
      for (i = 0; i < size; i++) Field(res, i) = init;
    }
    else if (size > Max_wosize) caml_invalid_argument("Array.make");
    else if (Is_block(init) && Is_young(init)) {
      /* We don't want to create so many major-to-minor references,
         so [init] is moved to the major heap by doing a minor GC. */
      CAML_INSTR_INT ("force_minor/make_vect@", 1);
      caml_request_minor_gc ();
      caml_gc_dispatch ();
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

/* This primitive is used internally by the compiler to compile
   explicit array expressions.
   For float arrays when FLAT_FLOAT_ARRAY is true, it takes an array of
   boxed floats and returns the corresponding flat-allocated [float array].
   In all other cases, it just returns its argument unchanged.
*/
CAMLprim value caml_make_array(value init)
{
#ifdef FLAT_FLOAT_ARRAY
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
        double d = Double_val(Field(init, i));
        Store_double_flat_field(res, i, d);
      }
      CAMLreturn (res);
    }
  }
#else
  return init;
#endif
}

/* Blitting */

CAMLprim value caml_array_blit(value a1, value ofs1, value a2, value ofs2,
                               value n)
{
  value * src, * dst;
  intnat count;

#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(a2) == Double_array_tag) {
    /* Arrays of floats.  The values being copied are floats, not
       pointer, so we can do a direct copy.  memmove takes care of
       potential overlap between the copied areas. */
    memmove((double *)a2 + Long_val(ofs2),
            (double *)a1 + Long_val(ofs1),
            Long_val(n) * sizeof(double));
    return Val_unit;
  }
#endif
  CAMLassert (Tag_val(a2) != Double_array_tag);
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
#ifdef FLAT_FLOAT_ARRAY
  int isfloat = 0;
  mlsize_t wsize;
#endif
  mlsize_t i, size, count, pos;
  value * src;

  /* Determine total size and whether result array is an array of floats */
  size = 0;
  for (i = 0; i < num_arrays; i++) {
    if (mlsize_t_max - lengths[i] < size) caml_invalid_argument("Array.concat");
    size += lengths[i];
#ifdef FLAT_FLOAT_ARRAY
    if (Tag_val(arrays[i]) == Double_array_tag) isfloat = 1;
#endif
  }
  if (size == 0) {
    /* If total size = 0, just return empty array */
    res = Atom(0);
  }
#ifdef FLAT_FLOAT_ARRAY
  else if (isfloat) {
    /* This is an array of floats.  We can use memcpy directly. */
    if (size > Max_wosize/Double_wosize) caml_invalid_argument("Array.concat");
    wsize = size * Double_wosize;
    res = caml_alloc(wsize, Double_array_tag);
    for (i = 0, pos = 0; i < num_arrays; i++) {
      memcpy((double *)res + pos,
             (double *)arrays[i] + offsets[i],
             lengths[i] * sizeof(double));
      pos += lengths[i];
    }
    CAMLassert(pos == size);
  }
#endif
  else if (size <= Max_young_wosize) {
    /* Array of values, small enough to fit in young generation.
       We can use memcpy directly. */
    res = caml_alloc_small(size, 0);
    for (i = 0, pos = 0; i < num_arrays; i++) {
      memcpy(&Field(res, pos),
             &Field(arrays[i], offsets[i]),
             lengths[i] * sizeof(value));
      pos += lengths[i];
    }
    CAMLassert(pos == size);
  }
  else if (size > Max_wosize) {
    /* Array of values, too big. */
    caml_invalid_argument("Array.concat");
  } else {
    /* Array of values, must be allocated in old generation and filled
       using caml_initialize. */
    res = caml_alloc_shr(size, 0);
    for (i = 0, pos = 0; i < num_arrays; i++) {
      for (src = &Field(arrays[i], offsets[i]), count = lengths[i];
           count > 0;
           count--, src++, pos++) {
        caml_initialize(&Field(res, pos), *src);
      }
    }
    CAMLassert(pos == size);

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
    offsets = caml_stat_alloc_noexc(n * sizeof(intnat));
    if (offsets == NULL) {
      caml_stat_free(arrays);
      caml_raise_out_of_memory();
    }
    lengths = caml_stat_alloc_noexc(n * sizeof(value));
    if (lengths == NULL) {
      caml_stat_free(offsets);
      caml_stat_free(arrays);
      caml_raise_out_of_memory();
    }
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
