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

/* 1. Allocation functions doing the same work as the macros in the
      case where [Setup_for_gc] and [Restore_after_gc] are no-ops.
   2. Convenience functions related to allocation.
*/

#include <string.h>
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/stacks.h"

#define Setup_for_gc
#define Restore_after_gc

CAMLexport value caml_alloc (mlsize_t wosize, tag_t tag)
{
  value result;
  mlsize_t i;

  CAMLassert (tag < 256);
  CAMLassert (tag != Infix_tag);
  if (wosize <= Max_young_wosize){
    if (wosize == 0){
      result = Atom (tag);
    }else{
      Alloc_small (result, wosize, tag);
      if (tag < No_scan_tag){
        for (i = 0; i < wosize; i++) Field (result, i) = Val_unit;
      }
    }
  }else{
    result = caml_alloc_shr (wosize, tag);
    if (tag < No_scan_tag){
      for (i = 0; i < wosize; i++) Field (result, i) = Val_unit;
    }
    result = caml_check_urgent_gc (result);
  }
  return result;
}

CAMLexport value caml_alloc_small (mlsize_t wosize, tag_t tag)
{
  value result;

  CAMLassert (wosize > 0);
  CAMLassert (wosize <= Max_young_wosize);
  CAMLassert (tag < 256);
  Alloc_small (result, wosize, tag);
  return result;
}

CAMLexport value caml_alloc_small_with_my_or_given_profinfo (mlsize_t wosize,
  tag_t tag, uintnat profinfo)
{
  if (profinfo == 0) {
    return caml_alloc_small(wosize, tag);
  }
  else {
    value result;

    CAMLassert (wosize > 0);
    CAMLassert (wosize <= Max_young_wosize);
    CAMLassert (tag < 256);
    Alloc_small_with_profinfo (result, wosize, tag, profinfo);
    return result;
  }
}

/* [n] is a number of words (fields) */
CAMLexport value caml_alloc_tuple(mlsize_t n)
{
  return caml_alloc(n, 0);
}

/* [len] is a number of bytes (chars) */
CAMLexport value caml_alloc_string (mlsize_t len)
{
  value result;
  mlsize_t offset_index;
  mlsize_t wosize = (len + sizeof (value)) / sizeof (value);

  if (wosize <= Max_young_wosize) {
    Alloc_small (result, wosize, String_tag);
  }else{
    result = caml_alloc_shr (wosize, String_tag);
    result = caml_check_urgent_gc (result);
  }
  Field (result, wosize - 1) = 0;
  offset_index = Bsize_wsize (wosize) - 1;
  Byte (result, offset_index) = offset_index - len;
  return result;
}

/* [len] is a number of bytes (chars) */
CAMLexport value caml_alloc_initialized_string (mlsize_t len, const char *p)
{
  value result = caml_alloc_string (len);
  memcpy((char *)String_val(result), p, len);
  return result;
}

/* [len] is a number of words.
   [mem] and [max] are relative (without unit).
*/
CAMLexport value caml_alloc_final (mlsize_t len, final_fun fun,
                                   mlsize_t mem, mlsize_t max)
{
  return caml_alloc_custom(caml_final_custom_operations(fun),
                           len * sizeof(value), mem, max);
}

CAMLexport value caml_copy_string(char const *s)
{
  int len;
  value res;

  len = strlen(s);
  res = caml_alloc_initialized_string(len, s);
  return res;
}

CAMLexport value caml_alloc_array(value (*funct)(char const *),
                                  char const ** arr)
{
  CAMLparam0 ();
  mlsize_t nbr, n;
  CAMLlocal2 (v, result);

  nbr = 0;
  while (arr[nbr] != 0) nbr++;
  result = caml_alloc (nbr, 0);
  for (n = 0; n < nbr; n++) {
    /* The two statements below must be separate because of evaluation
       order (don't take the address &Field(result, n) before
       calling funct, which may cause a GC and move result). */
    v = funct(arr[n]);
    caml_modify(&Field(result, n), v);
  }
  CAMLreturn (result);
}

/* [len] is a number of floats */
CAMLprim value caml_alloc_float_array(mlsize_t len)
{
#ifdef FLAT_FLOAT_ARRAY
  mlsize_t wosize = len * Double_wosize;
  value result;
  /* For consistency with [caml_make_vect], which can't tell whether it should
     create a float array or not when the size is zero, the tag is set to
     zero when the size is zero. */
  if (wosize <= Max_young_wosize){
    if (wosize == 0)
      return Atom(0);
    else
      Alloc_small (result, wosize, Double_array_tag);
  }else {
    result = caml_alloc_shr (wosize, Double_array_tag);
    result = caml_check_urgent_gc (result);
  }
  return result;
#else
  return caml_alloc (len, 0);
#endif
}


CAMLexport value caml_copy_string_array(char const ** arr)
{
  return caml_alloc_array(caml_copy_string, arr);
}

CAMLexport int caml_convert_flag_list(value list, int *flags)
{
  int res;
  res = 0;
  while (list != Val_int(0)) {
    res |= flags[Int_val(Field(list, 0))];
    list = Field(list, 1);
  }
  return res;
}

/* For compiling let rec over values */

/* [size] is a [value] representing number of words (fields) */
CAMLprim value caml_alloc_dummy(value size)
{
  mlsize_t wosize = Long_val(size);
  return caml_alloc (wosize, 0);
}

/* [size] is a [value] representing number of words (fields) */
CAMLprim value caml_alloc_dummy_function(value size,value arity)
{
  /* the arity argument is used by the js_of_ocaml runtime */
  return caml_alloc_dummy(size);
}

/* [size] is a [value] representing number of floats. */
CAMLprim value caml_alloc_dummy_float (value size)
{
  mlsize_t wosize = Long_val(size) * Double_wosize;
  return caml_alloc (wosize, 0);
}

CAMLprim value caml_update_dummy(value dummy, value newval)
{
  mlsize_t size, i;
  tag_t tag;

  size = Wosize_val(newval);
  tag = Tag_val (newval);
  CAMLassert (size == Wosize_val(dummy));
  CAMLassert (tag < No_scan_tag || tag == Double_array_tag);

  Tag_val(dummy) = tag;
  if (tag == Double_array_tag){
    size = Wosize_val (newval) / Double_wosize;
    for (i = 0; i < size; i++){
      Store_double_flat_field (dummy, i, Double_flat_field (newval, i));
    }
  }else{
    for (i = 0; i < size; i++){
      caml_modify (&Field(dummy, i), Field(newval, i));
    }
  }
  return Val_unit;
}
