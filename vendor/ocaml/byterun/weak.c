/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1997 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Operations on weak arrays and ephemerons (named ephe here)*/

#include <string.h>

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/weak.h"

value caml_ephe_list_head = 0;

static value ephe_dummy = 0;
value caml_ephe_none = (value) &ephe_dummy;

#if defined (NATIVE_CODE) && defined (NO_NAKED_POINTERS)
/** The minor heap is considered alive.
    Outside minor and major heap, x must be black.
*/
static inline int Is_Dead_during_clean(value x){
  CAMLassert (x != caml_ephe_none);
  CAMLassert (caml_gc_phase == Phase_clean);
  return Is_block (x) && !Is_young (x) && Is_white_val(x);
}
/** The minor heap doesn't have to be marked, outside they should
    already be black
*/
static inline int Must_be_Marked_during_mark(value x){
  CAMLassert (x != caml_ephe_none);
  CAMLassert (caml_gc_phase == Phase_mark);
  return Is_block (x) && !Is_young (x);
}
#else
static inline int Is_Dead_during_clean(value x){
  CAMLassert (x != caml_ephe_none);
  CAMLassert (caml_gc_phase == Phase_clean);
  return Is_block (x) && Is_in_heap (x) && Is_white_val(x);
}
static inline int Must_be_Marked_during_mark(value x){
  CAMLassert (x != caml_ephe_none); 
  CAMLassert (caml_gc_phase == Phase_mark);
  return Is_block (x) && Is_in_heap (x);
}
#endif


/* [len] is a value that represents a number of words (fields) */
CAMLprim value caml_ephe_create (value len)
{
  mlsize_t size, i;
  value res;

  size = Long_val (len) + 1 /* weak_list */ + 1 /* the value */;
  if (size <= 0 || size > Max_wosize) caml_invalid_argument ("Weak.create");
  res = caml_alloc_shr (size, Abstract_tag);
  for (i = 1; i < size; i++) Field (res, i) = caml_ephe_none;
  Field (res, CAML_EPHE_LINK_OFFSET) = caml_ephe_list_head;
  caml_ephe_list_head = res;
  return res;
}

CAMLprim value caml_weak_create (value len)
{
  return caml_ephe_create(len);
}

/**
   Specificity of the cleaning phase (Phase_clean):

   The dead keys must be removed from the ephemerons and data removed
   when one the keys is dead. Here we call it cleaning the ephemerons.
   A specific phase of the GC is dedicated to this, Phase_clean. This
   phase is just after the mark phase, so the white values are dead
   values. It iterates the function caml_ephe_clean through all the
   ephemerons.

   However the GC is incremental and ocaml code can run on the middle
   of this cleaning phase. In order to respect the semantic of the
   ephemerons concerning dead values, the getter and setter must work
   as if the cleaning of all the ephemerons have been done at once.

   - key getter: Even if a dead key have not yet been replaced by
     caml_ephe_none, getting it should return none.
   - key setter: If we replace a dead key we need to set the data to
     caml_ephe_none and clean the ephemeron.

     This two cases are dealt by a call to do_check_key_clean that
     trigger the cleaning of the ephemerons when the accessed key is
     dead. This test is fast.

     In the case of value getter and value setter, there is no fast
     test because the removing of the data depend of the deadliness of the keys.
     We must always try to clean the ephemerons.

 */

#define None_val (Val_int(0))
#define Some_tag 0

/* If we are in Phase_clean we need to check if the key
   that is going to disappear is dead and so should trigger a cleaning
 */
static void do_check_key_clean(value ar, mlsize_t offset){
  CAMLassert ( offset >= 2);
  if (caml_gc_phase == Phase_clean){
    value elt = Field (ar, offset);
    if (elt != caml_ephe_none && Is_Dead_during_clean(elt)){
      Field(ar,offset) = caml_ephe_none;
      Field(ar,CAML_EPHE_DATA_OFFSET) = caml_ephe_none;
    };
  };
}

/* If we are in Phase_clean we need to do as if the key is empty when
   it will be cleaned during this phase */
static inline int is_ephe_key_none(value ar, mlsize_t offset){
  value elt = Field (ar, offset);
  if (elt == caml_ephe_none){
    return 1;
  }else if (caml_gc_phase == Phase_clean && Is_Dead_during_clean(elt)){
    Field(ar,offset) = caml_ephe_none;
    Field(ar,CAML_EPHE_DATA_OFFSET) = caml_ephe_none;
    return 1;
  } else {
    return 0;
  }
}


static void do_set (value ar, mlsize_t offset, value v)
{
  if (Is_block (v) && Is_young (v)){
    /* modified version of Modify */
    value old = Field (ar, offset);
    Field (ar, offset) = v;
    if (!(Is_block (old) && Is_young (old))){
      add_to_ephe_ref_table (&caml_ephe_ref_table, ar, offset);
    }
  }else{
    Field (ar, offset) = v;
  }
}

CAMLprim value caml_ephe_set_key (value ar, value n, value el)
{
  mlsize_t offset = Long_val (n) + 2;
  CAMLassert (Is_in_heap (ar));
  if (offset < 2 || offset >= Wosize_val (ar)){
    caml_invalid_argument ("Weak.set");
  }
  do_check_key_clean(ar,offset);
  do_set (ar, offset, el);
  return Val_unit;
}

CAMLprim value caml_ephe_unset_key (value ar, value n)
{
  mlsize_t offset = Long_val (n) + 2;
  CAMLassert (Is_in_heap (ar));
  if (offset < 2 || offset >= Wosize_val (ar)){
    caml_invalid_argument ("Weak.set");
  }
  do_check_key_clean(ar,offset);
  Field (ar, offset) = caml_ephe_none;
  return Val_unit;
}

value caml_ephe_set_key_option (value ar, value n, value el)
{
  mlsize_t offset = Long_val (n) + 2;
  CAMLassert (Is_in_heap (ar));
  if (offset < 2 || offset >= Wosize_val (ar)){
    caml_invalid_argument ("Weak.set");
  }
  do_check_key_clean(ar,offset);
  if (el != None_val && Is_block (el)){
    CAMLassert (Wosize_val (el) == 1);
    do_set (ar, offset, Field (el, 0));
  }else{
    Field (ar, offset) = caml_ephe_none;
  }
  return Val_unit;
}

CAMLprim value caml_weak_set (value ar, value n, value el){
  return caml_ephe_set_key_option(ar,n,el);
}

CAMLprim value caml_ephe_set_data (value ar, value el)
{
  CAMLassert (Is_in_heap (ar));
  if (caml_gc_phase == Phase_clean){
    /* During this phase since we don't know which ephemeron have been
       cleaned we always need to check it. */
    caml_ephe_clean(ar);
  };
  do_set (ar, 1, el);
  return Val_unit;
}

CAMLprim value caml_ephe_unset_data (value ar)
{
  CAMLassert (Is_in_heap (ar));
  Field (ar, CAML_EPHE_DATA_OFFSET) = caml_ephe_none;
  return Val_unit;
}

CAMLprim value caml_ephe_get_key (value ar, value n)
{
  CAMLparam2 (ar, n);
  mlsize_t offset = Long_val (n) + 2;
  CAMLlocal2 (res, elt);
  CAMLassert (Is_in_heap (ar));
  if (offset < 2 || offset >= Wosize_val (ar)){
    caml_invalid_argument ("Weak.get_key");
  }
  if (is_ephe_key_none(ar, offset)){
    res = None_val;
  }else{
    elt = Field (ar, offset);
    if (caml_gc_phase == Phase_mark && Must_be_Marked_during_mark(elt)){
      caml_darken (elt, NULL);
    }
    res = caml_alloc_small (1, Some_tag);
    Field (res, 0) = elt;
  }
  CAMLreturn (res);
}

CAMLprim value caml_weak_get (value ar, value n){
  return caml_ephe_get_key(ar, n);
}

CAMLprim value caml_ephe_get_data (value ar)
{
  CAMLparam1 (ar);
  mlsize_t offset = 1;
  CAMLlocal2 (res, elt);
  CAMLassert (Is_in_heap (ar));
  elt = Field (ar, offset);
  if(caml_gc_phase == Phase_clean) caml_ephe_clean(ar);
  if (elt == caml_ephe_none){
    res = None_val;
  }else{
    if (caml_gc_phase == Phase_mark && Must_be_Marked_during_mark(elt)){
      caml_darken (elt, NULL);
    }
    res = caml_alloc_small (1, Some_tag);
    Field (res, 0) = elt;
  }
  CAMLreturn (res);
}

CAMLprim value caml_ephe_get_key_copy (value ar, value n)
{
  CAMLparam2 (ar, n);
  mlsize_t offset = Long_val (n) + 2;
  CAMLlocal2 (res, elt);
  value v;  /* Caution: this is NOT a local root. */
  CAMLassert (Is_in_heap (ar));
  if (offset < 1 || offset >= Wosize_val (ar)){
    caml_invalid_argument ("Weak.get_copy");
  }

  if (is_ephe_key_none(ar, offset)) CAMLreturn (None_val);
  v = Field (ar, offset);
  /** Don't copy custom_block #7279 */
  if (Is_block (v) && Is_in_heap_or_young(v) && Tag_val(v) != Custom_tag ) {
    elt = caml_alloc (Wosize_val (v), Tag_val (v));
          /* The GC may erase or move v during this call to caml_alloc. */
    v = Field (ar, offset);
    if (is_ephe_key_none(ar, offset)) CAMLreturn (None_val);
    if (Tag_val (v) < No_scan_tag){
      mlsize_t i;
      for (i = 0; i < Wosize_val (v); i++){
        value f = Field (v, i);
        if (caml_gc_phase == Phase_mark && Must_be_Marked_during_mark(f)){
          caml_darken (f, NULL);
        }
        Modify (&Field (elt, i), f);
      }
    }else{
      memmove (Bp_val (elt), Bp_val (v), Bosize_val (v));
    }
  }else{
    if ( caml_gc_phase == Phase_mark && Must_be_Marked_during_mark(v) ){
      caml_darken (v, NULL);
    };
    elt = v;
  }
  res = caml_alloc_small (1, Some_tag);
  Field (res, 0) = elt;

  CAMLreturn (res);
}

CAMLprim value caml_weak_get_copy (value ar, value n){
  return caml_ephe_get_key_copy(ar,n);
}

CAMLprim value caml_ephe_get_data_copy (value ar)
{
  CAMLparam1 (ar);
  mlsize_t offset = 1;
  CAMLlocal2 (res, elt);
  value v;  /* Caution: this is NOT a local root. */
  CAMLassert (Is_in_heap (ar));

  v = Field (ar, offset);
  if (caml_gc_phase == Phase_clean) caml_ephe_clean(ar);
  if (v == caml_ephe_none) CAMLreturn (None_val);
  /** Don't copy custom_block #7279 */
  if (Is_block (v) && Is_in_heap_or_young(v) && Tag_val(v) != Custom_tag ) {
    elt = caml_alloc (Wosize_val (v), Tag_val (v));
          /* The GC may erase or move v during this call to caml_alloc. */
    v = Field (ar, offset);
    if (caml_gc_phase == Phase_clean) caml_ephe_clean(ar);
    if (v == caml_ephe_none) CAMLreturn (None_val);
    if (Tag_val (v) < No_scan_tag){
      mlsize_t i;
      for (i = 0; i < Wosize_val (v); i++){
        value f = Field (v, i);
        if (caml_gc_phase == Phase_mark && Must_be_Marked_during_mark(f)){
          caml_darken (f, NULL);
        }
        Modify (&Field (elt, i), f);
      }
    }else{
      memmove (Bp_val (elt), Bp_val (v), Bosize_val (v));
    }
  }else{
    if ( caml_gc_phase == Phase_mark && Must_be_Marked_during_mark(v) ){
      caml_darken (v, NULL);
    };
    elt = v;
  }
  res = caml_alloc_small (1, Some_tag);
  Field (res, 0) = elt;

  CAMLreturn (res);
}

CAMLprim value caml_ephe_check_key (value ar, value n)
{
  mlsize_t offset = Long_val (n) + 2;
  CAMLassert (Is_in_heap (ar));
  if (offset < 2 || offset >= Wosize_val (ar)){
    caml_invalid_argument ("Weak.check");
  }
  return Val_bool (!is_ephe_key_none(ar, offset));
}

CAMLprim value caml_weak_check (value ar, value n)
{
  return caml_ephe_check_key(ar,n);
}

CAMLprim value caml_ephe_check_data (value ar)
{
  if(caml_gc_phase == Phase_clean) caml_ephe_clean(ar);
  return Val_bool (Field (ar, CAML_EPHE_DATA_OFFSET) != caml_ephe_none);
}

CAMLprim value caml_ephe_blit_key (value ars, value ofs,
                               value ard, value ofd, value len)
{
  mlsize_t offset_s = Long_val (ofs) + 2;
  mlsize_t offset_d = Long_val (ofd) + 2;
  mlsize_t length = Long_val (len);
  long i;
  CAMLassert (Is_in_heap (ars));
  CAMLassert (Is_in_heap (ard));
  if (offset_s < 1 || offset_s + length > Wosize_val (ars)){
    caml_invalid_argument ("Weak.blit");
  }
  if (offset_d < 1 || offset_d + length > Wosize_val (ard)){
    caml_invalid_argument ("Weak.blit");
  }
  if (caml_gc_phase == Phase_clean){
    caml_ephe_clean(ars);
    caml_ephe_clean(ard);
  }
  if (offset_d < offset_s){
    for (i = 0; i < length; i++){
      do_set (ard, offset_d + i, Field (ars, offset_s + i));
    }
  }else{
    for (i = length - 1; i >= 0; i--){
      do_set (ard, offset_d + i,  Field (ars, offset_s + i));
    }
  }
  return Val_unit;
}

CAMLprim value caml_ephe_blit_data (value ars, value ard)
{
  if(caml_gc_phase == Phase_clean) {
    caml_ephe_clean(ars);
    caml_ephe_clean(ard);
  };
  do_set (ard, CAML_EPHE_DATA_OFFSET, Field (ars, CAML_EPHE_DATA_OFFSET));
  return Val_unit;
}

CAMLprim value caml_weak_blit (value ars, value ofs,
                      value ard, value ofd, value len)
{
  return caml_ephe_blit_key (ars, ofs, ard, ofd, len);
}
