/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*           Damien Doligez, projet Moscova, INRIA Rocquencourt           */
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

/* Handling of finalised values. */

#include "caml/callback.h"
#include "caml/compact.h"
#include "caml/fail.h"
#include "caml/finalise.h"
#include "caml/minor_gc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"
#if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
#include "caml/spacetime.h"
#endif

struct final {
  value fun;
  value val;
  int offset;
};

struct finalisable {
  struct final *table;
  uintnat old;
  uintnat young;
  uintnat size;
};
/* [0..old) : finalisable set, the values are in the major heap
   [old..young) : recent set, the values could be in the minor heap
   [young..size) : free space

   The element of the finalisable set are moved to the finalising set
   below when the value are unreachable (for the first or last time).

*/

static struct finalisable finalisable_first = {NULL,0,0,0};
static struct finalisable finalisable_last = {NULL,0,0,0};

struct to_do {
  struct to_do *next;
  int size;
  struct final item[1];  /* variable size */
};

static struct to_do *to_do_hd = NULL;
static struct to_do *to_do_tl = NULL;
/*
  to_do_hd: head of the list of finalisation functions that can be run.
  to_do_tl: tail of the list of finalisation functions that can be run.

  It is the finalising set.
*/


/* [size] is a number of elements for the [to_do.item] array */
static void alloc_to_do (int size)
{
  struct to_do *result = caml_stat_alloc_noexc (sizeof (struct to_do) +
                                                size * sizeof (struct final));
  if (result == NULL) caml_fatal_error ("out of memory");
  result->next = NULL;
  result->size = size;
  if (to_do_tl == NULL){
    to_do_hd = result;
    to_do_tl = result;
  }else{
    CAMLassert (to_do_tl->next == NULL);
    to_do_tl->next = result;
    to_do_tl = result;
  }
}

/* Find white finalisable values, move them to the finalising set, and
   darken them (if darken_value is true).
*/
static void generic_final_update (struct finalisable * final, int darken_value)
{
  uintnat i, j, k;
  uintnat todo_count = 0;

  CAMLassert (final->old <= final->young);
  for (i = 0; i < final->old; i++){
    CAMLassert (Is_block (final->table[i].val));
    CAMLassert (Is_in_heap (final->table[i].val));
    if (Is_white_val (final->table[i].val)){
      ++ todo_count;
    }
  }

  /** invariant:
      - 0 <= j <= i /\ 0 <= k <= i /\ 0 <= k <= todo_count
      - i : index in final_table, before i all the values are black
      (alive or in the minor heap) or the finalizer have been copied
      in to_do_tl.
      - j : index in final_table, before j all the values are black
      (alive or in the minor heap), next available slot.
      - k : index in to_do_tl, next available slot.
  */
  if (todo_count > 0){
    alloc_to_do (todo_count);
    j = k = 0;
    for (i = 0; i < final->old; i++){
      CAMLassert (Is_block (final->table[i].val));
      CAMLassert (Is_in_heap (final->table[i].val));
      CAMLassert (Tag_val (final->table[i].val) != Forward_tag);
      if(Is_white_val (final->table[i].val)){
        /** dead */
        to_do_tl->item[k] = final->table[i];
        if(!darken_value){
          /* The value is not darken so the finalisation function
             is called with unit not with the value */
          to_do_tl->item[k].val = Val_unit;
          to_do_tl->item[k].offset = 0;
        };
        k++;
      }else{
        /** alive */
        final->table[j++] = final->table[i];
      }
    }
    CAMLassert (i == final->old);
    CAMLassert (k == todo_count);
    final->old = j;
    for(;i < final->young; i++){
      final->table[j++] = final->table[i];
    }
    final->young = j;
    to_do_tl->size = k;
    if(darken_value){
      for (i = 0; i < k; i++){
        /* Note that item may already be dark due to multiple entries in
           the final table. */
        caml_darken (to_do_tl->item[i].val, NULL);
      }
    }
  }
}

void caml_final_update_mark_phase (){
  generic_final_update(&finalisable_first, /* darken_value */ 1);
}

void caml_final_update_clean_phase (){
  generic_final_update(&finalisable_last, /* darken_value */ 0);
}


static int running_finalisation_function = 0;

/* Call the finalisation functions for the finalising set.
   Note that this function must be reentrant.
*/
void caml_final_do_calls (void)
{
  struct final f;
  value res;
#if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
  void* saved_spacetime_trie_node_ptr;
#endif

  if (running_finalisation_function) return;
  if (to_do_hd != NULL){
    if (caml_finalise_begin_hook != NULL) (*caml_finalise_begin_hook) ();
    caml_gc_message (0x80, "Calling finalisation functions.\n");
    while (1){
      while (to_do_hd != NULL && to_do_hd->size == 0){
        struct to_do *next_hd = to_do_hd->next;
        caml_stat_free (to_do_hd);
        to_do_hd = next_hd;
        if (to_do_hd == NULL) to_do_tl = NULL;
      }
      if (to_do_hd == NULL) break;
      CAMLassert (to_do_hd->size > 0);
      -- to_do_hd->size;
      f = to_do_hd->item[to_do_hd->size];
      running_finalisation_function = 1;
#if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
      /* We record the finaliser's execution separately.
         (The code of [caml_callback_exn] will do the hard work of finding
         the correct place in the trie.) */
      saved_spacetime_trie_node_ptr = caml_spacetime_trie_node_ptr;
      caml_spacetime_trie_node_ptr = caml_spacetime_finaliser_trie_root;
#endif
      res = caml_callback_exn (f.fun, f.val + f.offset);
#if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
      caml_spacetime_trie_node_ptr = saved_spacetime_trie_node_ptr;
#endif
      running_finalisation_function = 0;
      if (Is_exception_result (res)) caml_raise (Extract_exception (res));
    }
    caml_gc_message (0x80, "Done calling finalisation functions.\n");
    if (caml_finalise_end_hook != NULL) (*caml_finalise_end_hook) ();
  }
}

/* Call a scanning_action [f] on [x]. */
#define Call_action(f,x) (*(f)) ((x), &(x))

/* Call [*f] on the closures of the finalisable set and
   the closures and values of the finalising set.
   This is called by the major GC [caml_darken_all_roots]
   and by the compactor through [caml_do_roots]
*/
void caml_final_do_roots (scanning_action f)
{
  uintnat i;
  struct to_do *todo;

  CAMLassert (finalisable_first.old <= finalisable_first.young);
  for (i = 0; i < finalisable_first.young; i++){
    Call_action (f, finalisable_first.table[i].fun);
  };

  CAMLassert (finalisable_last.old <= finalisable_last.young);
  for (i = 0; i < finalisable_last.young; i++){
    Call_action (f, finalisable_last.table[i].fun);
  };

  for (todo = to_do_hd; todo != NULL; todo = todo->next){
    for (i = 0; i < todo->size; i++){
      Call_action (f, todo->item[i].fun);
      Call_action (f, todo->item[i].val);
    }
  }
}

/* Call caml_invert_root on the values of the finalisable set. This is called
   directly by the compactor.
*/
void caml_final_invert_finalisable_values ()
{
  uintnat i;

  CAMLassert (finalisable_first.old <= finalisable_first.young);
  for (i = 0; i < finalisable_first.young; i++){
    caml_invert_root(finalisable_first.table[i].val,
                &finalisable_first.table[i].val);
  };

  CAMLassert (finalisable_last.old <= finalisable_last.young);
  for (i = 0; i < finalisable_last.young; i++){
    caml_invert_root(finalisable_last.table[i].val,
                &finalisable_last.table[i].val);
  };
}

/* Call [caml_oldify_one] on the closures and values of the recent set.
   This is called by the minor GC through [caml_oldify_local_roots].
*/
void caml_final_oldify_young_roots ()
{
  uintnat i;

  CAMLassert (finalisable_first.old <= finalisable_first.young);
  for (i = finalisable_first.old; i < finalisable_first.young; i++){
    caml_oldify_one(finalisable_first.table[i].fun,
                    &finalisable_first.table[i].fun);
    caml_oldify_one(finalisable_first.table[i].val,
                    &finalisable_first.table[i].val);
  }

  CAMLassert (finalisable_last.old <= finalisable_last.young);
  for (i = finalisable_last.old; i < finalisable_last.young; i++){
    caml_oldify_one(finalisable_last.table[i].fun,
                    &finalisable_last.table[i].fun);
  }

}

static void generic_final_minor_update (struct finalisable * final)
{
  uintnat i, j, k;
  uintnat todo_count = 0;

  CAMLassert (final->old <= final->young);
  for (i = final->old; i < final->young; i++){
    CAMLassert (Is_block (final->table[i].val));
    CAMLassert (Is_in_heap_or_young (final->table[i].val));
    if (Is_young(final->table[i].val) && Hd_val(final->table[i].val) != 0){
      ++ todo_count;
    }
  }

  /** invariant:
      - final->old <= j <= i /\ final->old <= k <= i /\ 0 <= k <= todo_count
      - i : index in final_table, before i all the values are alive
            or the finalizer have been copied in to_do_tl.
      - j : index in final_table, before j all the values are alive,
            next available slot.
      - k : index in to_do_tl, next available slot.
  */
  if (todo_count > 0){
    alloc_to_do (todo_count);
    k = 0;
    j = final->old;
    for (i = final->old; i < final->young; i++){
      CAMLassert (Is_block (final->table[i].val));
      CAMLassert (Is_in_heap_or_young (final->table[i].val));
      CAMLassert (Tag_val (final->table[i].val) != Forward_tag);
      if(Is_young(final->table[j].val) && Hd_val(final->table[i].val) != 0){
        /** dead */
        to_do_tl->item[k] = final->table[i];
        /* The finalisation function is called with unit not with the value */
        to_do_tl->item[k].val = Val_unit;
        to_do_tl->item[k].offset = 0;
        k++;
      }else{
        /** alive */
        final->table[j++] = final->table[i];
      }
    }
    CAMLassert (i == final->young);
    CAMLassert (k == todo_count);
    final->young = j;
    to_do_tl->size = todo_count;
  }

  /** update the minor value to the copied major value */
  for (i = final->old; i < final->young; i++){
    CAMLassert (Is_block (final->table[i].val));
    CAMLassert (Is_in_heap_or_young (final->table[i].val));
    if (Is_young(final->table[i].val)) {
      CAMLassert (Hd_val(final->table[i].val) == 0);
      final->table[i].val = Field(final->table[i].val,0);
    }
  }

  /** check invariant */
  CAMLassert (final->old <= final->young);
  for (i = 0; i < final->young; i++){
    CAMLassert( Is_in_heap(final->table[i].val) );
  };

}

/* At the end of minor collection update the finalise_last roots in
   minor heap when moved to major heap or moved them to the finalising
   set when dead.
*/
void caml_final_update_minor_roots ()
{
  generic_final_minor_update(&finalisable_last);
}

/* Empty the recent set into the finalisable set.
   This is called at the end of each minor collection.
   The minor heap must be empty when this is called.
*/
void caml_final_empty_young (void)
{
  finalisable_first.old = finalisable_first.young;
  finalisable_last.old = finalisable_last.young;
}

/* Put (f,v) in the recent set. */
static void generic_final_register (struct finalisable *final, value f, value v)
{
  if (!Is_block (v)
      || !Is_in_heap_or_young(v)
      || Tag_val (v) == Lazy_tag
#ifdef FLAT_FLOAT_ARRAY
      || Tag_val (v) == Double_tag
#endif
      || Tag_val (v) == Forward_tag) {
    caml_invalid_argument ("Gc.finalise");
  }
  CAMLassert (final->old <= final->young);

  if (final->young >= final->size){
    if (final->table == NULL){
      uintnat new_size = 30;
      final->table = caml_stat_alloc (new_size * sizeof (struct final));
      CAMLassert (final->old == 0);
      CAMLassert (final->young == 0);
      final->size = new_size;
    }else{
      uintnat new_size = final->size * 2;
      final->table = caml_stat_resize (final->table,
                                      new_size * sizeof (struct final));
      final->size = new_size;
    }
  }
  CAMLassert (final->young < final->size);
  final->table[final->young].fun = f;
  if (Tag_val (v) == Infix_tag){
    final->table[final->young].offset = Infix_offset_val (v);
    final->table[final->young].val = v - Infix_offset_val (v);
  }else{
    final->table[final->young].offset = 0;
    final->table[final->young].val = v;
  }
  ++ final->young;

}

CAMLprim value caml_final_register (value f, value v){
  generic_final_register(&finalisable_first, f, v);
  return Val_unit;
}

CAMLprim value caml_final_register_called_without_value (value f, value v){
  generic_final_register(&finalisable_last, f, v);
  return Val_unit;
}


CAMLprim value caml_final_release (value unit)
{
  running_finalisation_function = 0;
  return Val_unit;
}

static void gen_final_invariant_check(struct finalisable *final){
  uintnat i;

  CAMLassert (final->old <= final->young);
  for (i = 0; i < final->old; i++){
    CAMLassert( Is_in_heap(final->table[i].val) );
  };
  for (i = final->old; i < final->young; i++){
    CAMLassert( Is_in_heap_or_young(final->table[i].val) );
  };
}

void caml_final_invariant_check(void){
  gen_final_invariant_check(&finalisable_first);
  gen_final_invariant_check(&finalisable_last);
}
