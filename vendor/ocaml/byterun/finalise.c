/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*          Damien Doligez, projet Moscova, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Handling of finalised values. */

#include "caml/callback.h"
#include "caml/fail.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"

struct final {
  value fun;
  value val;
  int offset;
};

static struct final *final_table = NULL;
static uintnat old = 0, young = 0, size = 0;
/* [0..old) : finalisable set
   [old..young) : recent set
   [young..size) : free space
*/

struct to_do {
  struct to_do *next;
  int size;
  struct final item[1];  /* variable size */
};

static struct to_do *to_do_hd = NULL;
static struct to_do *to_do_tl = NULL;

static void alloc_to_do (int size)
{
  struct to_do *result = malloc (sizeof (struct to_do)
                                 + size * sizeof (struct final));
  if (result == NULL) caml_fatal_error ("out of memory");
  result->next = NULL;
  result->size = size;
  if (to_do_tl == NULL){
    to_do_hd = result;
    to_do_tl = result;
  }else{
    Assert (to_do_tl->next == NULL);
    to_do_tl->next = result;
    to_do_tl = result;
  }
}

/* Find white finalisable values, put them in the finalising set, and
   darken them.
   The recent set is empty.
*/
void caml_final_update (void)
{
  uintnat i, j, k;
  uintnat todo_count = 0;

  Assert (young == old);
  for (i = 0; i < old; i++){
    Assert (Is_block (final_table[i].val));
    Assert (Is_in_heap (final_table[i].val));
    if (Is_white_val (final_table[i].val)) ++ todo_count;
  }

  if (todo_count > 0){
    alloc_to_do (todo_count);
    j = k = 0;
    for (i = 0; i < old; i++){
    again:
      Assert (Is_block (final_table[i].val));
      Assert (Is_in_heap (final_table[i].val));
      if (Is_white_val (final_table[i].val)){
        if (Tag_val (final_table[i].val) == Forward_tag){
          value fv;
          Assert (final_table[i].offset == 0);
          fv = Forward_val (final_table[i].val);
          if (Is_block (fv)
              && (!Is_in_value_area(fv) || Tag_val (fv) == Forward_tag
                  || Tag_val (fv) == Lazy_tag || Tag_val (fv) == Double_tag)){
            /* Do not short-circuit the pointer. */
          }else{
            final_table[i].val = fv;
            if (Is_block (final_table[i].val)
                && Is_in_heap (final_table[i].val)){
              goto again;
            }
          }
        }
        to_do_tl->item[k++] = final_table[i];
      }else{
        final_table[j++] = final_table[i];
      }
    }
    young = old = j;
    to_do_tl->size = k;
    for (i = 0; i < k; i++){
      CAMLassert (Is_white_val (to_do_tl->item[i].val));
      caml_darken (to_do_tl->item[i].val, NULL);
    }
  }
}

static int running_finalisation_function = 0;

/* Call the finalisation functions for the finalising set.
   Note that this function must be reentrant.
*/
void caml_final_do_calls (void)
{
  struct final f;
  value res;

  if (running_finalisation_function) return;

  if (to_do_hd != NULL){
    caml_gc_message (0x80, "Calling finalisation functions.\n", 0);
    while (1){
      while (to_do_hd != NULL && to_do_hd->size == 0){
        struct to_do *next_hd = to_do_hd->next;
        free (to_do_hd);
        to_do_hd = next_hd;
        if (to_do_hd == NULL) to_do_tl = NULL;
      }
      if (to_do_hd == NULL) break;
      Assert (to_do_hd->size > 0);
      -- to_do_hd->size;
      f = to_do_hd->item[to_do_hd->size];
      running_finalisation_function = 1;
      res = caml_callback_exn (f.fun, f.val + f.offset);
      running_finalisation_function = 0;
      if (Is_exception_result (res)) caml_raise (Extract_exception (res));
    }
    caml_gc_message (0x80, "Done calling finalisation functions.\n", 0);
  }
}

/* Call a scanning_action [f] on [x]. */
#define Call_action(f,x) (*(f)) ((x), &(x))

/* Call [*f] on the closures of the finalisable set and
   the closures and values of the finalising set.
   The recent set is empty.
   This is called by the major GC and the compactor
   through [caml_darken_all_roots].
*/
void caml_final_do_strong_roots (scanning_action f)
{
  uintnat i;
  struct to_do *todo;

  Assert (old == young);
  for (i = 0; i < old; i++) Call_action (f, final_table[i].fun);

  for (todo = to_do_hd; todo != NULL; todo = todo->next){
    for (i = 0; i < todo->size; i++){
      Call_action (f, todo->item[i].fun);
      Call_action (f, todo->item[i].val);
    }
  }
}

/* Call [*f] on the values of the finalisable set.
   The recent set is empty.
   This is called directly by the compactor.
*/
void caml_final_do_weak_roots (scanning_action f)
{
  uintnat i;

  Assert (old == young);
  for (i = 0; i < old; i++) Call_action (f, final_table[i].val);
}

/* Call [*f] on the closures and values of the recent set.
   This is called by the minor GC through [caml_oldify_local_roots].
*/
void caml_final_do_young_roots (scanning_action f)
{
  uintnat i;

  Assert (old <= young);
  for (i = old; i < young; i++){
    Call_action (f, final_table[i].fun);
    Call_action (f, final_table[i].val);
  }
}

/* Empty the recent set into the finalisable set.
   This is called at the end of each minor collection.
   The minor heap must be empty when this is called.
*/
void caml_final_empty_young (void)
{
  old = young;
}

/* Put (f,v) in the recent set. */
CAMLprim value caml_final_register (value f, value v)
{
  if (!(Is_block (v) && Is_in_heap_or_young(v))) {
    caml_invalid_argument ("Gc.finalise");
  }
  Assert (old <= young);

  if (young >= size){
    if (final_table == NULL){
      uintnat new_size = 30;
      final_table = caml_stat_alloc (new_size * sizeof (struct final));
      Assert (old == 0);
      Assert (young == 0);
      size = new_size;
    }else{
      uintnat new_size = size * 2;
      final_table = caml_stat_resize (final_table,
                                      new_size * sizeof (struct final));
      size = new_size;
    }
  }
  Assert (young < size);
  final_table[young].fun = f;
  if (Tag_val (v) == Infix_tag){
    final_table[young].offset = Infix_offset_val (v);
    final_table[young].val = v - Infix_offset_val (v);
  }else{
    final_table[young].offset = 0;
    final_table[young].val = v;
  }
  ++ young;

  return Val_unit;
}

CAMLprim value caml_final_release (value unit)
{
  running_finalisation_function = 0;
  return Val_unit;
}
