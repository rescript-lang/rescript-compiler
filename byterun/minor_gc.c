/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#include <string.h>
#include "caml/config.h"
#include "caml/fail.h"
#include "caml/finalise.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/weak.h"

asize_t caml_minor_heap_size;
static void *caml_young_base = NULL;
CAMLexport char *caml_young_start = NULL, *caml_young_end = NULL;
CAMLexport char *caml_young_ptr = NULL, *caml_young_limit = NULL;

CAMLexport struct caml_ref_table
  caml_ref_table = { NULL, NULL, NULL, NULL, NULL, 0, 0},
  caml_weak_ref_table = { NULL, NULL, NULL, NULL, NULL, 0, 0};

int caml_in_minor_collection = 0;

#ifdef DEBUG
static unsigned long minor_gc_counter = 0;
#endif

void caml_alloc_table (struct caml_ref_table *tbl, asize_t sz, asize_t rsv)
{
  value **new_table;

  tbl->size = sz;
  tbl->reserve = rsv;
  new_table = (value **) caml_stat_alloc ((tbl->size + tbl->reserve)
                                          * sizeof (value *));
  if (tbl->base != NULL) caml_stat_free (tbl->base);
  tbl->base = new_table;
  tbl->ptr = tbl->base;
  tbl->threshold = tbl->base + tbl->size;
  tbl->limit = tbl->threshold;
  tbl->end = tbl->base + tbl->size + tbl->reserve;
}

static void reset_table (struct caml_ref_table *tbl)
{
  tbl->size = 0;
  tbl->reserve = 0;
  if (tbl->base != NULL) caml_stat_free (tbl->base);
  tbl->base = tbl->ptr = tbl->threshold = tbl->limit = tbl->end = NULL;
}

static void clear_table (struct caml_ref_table *tbl)
{
    tbl->ptr = tbl->base;
    tbl->limit = tbl->threshold;
}

/* size in bytes */
void caml_set_minor_heap_size (asize_t size)
{
  char *new_heap;
  void *new_heap_base;

  Assert (size >= Bsize_wsize(Minor_heap_min));
  Assert (size <= Bsize_wsize(Minor_heap_max));
  Assert (size % sizeof (value) == 0);
  if (caml_young_ptr != caml_young_end) caml_minor_collection ();
                                    Assert (caml_young_ptr == caml_young_end);
  new_heap = caml_aligned_malloc(size, 0, &new_heap_base);
  if (new_heap == NULL) caml_raise_out_of_memory();
  if (caml_page_table_add(In_young, new_heap, new_heap + size) != 0)
    caml_raise_out_of_memory();

  if (caml_young_start != NULL){
    caml_page_table_remove(In_young, caml_young_start, caml_young_end);
    free (caml_young_base);
  }
  caml_young_base = new_heap_base;
  caml_young_start = new_heap;
  caml_young_end = new_heap + size;
  caml_young_limit = caml_young_start;
  caml_young_ptr = caml_young_end;
  caml_minor_heap_size = size;

  reset_table (&caml_ref_table);
  reset_table (&caml_weak_ref_table);
}

static value oldify_todo_list = 0;

/* Note that the tests on the tag depend on the fact that Infix_tag,
   Forward_tag, and No_scan_tag are contiguous. */

void caml_oldify_one (value v, value *p)
{
  value result;
  header_t hd;
  mlsize_t sz, i;
  tag_t tag;

 tail_call:
  if (Is_block (v) && Is_young (v)){
    Assert (Hp_val (v) >= caml_young_ptr);
    hd = Hd_val (v);
    if (hd == 0){         /* If already forwarded */
      *p = Field (v, 0);  /*  then forward pointer is first field. */
    }else{
      tag = Tag_hd (hd);
      if (tag < Infix_tag){
        value field0;

        sz = Wosize_hd (hd);
        result = caml_alloc_shr (sz, tag);
        *p = result;
        field0 = Field (v, 0);
        Hd_val (v) = 0;            /* Set forward flag */
        Field (v, 0) = result;     /*  and forward pointer. */
        if (sz > 1){
          Field (result, 0) = field0;
          Field (result, 1) = oldify_todo_list;    /* Add this block */
          oldify_todo_list = v;                    /*  to the "to do" list. */
        }else{
          Assert (sz == 1);
          p = &Field (result, 0);
          v = field0;
          goto tail_call;
        }
      }else if (tag >= No_scan_tag){
        sz = Wosize_hd (hd);
        result = caml_alloc_shr (sz, tag);
        for (i = 0; i < sz; i++) Field (result, i) = Field (v, i);
        Hd_val (v) = 0;            /* Set forward flag */
        Field (v, 0) = result;     /*  and forward pointer. */
        *p = result;
      }else if (tag == Infix_tag){
        mlsize_t offset = Infix_offset_hd (hd);
        caml_oldify_one (v - offset, p);   /* Cannot recurse deeper than 1. */
        *p += offset;
      }else{
        value f = Forward_val (v);
        tag_t ft = 0;
        int vv = 1;

        Assert (tag == Forward_tag);
        if (Is_block (f)){
          if (Is_young (f)){
            vv = 1;
            ft = Tag_val (Hd_val (f) == 0 ? Field (f, 0) : f);
          }else{
            vv = Is_in_value_area(f);
            if (vv){
              ft = Tag_val (f);
            }
          }
        }
        if (!vv || ft == Forward_tag || ft == Lazy_tag || ft == Double_tag){
          /* Do not short-circuit the pointer.  Copy as a normal block. */
          Assert (Wosize_hd (hd) == 1);
          result = caml_alloc_shr (1, Forward_tag);
          *p = result;
          Hd_val (v) = 0;             /* Set (GC) forward flag */
          Field (v, 0) = result;      /*  and forward pointer. */
          p = &Field (result, 0);
          v = f;
          goto tail_call;
        }else{
          v = f;                        /* Follow the forwarding */
          goto tail_call;               /*  then oldify. */
        }
      }
    }
  }else{
    *p = v;
  }
}

/* Finish the work that was put off by [caml_oldify_one].
   Note that [caml_oldify_one] itself is called by oldify_mopup, so we
   have to be careful to remove the first entry from the list before
   oldifying its fields. */
void caml_oldify_mopup (void)
{
  value v, new_v, f;
  mlsize_t i;

  while (oldify_todo_list != 0){
    v = oldify_todo_list;                /* Get the head. */
    Assert (Hd_val (v) == 0);            /* It must be forwarded. */
    new_v = Field (v, 0);                /* Follow forward pointer. */
    oldify_todo_list = Field (new_v, 1); /* Remove from list. */

    f = Field (new_v, 0);
    if (Is_block (f) && Is_young (f)){
      caml_oldify_one (f, &Field (new_v, 0));
    }
    for (i = 1; i < Wosize_val (new_v); i++){
      f = Field (v, i);
      if (Is_block (f) && Is_young (f)){
        caml_oldify_one (f, &Field (new_v, i));
      }else{
        Field (new_v, i) = f;
      }
    }
  }
}

/* Make sure the minor heap is empty by performing a minor collection
   if needed.
*/
void caml_empty_minor_heap (void)
{
  value **r;
  uintnat prev_alloc_words;

  if (caml_young_ptr != caml_young_end){
    if (caml_minor_gc_begin_hook != NULL) (*caml_minor_gc_begin_hook) ();
    prev_alloc_words = caml_allocated_words;
    caml_in_minor_collection = 1;
    caml_gc_message (0x02, "<", 0);
    caml_oldify_local_roots();
    for (r = caml_ref_table.base; r < caml_ref_table.ptr; r++){
      caml_oldify_one (**r, *r);
    }
    caml_oldify_mopup ();
    for (r = caml_weak_ref_table.base; r < caml_weak_ref_table.ptr; r++){
      if (Is_block (**r) && Is_young (**r)){
        if (Hd_val (**r) == 0){
          **r = Field (**r, 0);
        }else{
          **r = caml_weak_none;
        }
      }
    }
    if (caml_young_ptr < caml_young_start) caml_young_ptr = caml_young_start;
    caml_stat_minor_words += Wsize_bsize (caml_young_end - caml_young_ptr);
    caml_young_ptr = caml_young_end;
    caml_young_limit = caml_young_start;
    clear_table (&caml_ref_table);
    clear_table (&caml_weak_ref_table);
    caml_gc_message (0x02, ">", 0);
    caml_in_minor_collection = 0;
    caml_stat_promoted_words += caml_allocated_words - prev_alloc_words;
    ++ caml_stat_minor_collections;
    caml_final_empty_young ();
    if (caml_minor_gc_end_hook != NULL) (*caml_minor_gc_end_hook) ();
  }else{
    caml_final_empty_young ();
  }
#ifdef DEBUG
  {
    value *p;
    for (p = (value *) caml_young_start; p < (value *) caml_young_end; ++p){
      *p = Debug_free_minor;
    }
    ++ minor_gc_counter;
  }
#endif
}

/* Do a minor collection and a slice of major collection, call finalisation
   functions, etc.
   Leave the minor heap empty.
*/
CAMLexport void caml_minor_collection (void)
{
  caml_empty_minor_heap ();

  caml_major_collection_slice (0);
  caml_force_major_slice = 0;

  if (caml_finalise_begin_hook != NULL) (*caml_finalise_begin_hook) ();
  caml_final_do_calls ();
  if (caml_finalise_end_hook != NULL) (*caml_finalise_end_hook) ();

  caml_empty_minor_heap ();
}

CAMLexport value caml_check_urgent_gc (value extra_root)
{
  CAMLparam1 (extra_root);
  if (caml_force_major_slice) caml_minor_collection();
  CAMLreturn (extra_root);
}

void caml_realloc_ref_table (struct caml_ref_table *tbl)
{                                           Assert (tbl->ptr == tbl->limit);
                                            Assert (tbl->limit <= tbl->end);
                                      Assert (tbl->limit >= tbl->threshold);

  if (tbl->base == NULL){
    caml_alloc_table (tbl, caml_minor_heap_size / sizeof (value) / 8, 256);
  }else if (tbl->limit == tbl->threshold){
    caml_gc_message (0x08, "ref_table threshold crossed\n", 0);
    tbl->limit = tbl->end;
    caml_urge_major_slice ();
  }else{ /* This will almost never happen with the bytecode interpreter. */
    asize_t sz;
    asize_t cur_ptr = tbl->ptr - tbl->base;
                                             Assert (caml_force_major_slice);

    tbl->size *= 2;
    sz = (tbl->size + tbl->reserve) * sizeof (value *);
    caml_gc_message (0x08, "Growing ref_table to %"
                           ARCH_INTNAT_PRINTF_FORMAT "dk bytes\n",
                     (intnat) sz/1024);
    tbl->base = (value **) realloc ((char *) tbl->base, sz);
    if (tbl->base == NULL){
      caml_fatal_error ("Fatal error: ref_table overflow\n");
    }
    tbl->end = tbl->base + tbl->size + tbl->reserve;
    tbl->threshold = tbl->base + tbl->size;
    tbl->ptr = tbl->base + cur_ptr;
    tbl->limit = tbl->end;
  }
}
