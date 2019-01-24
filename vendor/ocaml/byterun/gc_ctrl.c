/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
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

#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/compact.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/finalise.h"
#include "caml/freelist.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/signals.h"
#ifdef NATIVE_CODE
#include "caml/stack.h"
#else
#include "caml/stacks.h"
#endif
#include "caml/startup_aux.h"

#ifndef NATIVE_CODE
extern uintnat caml_max_stack_size;    /* defined in stacks.c */
#endif

double caml_stat_minor_words = 0.0,
       caml_stat_promoted_words = 0.0,
       caml_stat_major_words = 0.0;

intnat caml_stat_minor_collections = 0,
       caml_stat_major_collections = 0,
       caml_stat_heap_wsz = 0,
       caml_stat_top_heap_wsz = 0,
       caml_stat_compactions = 0,
       caml_stat_heap_chunks = 0;

extern uintnat caml_major_heap_increment; /* percent or words; see major_gc.c */
extern uintnat caml_percent_free;         /*        see major_gc.c */
extern uintnat caml_percent_max;          /*        see compact.c */
extern uintnat caml_allocation_policy;    /*        see freelist.c */

#define Next(hp) ((hp) + Whsize_hp (hp))

#ifdef DEBUG

/* Check that [v]'s header looks good.  [v] must be a block in the heap. */
static void check_head (value v)
{
  CAMLassert (Is_block (v));
  CAMLassert (Is_in_heap (v));

  CAMLassert (Wosize_val (v) != 0);
  CAMLassert (Color_hd (Hd_val (v)) != Caml_blue);
  CAMLassert (Is_in_heap (v));
  if (Tag_val (v) == Infix_tag){
    int offset = Wsize_bsize (Infix_offset_val (v));
    value trueval = Val_op (&Field (v, -offset));
    CAMLassert (Tag_val (trueval) == Closure_tag);
    CAMLassert (Wosize_val (trueval) > offset);
    CAMLassert (Is_in_heap (&Field (trueval, Wosize_val (trueval) - 1)));
  }else{
    CAMLassert (Is_in_heap (&Field (v, Wosize_val (v) - 1)));
  }
  if (Tag_val (v) ==  Double_tag){
    CAMLassert (Wosize_val (v) == Double_wosize);
  }else if (Tag_val (v) == Double_array_tag){
    CAMLassert (Wosize_val (v) % Double_wosize == 0);
  }
}

static void check_block (header_t *hp)
{
  mlsize_t i;
  value v = Val_hp (hp);
  value f;

  check_head (v);
  switch (Tag_hp (hp)){
  case Abstract_tag: break;
  case String_tag:
    break;
  case Double_tag:
    CAMLassert (Wosize_val (v) == Double_wosize);
    break;
  case Double_array_tag:
    CAMLassert (Wosize_val (v) % Double_wosize == 0);
    break;
  case Custom_tag:
    CAMLassert (!Is_in_heap (Custom_ops_val (v)));
    break;

  case Infix_tag:
    CAMLassert (0);
    break;

  default:
    CAMLassert (Tag_hp (hp) < No_scan_tag);
    for (i = 0; i < Wosize_hp (hp); i++){
      f = Field (v, i);
      if (Is_block (f) && Is_in_heap (f)){
        check_head (f);
        CAMLassert (Color_val (f) != Caml_blue);
      }
    }
  }
}

#endif /* DEBUG */

/* Check the heap structure (if compiled in debug mode) and
   gather statistics; return the stats if [returnstats] is true,
   otherwise return [Val_unit].
*/
static value heap_stats (int returnstats)
{
  CAMLparam0 ();
  intnat live_words = 0, live_blocks = 0,
         free_words = 0, free_blocks = 0, largest_free = 0,
         fragments = 0, heap_chunks = 0;
  char *chunk = caml_heap_start, *chunk_end;
  header_t *cur_hp;
#ifdef DEBUG
  header_t *prev_hp;
#endif
  header_t cur_hd;

#ifdef DEBUG
  caml_gc_message (-1, "### OCaml runtime: heap check ###\n");
#endif

  while (chunk != NULL){
    ++ heap_chunks;
    chunk_end = chunk + Chunk_size (chunk);
#ifdef DEBUG
    prev_hp = NULL;
#endif
    cur_hp = (header_t *) chunk;
    while (cur_hp < (header_t *) chunk_end){
      cur_hd = Hd_hp (cur_hp);
      CAMLassert (Next (cur_hp) <= (header_t *) chunk_end);
      switch (Color_hd (cur_hd)){
      case Caml_white:
        if (Wosize_hd (cur_hd) == 0){
          ++ fragments;
          CAMLassert (prev_hp == NULL
                      || Color_hp (prev_hp) != Caml_blue
                      || cur_hp == (header_t *) caml_gc_sweep_hp);
        }else{
          if (caml_gc_phase == Phase_sweep
              && cur_hp >= (header_t *) caml_gc_sweep_hp){
            ++ free_blocks;
            free_words += Whsize_hd (cur_hd);
            if (Whsize_hd (cur_hd) > largest_free){
              largest_free = Whsize_hd (cur_hd);
            }
          }else{
            ++ live_blocks;
            live_words += Whsize_hd (cur_hd);
#ifdef DEBUG
            check_block (cur_hp);
#endif
          }
        }
        break;
      case Caml_gray: case Caml_black:
        CAMLassert (Wosize_hd (cur_hd) > 0);
        ++ live_blocks;
        live_words += Whsize_hd (cur_hd);
#ifdef DEBUG
        check_block (cur_hp);
#endif
        break;
      case Caml_blue:
        CAMLassert (Wosize_hd (cur_hd) > 0);
        ++ free_blocks;
        free_words += Whsize_hd (cur_hd);
        if (Whsize_hd (cur_hd) > largest_free){
          largest_free = Whsize_hd (cur_hd);
        }
        /* not true any more with big heap chunks
        CAMLassert (prev_hp == NULL
                    || (Color_hp (prev_hp) != Caml_blue
                        && Wosize_hp (prev_hp) > 0)
                    || cur_hp == caml_gc_sweep_hp);
        CAMLassert (Next (cur_hp) == chunk_end
                    || (Color_hp (Next (cur_hp)) != Caml_blue 
                       && Wosize_hp (Next (cur_hp)) > 0)
                    || (Whsize_hd (cur_hd) + Wosize_hp (Next (cur_hp))
                       > Max_wosize)
                    || Next (cur_hp) == caml_gc_sweep_hp);
        */
        break;
      }
#ifdef DEBUG
      prev_hp = cur_hp;
#endif
      cur_hp = Next (cur_hp);
    }
    CAMLassert (cur_hp == (header_t *) chunk_end);
    chunk = Chunk_next (chunk);
  }

#ifdef DEBUG
  caml_final_invariant_check();
#endif

  CAMLassert (heap_chunks == caml_stat_heap_chunks);
  CAMLassert (live_words + free_words + fragments == caml_stat_heap_wsz);

  if (returnstats){
    CAMLlocal1 (res);

    /* get a copy of these before allocating anything... */
    double minwords = caml_stat_minor_words
                      + (double) (caml_young_alloc_end - caml_young_ptr);
    double prowords = caml_stat_promoted_words;
    double majwords = caml_stat_major_words + (double) caml_allocated_words;
    intnat mincoll = caml_stat_minor_collections;
    intnat majcoll = caml_stat_major_collections;
    intnat heap_words = caml_stat_heap_wsz;
    intnat cpct = caml_stat_compactions;
    intnat top_heap_words = caml_stat_top_heap_wsz;

    res = caml_alloc_tuple (16);
    Store_field (res, 0, caml_copy_double (minwords));
    Store_field (res, 1, caml_copy_double (prowords));
    Store_field (res, 2, caml_copy_double (majwords));
    Store_field (res, 3, Val_long (mincoll));
    Store_field (res, 4, Val_long (majcoll));
    Store_field (res, 5, Val_long (heap_words));
    Store_field (res, 6, Val_long (heap_chunks));
    Store_field (res, 7, Val_long (live_words));
    Store_field (res, 8, Val_long (live_blocks));
    Store_field (res, 9, Val_long (free_words));
    Store_field (res, 10, Val_long (free_blocks));
    Store_field (res, 11, Val_long (largest_free));
    Store_field (res, 12, Val_long (fragments));
    Store_field (res, 13, Val_long (cpct));
    Store_field (res, 14, Val_long (top_heap_words));
    Store_field (res, 15, Val_long (caml_stack_usage()));
    CAMLreturn (res);
  }else{
    CAMLreturn (Val_unit);
  }
}

#ifdef DEBUG
void caml_heap_check (void)
{
  heap_stats (0);
}
#endif

CAMLprim value caml_gc_stat(value v)
{
  value result;
  CAML_INSTR_SETUP (tmr, "");
  CAMLassert (v == Val_unit);
  result = heap_stats (1);
  CAML_INSTR_TIME (tmr, "explicit/gc_stat");
  return result;
}

CAMLprim value caml_gc_quick_stat(value v)
{
  CAMLparam0 ();
  CAMLlocal1 (res);

  /* get a copy of these before allocating anything... */
  double minwords = caml_stat_minor_words
                    + (double) (caml_young_alloc_end - caml_young_ptr);
  double prowords = caml_stat_promoted_words;
  double majwords = caml_stat_major_words + (double) caml_allocated_words;
  intnat mincoll = caml_stat_minor_collections;
  intnat majcoll = caml_stat_major_collections;
  intnat heap_words = caml_stat_heap_wsz;
  intnat top_heap_words = caml_stat_top_heap_wsz;
  intnat cpct = caml_stat_compactions;
  intnat heap_chunks = caml_stat_heap_chunks;

  res = caml_alloc_tuple (16);
  Store_field (res, 0, caml_copy_double (minwords));
  Store_field (res, 1, caml_copy_double (prowords));
  Store_field (res, 2, caml_copy_double (majwords));
  Store_field (res, 3, Val_long (mincoll));
  Store_field (res, 4, Val_long (majcoll));
  Store_field (res, 5, Val_long (heap_words));
  Store_field (res, 6, Val_long (heap_chunks));
  Store_field (res, 7, Val_long (0));
  Store_field (res, 8, Val_long (0));
  Store_field (res, 9, Val_long (0));
  Store_field (res, 10, Val_long (0));
  Store_field (res, 11, Val_long (0));
  Store_field (res, 12, Val_long (0));
  Store_field (res, 13, Val_long (cpct));
  Store_field (res, 14, Val_long (top_heap_words));
  Store_field (res, 15, Val_long (caml_stack_usage()));
  CAMLreturn (res);
}

double caml_gc_minor_words_unboxed()
{
  return (caml_stat_minor_words
          + (double) (caml_young_alloc_end - caml_young_ptr));
}

CAMLprim value caml_gc_minor_words(value v)
{
  CAMLparam0 ();   /* v is ignored */
  CAMLreturn(caml_copy_double(caml_gc_minor_words_unboxed()));
}

CAMLprim value caml_gc_counters(value v)
{
  CAMLparam0 ();   /* v is ignored */
  CAMLlocal1 (res);

  /* get a copy of these before allocating anything... */
  double minwords = caml_stat_minor_words
                    + (double) (caml_young_alloc_end - caml_young_ptr);
  double prowords = caml_stat_promoted_words;
  double majwords = caml_stat_major_words + (double) caml_allocated_words;

  res = caml_alloc_tuple (3);
  Store_field (res, 0, caml_copy_double (minwords));
  Store_field (res, 1, caml_copy_double (prowords));
  Store_field (res, 2, caml_copy_double (majwords));
  CAMLreturn (res);
}

CAMLprim value caml_gc_huge_fallback_count (value v)
{
  return Val_long (caml_huge_fallback_count);
}

CAMLprim value caml_gc_get(value v)
{
  CAMLparam0 ();   /* v is ignored */
  CAMLlocal1 (res);

  res = caml_alloc_tuple (8);
  Store_field (res, 0, Val_long (caml_minor_heap_wsz));                 /* s */
  Store_field (res, 1, Val_long (caml_major_heap_increment));           /* i */
  Store_field (res, 2, Val_long (caml_percent_free));                   /* o */
  Store_field (res, 3, Val_long (caml_verb_gc));                        /* v */
  Store_field (res, 4, Val_long (caml_percent_max));                    /* O */
#ifndef NATIVE_CODE
  Store_field (res, 5, Val_long (caml_max_stack_size));                 /* l */
#else
  Store_field (res, 5, Val_long (0));
#endif
  Store_field (res, 6, Val_long (caml_allocation_policy));              /* a */
  Store_field (res, 7, Val_long (caml_major_window));                   /* w */
  CAMLreturn (res);
}

#define Max(x,y) ((x) < (y) ? (y) : (x))

static uintnat norm_pfree (uintnat p)
{
  return Max (p, 1);
}

static uintnat norm_pmax (uintnat p)
{
  return p;
}

static intnat norm_minsize (intnat s)
{
  if (s < Minor_heap_min) s = Minor_heap_min;
  if (s > Minor_heap_max) s = Minor_heap_max;
  return s;
}

static uintnat norm_window (intnat w)
{
  if (w < 1) w = 1;
  if (w > Max_major_window) w = Max_major_window;
  return w;
}

CAMLprim value caml_gc_set(value v)
{
  uintnat newpf, newpm;
  asize_t newheapincr;
  asize_t newminwsz;
  uintnat oldpolicy;
  CAML_INSTR_SETUP (tmr, "");

  caml_verb_gc = Long_val (Field (v, 3));

#ifndef NATIVE_CODE
  caml_change_max_stack_size (Long_val (Field (v, 5)));
#endif

  newpf = norm_pfree (Long_val (Field (v, 2)));
  if (newpf != caml_percent_free){
    caml_percent_free = newpf;
    caml_gc_message (0x20, "New space overhead: %"
                     ARCH_INTNAT_PRINTF_FORMAT "u%%\n", caml_percent_free);
  }

  newpm = norm_pmax (Long_val (Field (v, 4)));
  if (newpm != caml_percent_max){
    caml_percent_max = newpm;
    caml_gc_message (0x20, "New max overhead: %"
                     ARCH_INTNAT_PRINTF_FORMAT "u%%\n", caml_percent_max);
  }

  newheapincr = Long_val (Field (v, 1));
  if (newheapincr != caml_major_heap_increment){
    caml_major_heap_increment = newheapincr;
    if (newheapincr > 1000){
      caml_gc_message (0x20, "New heap increment size: %"
                       ARCH_INTNAT_PRINTF_FORMAT "uk words\n",
                       caml_major_heap_increment/1024);
    }else{
      caml_gc_message (0x20, "New heap increment size: %"
                       ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                       caml_major_heap_increment);
    }
  }
  oldpolicy = caml_allocation_policy;
  caml_set_allocation_policy (Long_val (Field (v, 6)));
  if (oldpolicy != caml_allocation_policy){
    caml_gc_message (0x20, "New allocation policy: %"
                     ARCH_INTNAT_PRINTF_FORMAT "u\n", caml_allocation_policy);
  }

  /* This field was added in 4.03.0. */
  if (Wosize_val (v) >= 8){
    int old_window = caml_major_window;
    caml_set_major_window (norm_window (Long_val (Field (v, 7))));
    if (old_window != caml_major_window){
      caml_gc_message (0x20, "New smoothing window size: %d\n",
                       caml_major_window);
    }
  }

    /* Minor heap size comes last because it will trigger a minor collection
       (thus invalidating [v]) and it can raise [Out_of_memory]. */
  newminwsz = norm_minsize (Long_val (Field (v, 0)));
  if (newminwsz != caml_minor_heap_wsz){
    caml_gc_message (0x20, "New minor heap size: %"
                     ARCH_SIZET_PRINTF_FORMAT "uk words\n", newminwsz / 1024);
    caml_set_minor_heap_size (Bsize_wsize (newminwsz));
  }
  CAML_INSTR_TIME (tmr, "explicit/gc_set");
  return Val_unit;
}

CAMLprim value caml_gc_minor(value v)
{
  CAML_INSTR_SETUP (tmr, "");
  CAMLassert (v == Val_unit);
  caml_request_minor_gc ();
  caml_gc_dispatch ();
  CAML_INSTR_TIME (tmr, "explicit/gc_minor");
  return Val_unit;
}

static void test_and_compact (void)
{
  float fp;

  fp = 100.0 * caml_fl_cur_wsz / (caml_stat_heap_wsz - caml_fl_cur_wsz);
  if (fp > 999999.0) fp = 999999.0;
  caml_gc_message (0x200, "Estimated overhead (lower bound) = %"
                          ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                   (uintnat) fp);
  if (fp >= caml_percent_max){
    caml_gc_message (0x200, "Automatic compaction triggered.\n");
    caml_compact_heap ();
  }
}

CAMLprim value caml_gc_major(value v)
{
  CAML_INSTR_SETUP (tmr, "");
  CAMLassert (v == Val_unit);
  caml_gc_message (0x1, "Major GC cycle requested\n");
  caml_empty_minor_heap ();
  caml_finish_major_cycle ();
  test_and_compact ();
  caml_final_do_calls ();
  CAML_INSTR_TIME (tmr, "explicit/gc_major");
  return Val_unit;
}

CAMLprim value caml_gc_full_major(value v)
{
  CAML_INSTR_SETUP (tmr, "");
  CAMLassert (v == Val_unit);
  caml_gc_message (0x1, "Full major GC cycle requested\n");
  caml_empty_minor_heap ();
  caml_finish_major_cycle ();
  caml_final_do_calls ();
  caml_empty_minor_heap ();
  caml_finish_major_cycle ();
  test_and_compact ();
  caml_final_do_calls ();
  CAML_INSTR_TIME (tmr, "explicit/gc_full_major");
  return Val_unit;
}

CAMLprim value caml_gc_major_slice (value v)
{
  CAML_INSTR_SETUP (tmr, "");
  CAMLassert (Is_long (v));
  caml_major_collection_slice (Long_val (v));
  CAML_INSTR_TIME (tmr, "explicit/gc_major_slice");
  return Val_long (0);
}

CAMLprim value caml_gc_compaction(value v)
{
  CAML_INSTR_SETUP (tmr, "");
  CAMLassert (v == Val_unit);
  caml_gc_message (0x10, "Heap compaction requested\n");
  caml_empty_minor_heap ();
  caml_finish_major_cycle ();
  caml_final_do_calls ();
  caml_empty_minor_heap ();
  caml_finish_major_cycle ();
  caml_compact_heap ();
  caml_final_do_calls ();
  CAML_INSTR_TIME (tmr, "explicit/gc_compact");
  return Val_unit;
}

CAMLprim value caml_get_minor_free (value v)
{
  return Val_int (caml_young_ptr - caml_young_alloc_start);
}

CAMLprim value caml_get_major_bucket (value v)
{
  long i = Long_val (v);
  if (i < 0) caml_invalid_argument ("Gc.get_bucket");
  if (i < caml_major_window){
    i += caml_major_ring_index;
    if (i >= caml_major_window) i -= caml_major_window;
    CAMLassert (0 <= i && i < caml_major_window);
    return Val_long ((long) (caml_major_ring[i] * 1e6));
  }else{
    return Val_long (0);
  }
}

CAMLprim value caml_get_major_credit (value v)
{
  CAMLassert (v == Val_unit);
  return Val_long ((long) (caml_major_work_credit * 1e6));
}

uintnat caml_normalize_heap_increment (uintnat i)
{
  if (i < Bsize_wsize (Heap_chunk_min)){
    i = Bsize_wsize (Heap_chunk_min);
  }
  return ((i + Page_size - 1) >> Page_log) << Page_log;
}

/* [minor_size] and [major_size] are numbers of words
   [major_incr] is either a percentage or a number of words */
void caml_init_gc (uintnat minor_size, uintnat major_size,
                   uintnat major_incr, uintnat percent_fr,
                   uintnat percent_m, uintnat window)
{
  uintnat major_heap_size =
    Bsize_wsize (caml_normalize_heap_increment (major_size));

  CAML_INSTR_INIT ();
  if (caml_init_alloc_for_heap () != 0){
    caml_fatal_error ("cannot initialize heap: mmap failed\n");
  }
  if (caml_page_table_initialize(Bsize_wsize(minor_size) + major_heap_size)){
    caml_fatal_error ("OCaml runtime error: cannot initialize page table\n");
  }
  caml_set_minor_heap_size (Bsize_wsize (norm_minsize (minor_size)));
  caml_major_heap_increment = major_incr;
  caml_percent_free = norm_pfree (percent_fr);
  caml_percent_max = norm_pmax (percent_m);
  caml_init_major_heap (major_heap_size);
  caml_major_window = norm_window (window);
  caml_gc_message (0x20, "Initial minor heap size: %"
                   ARCH_SIZET_PRINTF_FORMAT "uk words\n",
                   caml_minor_heap_wsz / 1024);
  caml_gc_message (0x20, "Initial major heap size: %"
                   ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                   major_heap_size / 1024);
  caml_gc_message (0x20, "Initial space overhead: %"
                   ARCH_INTNAT_PRINTF_FORMAT "u%%\n", caml_percent_free);
  caml_gc_message (0x20, "Initial max overhead: %"
                   ARCH_INTNAT_PRINTF_FORMAT "u%%\n", caml_percent_max);
  if (caml_major_heap_increment > 1000){
    caml_gc_message (0x20, "Initial heap increment: %"
                     ARCH_INTNAT_PRINTF_FORMAT "uk words\n",
                     caml_major_heap_increment / 1024);
  }else{
    caml_gc_message (0x20, "Initial heap increment: %"
                     ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                     caml_major_heap_increment);
  }
  caml_gc_message (0x20, "Initial allocation policy: %"
                   ARCH_INTNAT_PRINTF_FORMAT "u\n", caml_allocation_policy);
  caml_gc_message (0x20, "Initial smoothing window: %d\n",
                   caml_major_window);
}


/* FIXME After the startup_aux.c unification, move these functions there. */

CAMLprim value caml_runtime_variant (value unit)
{
  CAMLassert (unit == Val_unit);
#if defined (DEBUG)
  return caml_copy_string ("d");
#elif defined (CAML_INSTR)
  return caml_copy_string ("i");
#else
  return caml_copy_string ("");
#endif
}

extern int caml_parser_trace;

CAMLprim value caml_runtime_parameters (value unit)
{
#define F_Z ARCH_INTNAT_PRINTF_FORMAT
#define F_S ARCH_SIZET_PRINTF_FORMAT

  CAMLassert (unit == Val_unit);
  return caml_alloc_sprintf
    ("a=%d,b=%d,H=%"F_Z"u,i=%"F_Z"u,l=%"F_Z"u,o=%"F_Z"u,O=%"F_Z"u,p=%d,s=%"F_S"u,t=%"F_Z"u,v=%"F_Z"u,w=%d,W=%"F_Z"u",
     /* a */ (int) caml_allocation_policy,
     /* b */ caml_backtrace_active,
     /* h */ /* missing */ /* FIXME add when changed to min_heap_size */
     /* H */ caml_use_huge_pages,
     /* i */ caml_major_heap_increment,
#ifdef NATIVE_CODE
     /* l */ (uintnat) 0,
#else
     /* l */ caml_max_stack_size,
#endif
     /* o */ caml_percent_free,
     /* O */ caml_percent_max,
     /* p */ caml_parser_trace,
     /* R */ /* missing */
     /* s */ caml_minor_heap_wsz,
     /* t */ caml_trace_level,
     /* v */ caml_verb_gc,
     /* w */ caml_major_window,
     /* W */ caml_runtime_warnings
     );
#undef F_Z
#undef F_S
}

/* Control runtime warnings */

CAMLprim value caml_ml_enable_runtime_warnings(value vbool)
{
  caml_runtime_warnings = Bool_val(vbool);
  return Val_unit;
}

CAMLprim value caml_ml_runtime_warnings_enabled(value unit)
{
  CAMLassert (unit == Val_unit);
  return Val_bool(caml_runtime_warnings);
}
