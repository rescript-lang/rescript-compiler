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

#ifndef CAML_MAJOR_GC_H
#define CAML_MAJOR_GC_H

#ifdef CAML_INTERNALS

#include "freelist.h"
#include "misc.h"

typedef struct {
  void *block;           /* address of the malloced block this chunk lives in */
  asize_t alloc;         /* in bytes, used for compaction */
  asize_t size;          /* in bytes */
  char *next;
} heap_chunk_head;

#define Chunk_size(c) (((heap_chunk_head *) (c)) [-1]).size
#define Chunk_alloc(c) (((heap_chunk_head *) (c)) [-1]).alloc
#define Chunk_next(c) (((heap_chunk_head *) (c)) [-1]).next
#define Chunk_block(c) (((heap_chunk_head *) (c)) [-1]).block

extern int caml_gc_phase;
extern int caml_gc_subphase;
extern uintnat caml_allocated_words;
extern double caml_extra_heap_resources;
extern uintnat caml_dependent_size, caml_dependent_allocated;
extern uintnat caml_fl_wsz_at_phase_change;

#define Phase_mark 0
#define Phase_clean 1
#define Phase_sweep 2
#define Phase_idle 3

/* Subphase of mark */
#define Subphase_mark_roots 10
/* Subphase_mark_roots: At the end of this subphase all the global
   roots are marked. */
#define Subphase_mark_main 11
/* Subphase_mark_main: At the end of this subphase all the value alive at
   the start of this subphase and created during it are marked. */
#define Subphase_mark_final 12
/* Subphase_mark_final: At the start of this subphase register which
   value with an ocaml finalizer are not marked, the associated
   finalizer will be run later. So we mark now these values as alive,
   since they must be available for their finalizer.
  */

CAMLextern char *caml_heap_start;
extern uintnat total_heap_size;
extern char *caml_gc_sweep_hp;

extern int caml_major_window;
double caml_major_ring[Max_major_window];
int caml_major_ring_index;
double caml_major_work_credit;
extern double caml_gc_clock;

/* [caml_major_gc_hook] is called just between the end of the mark
   phase and the beginning of the sweep phase of the major GC */
CAMLextern void (*caml_major_gc_hook)(void);

void caml_init_major_heap (asize_t);           /* size in bytes */
asize_t caml_clip_heap_chunk_wsz (asize_t wsz);
void caml_darken (value, value *);
void caml_major_collection_slice (intnat);
void major_collection (void);
void caml_finish_major_cycle (void);
void caml_set_major_window (int);

/* Forces finalisation of all heap-allocated values,
   disregarding both local and global roots.

   Warning: finalisation is performed by means of forced sweeping, which may
   result in pointers referencing nonexistent values; therefore the function
   should only be used on runtime shutdown.
*/
void caml_finalise_heap (void);

#endif /* CAML_INTERNALiS */

#endif /* CAML_MAJOR_GC_H */
