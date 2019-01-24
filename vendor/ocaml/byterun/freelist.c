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

#define FREELIST_DEBUG 0
#if FREELIST_DEBUG
#include <stdio.h>
#endif

#include <string.h>

#include "caml/config.h"
#include "caml/freelist.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/memory.h"
#include "caml/major_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

/* The free-list is kept sorted by increasing addresses.
   This makes the merging of adjacent free blocks possible.
   (See [caml_fl_merge_block].)
*/

/* A free list block is a [value] (integer representing a pointer to the
   first word after the block's header). The end of the  list is NULL. */
#define Val_NULL ((value) NULL)

/* The sentinel can be located anywhere in memory, but it must not be
   adjacent to any heap object. */
static struct {
  value filler1; /* Make sure the sentinel is never adjacent to any block. */
  header_t h;
  value first_field;
  value filler2; /* Make sure the sentinel is never adjacent to any block. */
} sentinel = {0, Make_header (0, 0, Caml_blue), Val_NULL, 0};

#define Fl_head (Val_bp (&(sentinel.first_field)))
static value fl_prev = Fl_head;  /* Current allocation pointer. */
static value fl_last = Val_NULL; /* Last block in the list.  Only valid
                                  just after [caml_fl_allocate] returns NULL. */
value caml_fl_merge = Fl_head;   /* Current insertion pointer.  Managed
                                    jointly with [sweep_slice]. */
asize_t caml_fl_cur_wsz = 0;     /* Number of words in the free list,
                                    including headers but not fragments. */

#define FLP_MAX 1000
static value flp [FLP_MAX];
static int flp_size = 0;
static value beyond = Val_NULL;

#define Next(b) (Field (b, 0))

#define Policy_next_fit 0
#define Policy_first_fit 1
uintnat caml_allocation_policy = Policy_next_fit;
#define policy caml_allocation_policy

#ifdef DEBUG
static void fl_check (void)
{
  value cur, prev;
  int prev_found = 0, flp_found = 0, merge_found = 0;
  uintnat size_found = 0;
  int sz = 0;

  prev = Fl_head;
  cur = Next (prev);
  while (cur != Val_NULL){
    size_found += Whsize_bp (cur);
    CAMLassert (Is_in_heap (cur));
    if (cur == fl_prev) prev_found = 1;
    if (policy == Policy_first_fit && Wosize_bp (cur) > sz){
      sz = Wosize_bp (cur);
      if (flp_found < flp_size){
        CAMLassert (Next (flp[flp_found]) == cur);
        ++ flp_found;
      }else{
        CAMLassert (beyond == Val_NULL || cur >= Next (beyond));
      }
    }
    if (cur == caml_fl_merge) merge_found = 1;
    prev = cur;
    cur = Next (prev);
  }
  if (policy == Policy_next_fit) CAMLassert (prev_found || fl_prev == Fl_head);
  if (policy == Policy_first_fit) CAMLassert (flp_found == flp_size);
  CAMLassert (merge_found || caml_fl_merge == Fl_head);
  CAMLassert (size_found == caml_fl_cur_wsz);
}

#endif

/* [allocate_block] is called by [caml_fl_allocate].  Given a suitable free
   block and the requested size, it allocates a new block from the free
   block.  There are three cases:
   0. The free block has the requested size. Detach the block from the
      free-list and return it.
   1. The free block is 1 word longer than the requested size. Detach
      the block from the free list.  The remaining word cannot be linked:
      turn it into an empty block (header only), and return the rest.
   2. The free block is large enough. Split it in two and return the right
      block.
   In all cases, the allocated block is right-justified in the free block:
   it is located in the high-address words of the free block, so that
   the linking of the free-list does not change in case 2.
*/
static header_t *allocate_block (mlsize_t wh_sz, int flpi, value prev,
                                 value cur)
{
  header_t h = Hd_bp (cur);
  CAMLassert (Whsize_hd (h) >= wh_sz);
  if (Wosize_hd (h) < wh_sz + 1){                        /* Cases 0 and 1. */
    caml_fl_cur_wsz -= Whsize_hd (h);
    Next (prev) = Next (cur);
    CAMLassert (Is_in_heap (Next (prev)) || Next (prev) == Val_NULL);
    if (caml_fl_merge == cur) caml_fl_merge = prev;
#ifdef DEBUG
    fl_last = Val_NULL;
#endif
      /* In case 1, the following creates the empty block correctly.
         In case 0, it gives an invalid header to the block.  The function
         calling [caml_fl_allocate] will overwrite it. */
    Hd_op (cur) = Make_header (0, 0, Caml_white);
    if (policy == Policy_first_fit){
      if (flpi + 1 < flp_size && flp[flpi + 1] == cur){
        flp[flpi + 1] = prev;
      }else if (flpi == flp_size - 1){
        beyond = (prev == Fl_head) ? Val_NULL : prev;
        -- flp_size;
      }
    }
  }else{                                                        /* Case 2. */
    caml_fl_cur_wsz -= wh_sz;
    Hd_op (cur) = Make_header (Wosize_hd (h) - wh_sz, 0, Caml_blue);
  }
  if (policy == Policy_next_fit) fl_prev = prev;
  return (header_t *) &Field (cur, Wosize_hd (h) - wh_sz);
}

#ifdef CAML_INSTR
static uintnat instr_size [20] =
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static char *instr_name [20] = {
  NULL,
  "alloc01@",
  "alloc02@",
  "alloc03@",
  "alloc04@",
  "alloc05@",
  "alloc06@",
  "alloc07@",
  "alloc08@",
  "alloc09@",
  "alloc10-19@",
  "alloc20-29@",
  "alloc30-39@",
  "alloc40-49@",
  "alloc50-59@",
  "alloc60-69@",
  "alloc70-79@",
  "alloc80-89@",
  "alloc90-99@",
  "alloc_large@",
};
uintnat caml_instr_alloc_jump = 0;
/* number of pointers followed to allocate from the free list */
#endif /*CAML_INSTR*/

/* [caml_fl_allocate] does not set the header of the newly allocated block.
   The calling function must do it before any GC function gets called.
   [caml_fl_allocate] returns a head pointer.
*/
header_t *caml_fl_allocate (mlsize_t wo_sz)
{
  value cur = Val_NULL, prev;
  header_t *result;
  int i;
  mlsize_t sz, prevsz;
  CAMLassert (sizeof (char *) == sizeof (value));
  CAMLassert (wo_sz >= 1);
#ifdef CAML_INSTR
  if (wo_sz < 10){
    ++instr_size[wo_sz];
  }else if (wo_sz < 100){
    ++instr_size[wo_sz/10 + 9];
  }else{
    ++instr_size[19];
  }
#endif /* CAML_INSTR */

  switch (policy){
  case Policy_next_fit:
    CAMLassert (fl_prev != Val_NULL);
    /* Search from [fl_prev] to the end of the list. */
    prev = fl_prev;
    cur = Next (prev);
    while (cur != Val_NULL){
      CAMLassert (Is_in_heap (cur));
      if (Wosize_bp (cur) >= wo_sz){
        return allocate_block (Whsize_wosize (wo_sz), 0, prev, cur);
      }
      prev = cur;
      cur = Next (prev);
#ifdef CAML_INSTR
      ++ caml_instr_alloc_jump;
#endif
    }
    fl_last = prev;
    /* Search from the start of the list to [fl_prev]. */
    prev = Fl_head;
    cur = Next (prev);
    while (prev != fl_prev){
      if (Wosize_bp (cur) >= wo_sz){
        return allocate_block (Whsize_wosize (wo_sz), 0, prev, cur);
      }
      prev = cur;
      cur = Next (prev);
#ifdef CAML_INSTR
      ++ caml_instr_alloc_jump;
#endif
    }
    /* No suitable block was found. */
    return NULL;
    break;

  case Policy_first_fit: {
    /* Search in the flp array. */
    for (i = 0; i < flp_size; i++){
      sz = Wosize_bp (Next (flp[i]));
      if (sz >= wo_sz){
#if FREELIST_DEBUG
        if (i > 5) fprintf (stderr, "FLP: found at %d  size=%d\n", i, wo_sz);
#endif
        result = allocate_block (Whsize_wosize (wo_sz), i, flp[i],
                                 Next (flp[i]));
        goto update_flp;
      }
    }
    /* Extend the flp array. */
    if (flp_size == 0){
      prev = Fl_head;
      prevsz = 0;
    }else{
      prev = Next (flp[flp_size - 1]);
      prevsz = Wosize_bp (prev);
      if (beyond != Val_NULL) prev = beyond;
    }
    while (flp_size < FLP_MAX){
      cur = Next (prev);
      if (cur == Val_NULL){
        fl_last = prev;
        beyond = (prev == Fl_head) ? Val_NULL : prev;
        return NULL;
      }else{
        sz = Wosize_bp (cur);
        if (sz > prevsz){
          flp[flp_size] = prev;
          ++ flp_size;
          if (sz >= wo_sz){
            beyond = cur;
            i = flp_size - 1;
#if FREELIST_DEBUG
            if (flp_size > 5){
              fprintf (stderr, "FLP: extended to %d\n", flp_size);
            }
#endif
            result = allocate_block (Whsize_wosize (wo_sz), flp_size - 1, prev,
                                     cur);
            goto update_flp;
          }
          prevsz = sz;
        }
      }
      prev = cur;
    }
    beyond = cur;

    /* The flp table is full.  Do a slow first-fit search. */
#if FREELIST_DEBUG
    fprintf (stderr, "FLP: table is full -- slow first-fit\n");
#endif
    if (beyond != Val_NULL){
      prev = beyond;
    }else{
      prev = flp[flp_size - 1];
    }
    prevsz = Wosize_bp (Next (flp[FLP_MAX-1]));
    CAMLassert (prevsz < wo_sz);
    cur = Next (prev);
    while (cur != Val_NULL){
      CAMLassert (Is_in_heap (cur));
      sz = Wosize_bp (cur);
      if (sz < prevsz){
        beyond = cur;
      }else if (sz >= wo_sz){
        return allocate_block (Whsize_wosize (wo_sz), flp_size, prev, cur);
      }
      prev = cur;
      cur = Next (prev);
    }
    fl_last = prev;
    return NULL;

  update_flp: /* (i, sz) */
    /* The block at [i] was removed or reduced.  Update the table. */
    CAMLassert (0 <= i && i < flp_size + 1);
    if (i < flp_size){
      if (i > 0){
        prevsz = Wosize_bp (Next (flp[i-1]));
      }else{
        prevsz = 0;
      }
      if (i == flp_size - 1){
        if (Wosize_bp (Next (flp[i])) <= prevsz){
          beyond = Next (flp[i]);
          -- flp_size;
        }else{
          beyond = Val_NULL;
        }
      }else{
        value buf [FLP_MAX];
        int j = 0;
        mlsize_t oldsz = sz;

        prev = flp[i];
        while (prev != flp[i+1]){
          cur = Next (prev);
          sz = Wosize_bp (cur);
          if (sz > prevsz){
            buf[j++] = prev;
            prevsz = sz;
            if (sz >= oldsz){
              CAMLassert (sz == oldsz);
              break;
            }
          }
          prev = cur;
        }
#if FREELIST_DEBUG
        if (j > 2) fprintf (stderr, "FLP: update; buf size = %d\n", j);
#endif
        if (FLP_MAX >= flp_size + j - 1){
          if (j != 1){
            memmove (&flp[i+j], &flp[i+1], sizeof (value) * (flp_size-i-1));
          }
          if (j > 0) memmove (&flp[i], &buf[0], sizeof (value) * j);
          flp_size += j - 1;
        }else{
          if (FLP_MAX > i + j){
            if (j != 1){
              memmove (&flp[i+j], &flp[i+1], sizeof (value) * (FLP_MAX-i-j));
            }
            if (j > 0) memmove (&flp[i], &buf[0], sizeof (value) * j);
          }else{
            if (i != FLP_MAX){
              memmove (&flp[i], &buf[0], sizeof (value) * (FLP_MAX - i));
            }
          }
          flp_size = FLP_MAX - 1;
          beyond = Next (flp[FLP_MAX - 1]);
        }
      }
    }
    return result;
  }
  break;

  default:
    CAMLassert (0);   /* unknown policy */
    break;
  }
  return NULL;  /* NOT REACHED */
}

/* Location of the last fragment seen by the sweeping code.
   This is a pointer to the first word after the fragment, which is
   the header of the next block.
   Note that [last_fragment] doesn't point to the fragment itself,
   but to the block after it.
*/
static header_t *last_fragment;

void caml_fl_init_merge (void)
{
#ifdef CAML_INSTR
  int i;
  for (i = 1; i < 20; i++){
    CAML_INSTR_INT (instr_name[i], instr_size[i]);
    instr_size[i] = 0;
  }
#endif /* CAML_INSTR */
  last_fragment = NULL;
  caml_fl_merge = Fl_head;
#ifdef DEBUG
  fl_check ();
#endif
}

static void truncate_flp (value changed)
{
  if (changed == Fl_head){
    flp_size = 0;
    beyond = Val_NULL;
  }else{
    while (flp_size > 0 && Next (flp[flp_size - 1]) >= changed)
      -- flp_size;
    if (beyond >= changed) beyond = Val_NULL;
  }
}

/* This is called by caml_compact_heap. */
void caml_fl_reset (void)
{
  Next (Fl_head) = Val_NULL;
  switch (policy){
  case Policy_next_fit:
    fl_prev = Fl_head;
    break;
  case Policy_first_fit:
    truncate_flp (Fl_head);
    break;
  default:
    CAMLassert (0);
    break;
  }
  caml_fl_cur_wsz = 0;
  caml_fl_init_merge ();
}

/* [caml_fl_merge_block] returns the head pointer of the next block after [bp],
   because merging blocks may change the size of [bp]. */
header_t *caml_fl_merge_block (value bp)
{
  value prev, cur;
  header_t *adj;
  header_t hd = Hd_val (bp);
  mlsize_t prev_wosz;

  caml_fl_cur_wsz += Whsize_hd (hd);

#ifdef DEBUG
  caml_set_fields (bp, 0, Debug_free_major);
#endif
  prev = caml_fl_merge;
  cur = Next (prev);
  /* The sweep code makes sure that this is the right place to insert
     this block: */
  CAMLassert (prev < bp || prev == Fl_head);
  CAMLassert (cur > bp || cur == Val_NULL);

  if (policy == Policy_first_fit) truncate_flp (prev);

  /* If [last_fragment] and [bp] are adjacent, merge them. */
  if (last_fragment == Hp_bp (bp)){
    mlsize_t bp_whsz = Whsize_val (bp);
    if (bp_whsz <= Max_wosize){
      hd = Make_header (bp_whsz, 0, Caml_white);
      bp = (value) last_fragment;
      Hd_val (bp) = hd;
      caml_fl_cur_wsz += Whsize_wosize (0);
    }
  }

  /* If [bp] and [cur] are adjacent, remove [cur] from the free-list
     and merge them. */
  adj = (header_t *) &Field (bp, Wosize_hd (hd));
  if (adj == Hp_val (cur)){
    value next_cur = Next (cur);
    mlsize_t cur_whsz = Whsize_val (cur);

    if (Wosize_hd (hd) + cur_whsz <= Max_wosize){
      Next (prev) = next_cur;
      if (policy == Policy_next_fit && fl_prev == cur) fl_prev = prev;
      hd = Make_header (Wosize_hd (hd) + cur_whsz, 0, Caml_blue);
      Hd_val (bp) = hd;
      adj = (header_t *) &Field (bp, Wosize_hd (hd));
#ifdef DEBUG
      fl_last = Val_NULL;
      Next (cur) = (value) Debug_free_major;
      Hd_val (cur) = Debug_free_major;
#endif
      cur = next_cur;
    }
  }
  /* If [prev] and [bp] are adjacent merge them, else insert [bp] into
     the free-list if it is big enough. */
  prev_wosz = Wosize_val (prev);
  if ((header_t *) &Field (prev, prev_wosz) == Hp_val (bp)
      && prev_wosz + Whsize_hd (hd) < Max_wosize){
    Hd_val (prev) = Make_header (prev_wosz + Whsize_hd (hd), 0,Caml_blue);
#ifdef DEBUG
    Hd_val (bp) = Debug_free_major;
#endif
    CAMLassert (caml_fl_merge == prev);
  }else if (Wosize_hd (hd) != 0){
    Hd_val (bp) = Bluehd_hd (hd);
    Next (bp) = cur;
    Next (prev) = bp;
    caml_fl_merge = bp;
  }else{
    /* This is a fragment.  Leave it in white but remember it for eventual
       merging with the next block. */
    last_fragment = (header_t *) bp;
    caml_fl_cur_wsz -= Whsize_wosize (0);
  }
  return adj;
}

/* This is a heap extension.  We have to insert it in the right place
   in the free-list.
   [caml_fl_add_blocks] can only be called right after a call to
   [caml_fl_allocate] that returned Val_NULL.
   Most of the heap extensions are expected to be at the end of the
   free list.  (This depends on the implementation of [malloc].)

   [bp] must point to a list of blocks chained by their field 0,
   terminated by Val_NULL, and field 1 of the first block must point to
   the last block.
*/
void caml_fl_add_blocks (value bp)
{
  CAMLassert (fl_last != Val_NULL);
  CAMLassert (Next (fl_last) == Val_NULL);
  caml_fl_cur_wsz += Whsize_bp (bp);

  if (bp > fl_last){
    Next (fl_last) = bp;
    if (fl_last == caml_fl_merge && (char *) bp < caml_gc_sweep_hp){
      caml_fl_merge = Field (bp, 1);
    }
    if (policy == Policy_first_fit && flp_size < FLP_MAX){
      flp [flp_size++] = fl_last;
    }
  }else{
    value cur, prev;

    prev = Fl_head;
    cur = Next (prev);
    while (cur != Val_NULL && cur < bp){
      CAMLassert (prev < bp || prev == Fl_head);
      /* XXX TODO: extend flp on the fly */
      prev = cur;
      cur = Next (prev);
    }
    CAMLassert (prev < bp || prev == Fl_head);
    CAMLassert (cur > bp || cur == Val_NULL);
    Next (Field (bp, 1)) = cur;
    Next (prev) = bp;
    /* When inserting blocks between [caml_fl_merge] and [caml_gc_sweep_hp],
       we must advance [caml_fl_merge] to the new block, so that [caml_fl_merge]
       is always the last free-list block before [caml_gc_sweep_hp]. */
    if (prev == caml_fl_merge && (char *) bp < caml_gc_sweep_hp){
      caml_fl_merge = Field (bp, 1);
    }
    if (policy == Policy_first_fit) truncate_flp (bp);
  }
}

/* Cut a block of memory into Max_wosize pieces, give them headers,
   and optionally merge them into the free list.
   arguments:
   p: pointer to the first word of the block
   size: size of the block (in words)
   do_merge: 1 -> do merge; 0 -> do not merge
   color: which color to give to the pieces; if [do_merge] is 1, this
          is overridden by the merge code, but we have historically used
          [Caml_white].
*/
void caml_make_free_blocks (value *p, mlsize_t size, int do_merge, int color)
{
  mlsize_t sz;

  while (size > 0){
    if (size > Whsize_wosize (Max_wosize)){
      sz = Whsize_wosize (Max_wosize);
    }else{
      sz = size;
    }
    *(header_t *)p =
      Make_header (Wosize_whsize (sz), 0, color);
    if (do_merge) caml_fl_merge_block (Val_hp (p));
    size -= sz;
    p += sz;
  }
}

void caml_set_allocation_policy (uintnat p)
{
  switch (p){
  case Policy_next_fit:
    fl_prev = Fl_head;
    policy = p;
    break;
  case Policy_first_fit:
    flp_size = 0;
    beyond = Val_NULL;
    policy = p;
    break;
  default:
    break;
  }
}
