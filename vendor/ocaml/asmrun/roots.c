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

/* To walk the memory roots for garbage collection */

#include "caml/finalise.h"
#include "caml/globroots.h"
#include "caml/memory.h"
#include "caml/major_gc.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/stack.h"
#include "caml/roots.h"
#include <string.h>
#include <stdio.h>

/* Roots registered from C functions */

struct caml__roots_block *caml_local_roots = NULL;

void (*caml_scan_roots_hook) (scanning_action) = NULL;

/* The hashtable of frame descriptors */
frame_descr ** caml_frame_descriptors = NULL;
int caml_frame_descriptors_mask = 0;

/* Linked-list */

typedef struct link {
  void *data;
  struct link *next;
} link;

static link *cons(void *data, link *tl) {
  link *lnk = caml_stat_alloc(sizeof(link));
  lnk->data = data;
  lnk->next = tl;
  return lnk;
}

#define iter_list(list,lnk) \
  for (lnk = list; lnk != NULL; lnk = lnk->next)

/* Linked-list of frametables */

static link *frametables = NULL;
static intnat num_descr = 0;

static int count_descriptors(link *list) {
  intnat num_descr = 0;
  link *lnk;
  iter_list(list,lnk) {
    num_descr += *((intnat*) lnk->data);
  }
  return num_descr;
}

static link* frametables_list_tail(link *list) {
  link *lnk, *tail = NULL;
  iter_list(list,lnk) {
    tail = lnk;
  }
  return tail;
}

static frame_descr * next_frame_descr(frame_descr * d) {
  uintnat nextd;
  nextd =
    ((uintnat)d +
     sizeof(char *) + sizeof(short) + sizeof(short) +
     sizeof(short) * d->num_live + sizeof(frame_descr *) - 1)
    & -sizeof(frame_descr *);
  if (d->frame_size & 1) nextd += sizeof(void *); /* pointer to debuginfo */
  return((frame_descr *) nextd);
}

static void fill_hashtable(link *frametables) {
  intnat len, j;
  intnat * tbl;
  frame_descr * d;
  uintnat h;
  link *lnk = NULL;

  iter_list(frametables,lnk) {
    tbl = (intnat*) lnk->data;
    len = *tbl;
    d = (frame_descr *)(tbl + 1);
    for (j = 0; j < len; j++) {
      h = Hash_retaddr(d->retaddr);
      while (caml_frame_descriptors[h] != NULL) {
        h = (h+1) & caml_frame_descriptors_mask;
      }
      caml_frame_descriptors[h] = d;
      d = next_frame_descr(d);
    }
  }
}

static void init_frame_descriptors(link *new_frametables)
{
  intnat tblsize, increase, i;
  link *tail = NULL;

  CAMLassert(new_frametables);

  tail = frametables_list_tail(new_frametables);
  increase = count_descriptors(new_frametables);
  tblsize = caml_frame_descriptors_mask + 1;

  /* Reallocate the caml_frame_descriptor table if it is too small */
  if(tblsize < (num_descr + increase) * 2) {

    /* Merge both lists */
    tail->next = frametables;
    frametables = NULL;

    /* [num_descr] can be less than [num_descr + increase] if frame
       tables where unregistered */
    num_descr = count_descriptors(new_frametables);

    tblsize = 4;
    while (tblsize < 2 * num_descr) tblsize *= 2;

    caml_frame_descriptors_mask = tblsize - 1;
    if(caml_frame_descriptors) caml_stat_free(caml_frame_descriptors);
    caml_frame_descriptors =
      (frame_descr **) caml_stat_alloc(tblsize * sizeof(frame_descr *));
    for (i = 0; i < tblsize; i++) caml_frame_descriptors[i] = NULL;

    fill_hashtable(new_frametables);
  } else {
    num_descr += increase;
    fill_hashtable(new_frametables);
    tail->next = frametables;
  }

  frametables = new_frametables;
}

void caml_init_frame_descriptors(void) {
  intnat i;
  link *new_frametables = NULL;
  for (i = 0; caml_frametable[i] != 0; i++)
    new_frametables = cons(caml_frametable[i],new_frametables);
  init_frame_descriptors(new_frametables);
}

void caml_register_frametable(intnat *table) {
  link *new_frametables = cons(table,NULL);
  init_frame_descriptors(new_frametables);
}

static void remove_entry(frame_descr * d) {
  uintnat i;
  uintnat r;
  uintnat j;

  i = Hash_retaddr(d->retaddr);
  while (caml_frame_descriptors[i] != d) {
    i = (i+1) & caml_frame_descriptors_mask;
  }

 r1:
  j = i;
  caml_frame_descriptors[i] = NULL;
 r2:
  i = (i+1) & caml_frame_descriptors_mask;
  // r3
  if(caml_frame_descriptors[i] == NULL) return;
  r = Hash_retaddr(caml_frame_descriptors[i]->retaddr);
  /* If r is between i and j (cyclically), i.e. if
     caml_frame_descriptors[i]->retaddr don't need to be moved */
  if(( ( j < r )  && ( r <= i ) ) ||
     ( ( i < j )  && ( j < r )  ) ||      /* i cycled, r not */
     ( ( r <= i ) && ( i < j ) )     ) {  /* i and r cycled */
    goto r2;
  }
  // r4
  caml_frame_descriptors[j] = caml_frame_descriptors[i];
  goto r1;
}

void caml_unregister_frametable(intnat *table) {
  intnat len, j;
  link *lnk;
  link *previous = frametables;
  frame_descr * d;

  len = *table;
  d = (frame_descr *)(table + 1);
  for (j = 0; j < len; j++) {
    remove_entry(d);
    d = next_frame_descr(d);
  }

  iter_list(frametables,lnk) {
    if(lnk->data == table) {
      previous->next = lnk->next;
      caml_stat_free(lnk);
      break;
    }
    previous = lnk;
  }
}

/* Communication with [caml_start_program] and [caml_call_gc]. */

char * caml_top_of_stack;
char * caml_bottom_of_stack = NULL; /* no stack initially */
uintnat caml_last_return_address = 1; /* not in OCaml code initially */
value * caml_gc_regs;
intnat caml_globals_inited = 0;
static intnat caml_globals_scanned = 0;
static link * caml_dyn_globals = NULL;

void caml_register_dyn_global(void *v) {
  caml_dyn_globals = cons((void*) v,caml_dyn_globals);
}

/* Call [caml_oldify_one] on (at least) all the roots that point to the minor
   heap. */
void caml_oldify_local_roots (void)
{
  char * sp;
  uintnat retaddr;
  value * regs;
  frame_descr * d;
  uintnat h;
  int i, j, n, ofs;
#ifdef Stack_grows_upwards
  short * p;  /* PR#4339: stack offsets are negative in this case */
#else
  unsigned short * p;
#endif
  value * glob;
  value * root;
  struct caml__roots_block *lr;
  link *lnk;

  /* The global roots */
  for (i = caml_globals_scanned;
       i <= caml_globals_inited && caml_globals[i] != 0;
       i++) {
    for(glob = caml_globals[i]; *glob != 0; glob++) {
      for (j = 0; j < Wosize_val(*glob); j++){
        Oldify (&Field (*glob, j));
      }
    }
  }
  caml_globals_scanned = caml_globals_inited;

  /* Dynamic global roots */
  iter_list(caml_dyn_globals, lnk) {
    for(glob = (value *) lnk->data; *glob != 0; glob++) {
      for (j = 0; j < Wosize_val(*glob); j++){
        Oldify (&Field (*glob, j));
      }
    }
  }

  /* The stack and local roots */
  sp = caml_bottom_of_stack;
  retaddr = caml_last_return_address;
  regs = caml_gc_regs;
  if (sp != NULL) {
    while (1) {
      /* Find the descriptor corresponding to the return address */
      h = Hash_retaddr(retaddr);
      while(1) {
        d = caml_frame_descriptors[h];
        if (d->retaddr == retaddr) break;
        h = (h+1) & caml_frame_descriptors_mask;
      }
      if (d->frame_size != 0xFFFF) {
        /* Scan the roots in this frame */
        for (p = d->live_ofs, n = d->num_live; n > 0; n--, p++) {
          ofs = *p;
          if (ofs & 1) {
            root = regs + (ofs >> 1);
          } else {
            root = (value *)(sp + ofs);
          }
          Oldify (root);
        }
        /* Move to next frame */
#ifndef Stack_grows_upwards
        sp += (d->frame_size & 0xFFFC);
#else
        sp -= (d->frame_size & 0xFFFC);
#endif
        retaddr = Saved_return_address(sp);
#ifdef Already_scanned
        /* Stop here if the frame has been scanned during earlier GCs  */
        if (Already_scanned(sp, retaddr)) break;
        /* Mark frame as already scanned */
        Mark_scanned(sp, retaddr);
#endif
      } else {
        /* This marks the top of a stack chunk for an ML callback.
           Skip C portion of stack and continue with next ML stack chunk. */
        struct caml_context * next_context = Callback_link(sp);
        sp = next_context->bottom_of_stack;
        retaddr = next_context->last_retaddr;
        regs = next_context->gc_regs;
        /* A null sp means no more ML stack chunks; stop here. */
        if (sp == NULL) break;
      }
    }
  }
  /* Local C roots */
  for (lr = caml_local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        root = &(lr->tables[i][j]);
        Oldify (root);
      }
    }
  }
  /* Global C roots */
  caml_scan_global_young_roots(&caml_oldify_one);
  /* Finalised values */
  caml_final_oldify_young_roots ();
  /* Hook */
  if (caml_scan_roots_hook != NULL) (*caml_scan_roots_hook)(&caml_oldify_one);
}

uintnat caml_incremental_roots_count = 0;

/* Call [caml_darken] on all roots, incrementally:
   [caml_darken_all_roots_start] does the non-incremental part and
   sets things up for [caml_darken_all_roots_slice].
*/
void caml_darken_all_roots_start (void)
{
  caml_do_roots (caml_darken, 0);
}

/* Call [caml_darken] on at most [work] global roots. Return the
   amount of work not done, if any. If this is strictly positive,
   the darkening is done.
 */
intnat caml_darken_all_roots_slice (intnat work)
{
  static int i, j;
  static value *glob;
  static int do_resume = 0;
  static mlsize_t roots_count = 0;
  intnat remaining_work = work;
  CAML_INSTR_SETUP (tmr, "");

  /* If the loop was started in a previous call, resume it. */
  if (do_resume) goto resume;

  /* This is the same loop as in [caml_do_roots], but we make it
     suspend itself when [work] reaches 0. */
  for (i = 0; caml_globals[i] != 0; i++) {
    for(glob = caml_globals[i]; *glob != 0; glob++) {
      for (j = 0; j < Wosize_val(*glob); j++){
        caml_darken (Field (*glob, j), &Field (*glob, j));
        -- remaining_work;
        if (remaining_work == 0){
          roots_count += work;
          do_resume = 1;
          goto suspend;
        }
      resume: ;
      }
    }
  }

  /* The loop finished normally, so all roots are now darkened. */
  caml_incremental_roots_count = roots_count + work - remaining_work;
  /* Prepare for the next run. */
  do_resume = 0;
  roots_count = 0;

 suspend:
  /* Do this in both cases. */
  CAML_INSTR_TIME (tmr, "major/mark/global_roots_slice");
  return remaining_work;
}

void caml_do_roots (scanning_action f, int do_globals)
{
  int i, j;
  value * glob;
  link *lnk;
  CAML_INSTR_SETUP (tmr, "major_roots");

  if (do_globals){
    /* The global roots */
    for (i = 0; caml_globals[i] != 0; i++) {
      for(glob = caml_globals[i]; *glob != 0; glob++) {
        for (j = 0; j < Wosize_val(*glob); j++)
          f (Field (*glob, j), &Field (*glob, j));
      }
    }
  }
  /* Dynamic global roots */
  iter_list(caml_dyn_globals, lnk) {
    for(glob = (value *) lnk->data; *glob != 0; glob++) {
      for (j = 0; j < Wosize_val(*glob); j++){
        f (Field (*glob, j), &Field (*glob, j));
      }
    }
  }
  CAML_INSTR_TIME (tmr, "major_roots/dynamic_global");
  /* The stack and local roots */
  caml_do_local_roots(f, caml_bottom_of_stack, caml_last_return_address,
                      caml_gc_regs, caml_local_roots);
  CAML_INSTR_TIME (tmr, "major_roots/local");
  /* Global C roots */
  caml_scan_global_roots(f);
  CAML_INSTR_TIME (tmr, "major_roots/C");
  /* Finalised values */
  caml_final_do_roots (f);
  CAML_INSTR_TIME (tmr, "major_roots/finalised");
  /* Hook */
  if (caml_scan_roots_hook != NULL) (*caml_scan_roots_hook)(f);
  CAML_INSTR_TIME (tmr, "major_roots/hook");
}

void caml_do_local_roots(scanning_action f, char * bottom_of_stack,
                         uintnat last_retaddr, value * gc_regs,
                         struct caml__roots_block * local_roots)
{
  char * sp;
  uintnat retaddr;
  value * regs;
  frame_descr * d;
  uintnat h;
  int i, j, n, ofs;
#ifdef Stack_grows_upwards
  short * p;  /* PR#4339: stack offsets are negative in this case */
#else
  unsigned short * p;
#endif
  value * root;
  struct caml__roots_block *lr;

  sp = bottom_of_stack;
  retaddr = last_retaddr;
  regs = gc_regs;
  if (sp != NULL) {
    while (1) {
      /* Find the descriptor corresponding to the return address */
      h = Hash_retaddr(retaddr);
      while(1) {
        d = caml_frame_descriptors[h];
        if (d->retaddr == retaddr) break;
        h = (h+1) & caml_frame_descriptors_mask;
      }
      if (d->frame_size != 0xFFFF) {
        /* Scan the roots in this frame */
        for (p = d->live_ofs, n = d->num_live; n > 0; n--, p++) {
          ofs = *p;
          if (ofs & 1) {
            root = regs + (ofs >> 1);
          } else {
            root = (value *)(sp + ofs);
          }
          f (*root, root);
        }
        /* Move to next frame */
#ifndef Stack_grows_upwards
        sp += (d->frame_size & 0xFFFC);
#else
        sp -= (d->frame_size & 0xFFFC);
#endif
        retaddr = Saved_return_address(sp);
#ifdef Mask_already_scanned
        retaddr = Mask_already_scanned(retaddr);
#endif
      } else {
        /* This marks the top of a stack chunk for an ML callback.
           Skip C portion of stack and continue with next ML stack chunk. */
        struct caml_context * next_context = Callback_link(sp);
        sp = next_context->bottom_of_stack;
        retaddr = next_context->last_retaddr;
        regs = next_context->gc_regs;
        /* A null sp means no more ML stack chunks; stop here. */
        if (sp == NULL) break;
      }
    }
  }
  /* Local C roots */
  for (lr = local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        root = &(lr->tables[i][j]);
        f (*root, root);
      }
    }
  }
}

uintnat (*caml_stack_usage_hook)(void) = NULL;

uintnat caml_stack_usage (void)
{
  uintnat sz;
  sz = (value *) caml_top_of_stack - (value *) caml_bottom_of_stack;
  if (caml_stack_usage_hook != NULL)
    sz += (*caml_stack_usage_hook)();
  return sz;
}
