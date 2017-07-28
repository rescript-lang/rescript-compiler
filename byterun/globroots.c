/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Registration of global memory roots */

#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/globroots.h"

/* The sets of global memory roots are represented as skip lists
   (see William Pugh, "Skip lists: a probabilistic alternative to
   balanced binary trees", Comm. ACM 33(6), 1990). */

struct global_root {
  value * root;                    /* the address of the root */
  struct global_root * forward[1]; /* variable-length array */
};

#define NUM_LEVELS 17

struct global_root_list {
  value * root;                 /* dummy value for layout compatibility */
  struct global_root * forward[NUM_LEVELS]; /* forward chaining */
  int level;                    /* max used level */
};

/* Generate a random level for a new node: 0 with probability 3/4,
   1 with probability 3/16, 2 with probability 3/64, etc.
   We use a simple linear congruential PRNG (see Knuth vol 2) instead
   of random(), because we need exactly 32 bits of pseudo-random data
   (i.e. 2 * (NUM_LEVELS - 1)).  Moreover, the congruential PRNG
   is faster and guaranteed to be deterministic (to reproduce bugs). */

static uint32 random_seed = 0;

static int random_level(void)
{
  uint32 r;
  int level = 0;

  /* Linear congruence with modulus = 2^32, multiplier = 69069
     (Knuth vol 2 p. 106, line 15 of table 1), additive = 25173. */
  r = random_seed = random_seed * 69069 + 25173;
  /* Knuth (vol 2 p. 13) shows that the least significant bits are
     "less random" than the most significant bits with a modulus of 2^m,
     so consume most significant bits first */
  while ((r & 0xC0000000U) == 0xC0000000U) { level++; r = r << 2; }
  Assert(level < NUM_LEVELS);
  return level;
}

/* Insertion in a global root list */

static void caml_insert_global_root(struct global_root_list * rootlist,
                                    value * r)
{
  struct global_root * update[NUM_LEVELS];
  struct global_root * e, * f;
  int i, new_level;

  /* Init "cursor" to list head */
  e = (struct global_root *) rootlist;
  /* Find place to insert new node */
  for (i = rootlist->level; i >= 0; i--) {
    while (1) {
      f = e->forward[i];
      if (f == NULL || f->root >= r) break;
      e = f;
    }
    update[i] = e;
  }
  e = e->forward[0];
  /* If already present, don't do anything */
  if (e != NULL && e->root == r) return;
  /* Insert additional element, updating list level if necessary */
  new_level = random_level();
  if (new_level > rootlist->level) {
    for (i = rootlist->level + 1; i <= new_level; i++)
      update[i] = (struct global_root *) rootlist;
    rootlist->level = new_level;
  }
  e = caml_stat_alloc(sizeof(struct global_root) +
                      new_level * sizeof(struct global_root *));
  e->root = r;
  for (i = 0; i <= new_level; i++) {
    e->forward[i] = update[i]->forward[i];
    update[i]->forward[i] = e;
  }
}

/* Deletion in a global root list */

static void caml_delete_global_root(struct global_root_list * rootlist,
                                    value * r)
{
  struct global_root * update[NUM_LEVELS];
  struct global_root * e, * f;
  int i;

  /* Init "cursor" to list head */
  e = (struct global_root *) rootlist;
  /* Find element in list */
  for (i = rootlist->level; i >= 0; i--) {
    while (1) {
      f = e->forward[i];
      if (f == NULL || f->root >= r) break;
      e = f;
    }
    update[i] = e;
  }
  e = e->forward[0];
  /* If not found, nothing to do */
  if (e == NULL || e->root != r) return;
  /* Rebuild list without node */
  for (i = 0; i <= rootlist->level; i++) {
    if (update[i]->forward[i] == e)
      update[i]->forward[i] = e->forward[i];
  }
  /* Reclaim list element */
  caml_stat_free(e);
  /* Down-correct list level */
  while (rootlist->level > 0 &&
         rootlist->forward[rootlist->level] == NULL)
    rootlist->level--;
}

/* Iterate over a global root list */

static void caml_iterate_global_roots(scanning_action f,
                                      struct global_root_list * rootlist)
{
  struct global_root * gr;

  for (gr = rootlist->forward[0]; gr != NULL; gr = gr->forward[0]) {
    f(*(gr->root), gr->root);
  }
}

/* Empty a global root list */

static void caml_empty_global_roots(struct global_root_list * rootlist)
{
  struct global_root * gr, * next;
  int i;

  for (gr = rootlist->forward[0]; gr != NULL; /**/) {
    next = gr->forward[0];
    caml_stat_free(gr);
    gr = next;
  }
  for (i = 0; i <= rootlist->level; i++) rootlist->forward[i] = NULL;
  rootlist->level = 0;
}

/* The three global root lists */

struct global_root_list caml_global_roots = { NULL, { NULL, }, 0 };
                  /* mutable roots, don't know whether old or young */
struct global_root_list caml_global_roots_young = { NULL, { NULL, }, 0 };
                 /* generational roots pointing to minor or major heap */
struct global_root_list caml_global_roots_old = { NULL, { NULL, }, 0 };
                  /* generational roots pointing to major heap */

/* Register a global C root of the mutable kind */

CAMLexport void caml_register_global_root(value *r)
{
  Assert (((intnat) r & 3) == 0);  /* compact.c demands this (for now) */
  caml_insert_global_root(&caml_global_roots, r);
}

/* Un-register a global C root of the mutable kind */

CAMLexport void caml_remove_global_root(value *r)
{
  caml_delete_global_root(&caml_global_roots, r);
}

/* Register a global C root of the generational kind */

CAMLexport void caml_register_generational_global_root(value *r)
{
  value v = *r;
  Assert (((intnat) r & 3) == 0);  /* compact.c demands this (for now) */
  if (Is_block(v)) {
    if (Is_young(v))
      caml_insert_global_root(&caml_global_roots_young, r);
    else if (Is_in_heap(v))
      caml_insert_global_root(&caml_global_roots_old, r);
  }
}

/* Un-register a global C root of the generational kind */

CAMLexport void caml_remove_generational_global_root(value *r)
{
  value v = *r;
  if (Is_block(v)) {
    if (Is_young(v))
      caml_delete_global_root(&caml_global_roots_young, r);
    else if (Is_in_heap(v))
      caml_delete_global_root(&caml_global_roots_old, r);
  }
}

/* Modify the value of a global C root of the generational kind */

CAMLexport void caml_modify_generational_global_root(value *r, value newval)
{
  value oldval = *r;

  /* It is OK to have a root in roots_young that suddenly points to
     the old generation -- the next minor GC will take care of that.
     What needs corrective action is a root in roots_old that suddenly
     points to the young generation. */
  if (Is_block(newval) && Is_young(newval) &&
      Is_block(oldval) && Is_in_heap(oldval)) {
    caml_delete_global_root(&caml_global_roots_old, r);
    caml_insert_global_root(&caml_global_roots_young, r);
  }
  /* PR#4704 */
  else if (!Is_block(oldval) && Is_block(newval)) {
    /* The previous value in the root was unboxed but now it is boxed.
       The root won't appear in any of the root lists thus far (by virtue
       of the operation of [caml_register_generational_global_root]), so we
       need to make sure it gets in, or else it will never be scanned. */
    if (Is_young(newval))
      caml_insert_global_root(&caml_global_roots_young, r);
    else if (Is_in_heap(newval))
      caml_insert_global_root(&caml_global_roots_old, r);
  }
  else if (Is_block(oldval) && !Is_block(newval)) {
    /* The previous value in the root was boxed but now it is unboxed, so
       the root should be removed. If [oldval] is young, this will happen
       anyway at the next minor collection, but it is safer to delete it
       here. */
    if (Is_young(oldval))
      caml_delete_global_root(&caml_global_roots_young, r);
    else if (Is_in_heap(oldval))
      caml_delete_global_root(&caml_global_roots_old, r);
  }
  /* end PR#4704 */
  *r = newval;
}

/* Scan all global roots */

void caml_scan_global_roots(scanning_action f)
{
  caml_iterate_global_roots(f, &caml_global_roots);
  caml_iterate_global_roots(f, &caml_global_roots_young);
  caml_iterate_global_roots(f, &caml_global_roots_old);
}

/* Scan global roots for a minor collection */

void caml_scan_global_young_roots(scanning_action f)
{
  struct global_root * gr;

  caml_iterate_global_roots(f, &caml_global_roots);
  caml_iterate_global_roots(f, &caml_global_roots_young);
  /* Move young roots to old roots */
  for (gr = caml_global_roots_young.forward[0];
       gr != NULL; gr = gr->forward[0]) {
    caml_insert_global_root(&caml_global_roots_old, gr->root);
  }
  caml_empty_global_roots(&caml_global_roots_young);
}
