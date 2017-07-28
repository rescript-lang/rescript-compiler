/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* For testing global root registration */

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/gc.h"

struct block { value header; value v; };

#define Block_val(v) ((struct block*) &((value*) v)[-1])
#define Val_block(b) ((value) &((b)->v))

value gb_get(value vblock)
{
  return Block_val(vblock)->v;
}

value gb_classic_register(value v)
{
  struct block * b = caml_stat_alloc(sizeof(struct block));
  b->header = Make_header(1, 0, Caml_black);
  b->v = v;
  caml_register_global_root(&(b->v));
  return Val_block(b);
}

value gb_classic_set(value vblock, value newval)
{
  Block_val(vblock)->v = newval;
  return Val_unit;
}

value gb_classic_remove(value vblock)
{
  caml_remove_global_root(&(Block_val(vblock)->v));
  return Val_unit;
}

value gb_generational_register(value v)
{
  struct block * b = caml_stat_alloc(sizeof(struct block));
  b->header = Make_header(1, 0, Caml_black);
  b->v = v;
  caml_register_generational_global_root(&(b->v));
  return Val_block(b);
}

value gb_generational_set(value vblock, value newval)
{
  caml_modify_generational_global_root(&(Block_val(vblock)->v), newval);
  return Val_unit;
}

value gb_generational_remove(value vblock)
{
  caml_remove_generational_global_root(&(Block_val(vblock)->v));
  return Val_unit;
}
