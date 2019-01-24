/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Mark Shinwell and Leo White, Jane Street Europe             */
/*                                                                        */
/*   Copyright 2013--2016, Jane Street Group, LLC                         */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>

#include "caml/alloc.h"
#include "caml/config.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/intext.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/stack.h"
#include "caml/sys.h"
#include "caml/spacetime.h"

#include "caml/s.h"

#define SPACETIME_PROFINFO_WIDTH 26
#define Spacetime_profinfo_hd(hd) \
  (Gen_profinfo_hd(SPACETIME_PROFINFO_WIDTH, hd))

#ifdef ARCH_SIXTYFOUR

/* CR-someday lwhite: The following two definitions are copied from spacetime.c
   because they are needed here, but must be inlined in spacetime.c
   for performance. Perhaps a macro or "static inline" would be
   more appropriate. */

c_node* caml_spacetime_offline_c_node_of_stored_pointer_not_null
          (value node_stored)
{
  CAMLassert(Is_c_node(node_stored));
  return (c_node*) Hp_val(node_stored);
}

c_node_type caml_spacetime_offline_classify_c_node(c_node* node)
{
  return (node->pc & 2) ? CALL : ALLOCATION;
}

CAMLprim value caml_spacetime_compare_node(
      value node1, value node2)
{
  CAMLassert(!Is_in_value_area(node1));
  CAMLassert(!Is_in_value_area(node2));

  if (node1 == node2) {
    return Val_long(0);
  }
  if (node1 < node2) {
    return Val_long(-1);
  }
  return Val_long(1);
}

CAMLprim value caml_spacetime_unmarshal_trie (value v_channel)
{
  return caml_input_value_to_outside_heap(v_channel);
}

CAMLprim value caml_spacetime_node_num_header_words(value unit)
{
  unit = Val_unit;
  return Val_long(Node_num_header_words);
}

CAMLprim value caml_spacetime_is_ocaml_node(value node)
{
  CAMLassert(Is_ocaml_node(node) || Is_c_node(node));
  return Val_bool(Is_ocaml_node(node));
}

CAMLprim value caml_spacetime_ocaml_function_identifier(value node)
{
  CAMLassert(Is_ocaml_node(node));
  return caml_copy_int64((uint64_t) Decode_node_pc(Node_pc(node)));
}

CAMLprim value caml_spacetime_ocaml_tail_chain(value node)
{
  CAMLassert(Is_ocaml_node(node));
  return Tail_link(node);
}

CAMLprim value caml_spacetime_classify_direct_call_point
      (value node, value offset)
{
  uintnat field;
  value callee_node;

  CAMLassert(Is_ocaml_node(node));

  field = Long_val(offset);

  callee_node = Direct_callee_node(node, field);
  if (!Is_block(callee_node)) {
    /* An unused call point (may be a tail call point). */
    return Val_long(0);
  } else if (Is_ocaml_node(callee_node)) {
    return Val_long(1);  /* direct call point to OCaml code */
  } else {
    return Val_long(2);  /* direct call point to non-OCaml code */
  }
}

CAMLprim value caml_spacetime_ocaml_allocation_point_annotation
      (value node, value offset)
{
  uintnat profinfo_shifted;
  profinfo_shifted = (uintnat) Alloc_point_profinfo(node, Long_val(offset));
  return Val_long(Spacetime_profinfo_hd(profinfo_shifted));
}

CAMLprim value caml_spacetime_ocaml_allocation_point_count
      (value node, value offset)
{
  value count = Alloc_point_count(node, Long_val(offset));
  CAMLassert(!Is_block(count));
  return count;
}

CAMLprim value caml_spacetime_ocaml_direct_call_point_callee_node
      (value node, value offset)
{
  return Direct_callee_node(node, Long_val(offset));
}

CAMLprim value caml_spacetime_ocaml_direct_call_point_call_count
(value node, value offset)
{
  return Direct_call_count(node, Long_val(offset));
}

CAMLprim value caml_spacetime_ocaml_indirect_call_point_callees
      (value node, value offset)
{
  value callees = Indirect_pc_linked_list(node, Long_val(offset));
  CAMLassert(Is_block(callees));
  CAMLassert(Is_c_node(callees));
  return callees;
}

CAMLprim value caml_spacetime_c_node_is_call(value node)
{
  c_node* c_node;
  CAMLassert(node != (value) NULL);
  CAMLassert(Is_c_node(node));
  c_node = caml_spacetime_offline_c_node_of_stored_pointer_not_null(node);
  switch (caml_spacetime_offline_classify_c_node(c_node)) {
    case CALL: return Val_true;
    case ALLOCATION: return Val_false;
  }
  CAMLassert(0);
  return Val_unit;  /* silence compiler warning */
}

CAMLprim value caml_spacetime_c_node_next(value node)
{
  c_node* c_node;

  CAMLassert(node != (value) NULL);
  CAMLassert(Is_c_node(node));
  c_node = caml_spacetime_offline_c_node_of_stored_pointer_not_null(node);
  CAMLassert(c_node->next == Val_unit || Is_c_node(c_node->next));
  return c_node->next;
}

CAMLprim value caml_spacetime_c_node_call_site(value node)
{
  c_node* c_node;
  CAMLassert(node != (value) NULL);
  CAMLassert(Is_c_node(node));
  c_node = caml_spacetime_offline_c_node_of_stored_pointer_not_null(node);
  return caml_copy_int64((uint64_t) Decode_c_node_pc(c_node->pc));
}

CAMLprim value caml_spacetime_c_node_callee_node(value node)
{
  c_node* c_node;
  CAMLassert(node != (value) NULL);
  CAMLassert(Is_c_node(node));
  c_node = caml_spacetime_offline_c_node_of_stored_pointer_not_null(node);
  CAMLassert(caml_spacetime_offline_classify_c_node(c_node) == CALL);
  /* This might be an uninitialised tail call point: for example if an OCaml
     callee was indirectly called but the callee wasn't instrumented (e.g. a
     leaf function that doesn't allocate). */
  if (Is_tail_caller_node_encoded(c_node->data.call.callee_node)) {
    return Val_unit;
  }
  return c_node->data.call.callee_node;
}

CAMLprim value caml_spacetime_c_node_call_count(value node)
{
  c_node* c_node;
  CAMLassert(node != (value) NULL);
  CAMLassert(Is_c_node(node));
  c_node = caml_spacetime_offline_c_node_of_stored_pointer_not_null(node);
  CAMLassert(caml_spacetime_offline_classify_c_node(c_node) == CALL);
  if (Is_tail_caller_node_encoded(c_node->data.call.callee_node)) {
    return Val_long(0);
  }
  return c_node->data.call.call_count;
}

CAMLprim value caml_spacetime_c_node_profinfo(value node)
{
  c_node* c_node;
  CAMLassert(node != (value) NULL);
  CAMLassert(Is_c_node(node));
  c_node = caml_spacetime_offline_c_node_of_stored_pointer_not_null(node);
  CAMLassert(caml_spacetime_offline_classify_c_node(c_node) == ALLOCATION);
  CAMLassert(!Is_block(c_node->data.allocation.profinfo));
  return Val_long(Spacetime_profinfo_hd(c_node->data.allocation.profinfo));
}

CAMLprim value caml_spacetime_c_node_allocation_count(value node)
{
  c_node* c_node;
  CAMLassert(node != (value) NULL);
  CAMLassert(Is_c_node(node));
  c_node = caml_spacetime_offline_c_node_of_stored_pointer_not_null(node);
  CAMLassert(caml_spacetime_offline_classify_c_node(c_node) == ALLOCATION);
  CAMLassert(!Is_block(c_node->data.allocation.count));
  return c_node->data.allocation.count;
}

#endif
