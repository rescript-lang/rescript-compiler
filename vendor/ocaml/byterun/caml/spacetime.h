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

#ifndef CAML_SPACETIME_H
#define CAML_SPACETIME_H

#ifdef NATIVE_CODE

#include "caml/io.h"
#include "caml/misc.h"
#include "caml/stack.h"

/* Runtime support for Spacetime profiling.
 * This header file is not intended for the casual user.
 *
 * The implementation is split into three files:
 *   1. spacetime.c: core management of the instrumentation;
 *   2. spacetime_snapshot.c: the taking of heap snapshots;
 *   3. spacetime_offline.c: functions that are also used when examining
 *      saved profiling data.
 */

typedef enum {
  CALL,
  ALLOCATION
} c_node_type;

/* All pointers between nodes point at the word immediately after the
   GC headers, and everything is traversable using the normal OCaml rules.

   On entry to an OCaml function:
   If the node hole pointer register has the bottom bit set, then the function
   is being tail called or called from a self-recursive call site:
   - If the node hole is empty, the callee must create a new node and link
     it into the tail chain.  The node hole pointer will point at the tail
     chain.
   - Otherwise the node should be used as normal.
   Otherwise (not a tail call):
   - If the node hole is empty, the callee must create a new node, but the
     tail chain is untouched.
   - Otherwise the node should be used as normal.
*/

/* Classification of nodes (OCaml or C) with corresponding GC tags. */
#define OCaml_node_tag 0
#define C_node_tag 1
#define Is_ocaml_node(node) (Is_block(node) && Tag_val(node) == OCaml_node_tag)
#define Is_c_node(node) (Is_block(node) && Tag_val(node) == C_node_tag)

/* The header words are:
   1. The node program counter.
   2. The tail link. */
#define Node_num_header_words 2

/* The "node program counter" at the start of an OCaml node. */
#define Node_pc(node) (Field(node, 0))
#define Encode_node_pc(pc) (((value) pc) | 1)
#define Decode_node_pc(encoded_pc) ((void*) (encoded_pc & ~1))

/* The circular linked list of tail-called functions within OCaml nodes. */
#define Tail_link(node) (Field(node, 1))

/* The convention for pointers from OCaml nodes to other nodes.  There are
   two special cases:
   1. [Val_unit] means "uninitialized", and further, that this is not a
      tail call point.  (Tail call points are pre-initialized, as in case 2.)
   2. If the bottom bit is set, and the value is not [Val_unit], this is a
      tail call point. */
#define Encode_tail_caller_node(node) ((node) | 1)
#define Decode_tail_caller_node(node) ((node) & ~1)
#define Is_tail_caller_node_encoded(node) (((node) & 1) == 1)

/* Allocation points within OCaml nodes.
   The "profinfo" value looks exactly like a black Infix_tag header.
   This enables us to point just after it and return such pointer as a valid
   OCaml value.  (Used for the list of all allocation points.  We could do
   without this and instead just encode the list pointers as integers, but
   this would mean that the structure was destroyed on marshalling.  This
   might not be a great problem since it is intended that the total counts
   be obtained via snapshots, but it seems neater and easier to use
   Infix_tag.
   The "count" is just an OCaml integer giving the total number of words
   (including headers) allocated at the point.
   The "pointer to next allocation point" points to the "count" word of the
   next allocation point in the linked list of all allocation points.
   There is no special encoding needed by virtue of the [Infix_tag] trick. */
#define Alloc_point_profinfo(node, offset) (Field(node, offset))
#define Alloc_point_count(node, offset) (Field(node, offset + 1))
#define Alloc_point_next_ptr(node, offset) (Field(node, offset + 2))

/* Direct call points (tail or non-tail) within OCaml nodes.
   They hold a pointer to the child node and (if the compiler was so
   configured) a call count.
   The call site and callee are both recorded in the shape. */
#define Direct_callee_node(node,offset) (Field(node, offset))
#define Direct_call_count(node,offset) (Field(node, offset + 1))
#define Encode_call_point_pc(pc) (((value) pc) | 1)
#define Decode_call_point_pc(pc) ((void*) (((value) pc) & ~((uintnat) 1)))

/* Indirect call points (tail or non-tail) within OCaml nodes.
   They hold a linked list of (PC upon entry to the callee, pointer to
   child node) pairs.  The linked list is encoded using C nodes and should
   be thought of as part of the OCaml node itself. */
#define Indirect_num_fields 1
#define Indirect_pc_linked_list(node,offset) (Field(node, offset))

/* Encodings of the program counter value within a C node. */
#define Encode_c_node_pc_for_call(pc) ((((value) pc) << 2) | 3)
#define Encode_c_node_pc_for_alloc_point(pc) ((((value) pc) << 2) | 1)
#define Decode_c_node_pc(pc) ((void*) (((uintnat) (pc)) >> 2))

typedef struct {
  /* The layout and encoding of this structure must match that of the
     allocation points within OCaml nodes, so that the linked list
     traversal across all allocation points works correctly. */
  value profinfo;  /* encoded using [Infix_tag] (see above) */
  value count;
  /* [next] is [Val_unit] for the end of the list.
     Otherwise it points at the second word of this [allocation_point]
     structure. */
  value next;
} allocation_point;

typedef struct {
  value callee_node;
  value call_count;
} call_point;

typedef struct {
  /* CR-soon mshinwell: delete [gc_header], all the offset arithmetic will
     then go away */
  uintnat gc_header;
  uintnat pc;           /* see above for encodings */
  union {
    call_point call;  /* for CALL */
    allocation_point allocation;  /* for ALLOCATION */
  } data;
  value next;           /* [Val_unit] for the end of the list */
} c_node; /* CR-soon mshinwell: rename to dynamic_node */

typedef struct shape_table {
  uint64_t* table;
  struct shape_table* next;
} shape_table;

extern uint64_t** caml_spacetime_static_shape_tables;
extern shape_table* caml_spacetime_dynamic_shape_tables;

typedef struct ext_table* spacetime_unwind_info_cache;

extern value caml_spacetime_trie_root;
extern value* caml_spacetime_trie_node_ptr;
extern value* caml_spacetime_finaliser_trie_root;

extern allocation_point* caml_all_allocation_points;

extern void caml_spacetime_initialize(void);
extern uintnat caml_spacetime_my_profinfo(
  spacetime_unwind_info_cache*, uintnat);
extern c_node_type caml_spacetime_classify_c_node(c_node* node);
extern c_node* caml_spacetime_c_node_of_stored_pointer(value);
extern c_node* caml_spacetime_c_node_of_stored_pointer_not_null(value);
extern value caml_spacetime_stored_pointer_of_c_node(c_node* node);
extern void caml_spacetime_register_thread(value*, value*);
extern void caml_spacetime_register_shapes(void*);
extern value caml_spacetime_frame_table(void);
extern value caml_spacetime_shape_table(void);
extern void caml_spacetime_save_snapshot (struct channel *chan,
                                          double time_override,
                                          int use_time_override);
extern value caml_spacetime_timestamp(double time_override,
                                      int use_time_override);
extern void caml_spacetime_automatic_snapshot (void);

/* For use in runtime functions that are executed from OCaml
   code, to save the overhead of using libunwind every time. */
#ifdef WITH_SPACETIME
#define Get_my_profinfo_with_cached_backtrace(profinfo, size) \
  do { \
    static spacetime_unwind_info_cache spacetime_unwind_info = NULL; \
    profinfo = caml_spacetime_my_profinfo(&spacetime_unwind_info, size); \
  } \
  while (0);
#else
#define Get_my_profinfo_with_cached_backtrace(profinfo, size) \
  profinfo = (uintnat) 0;
#endif

#else

#define Get_my_profinfo_with_cached_backtrace(profinfo, size)   \
  profinfo = (uintnat) 0;

#endif /* NATIVE_CODE */


#endif
