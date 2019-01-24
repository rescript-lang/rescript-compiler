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

/* Operations on objects */

#include <string.h>
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/interp.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/prims.h"
#include "caml/spacetime.h"

/* [size] is a value encoding a number of bytes */
CAMLprim value caml_static_alloc(value size)
{
  return (value) caml_stat_alloc((asize_t) Long_val(size));
}

CAMLprim value caml_static_free(value blk)
{
  caml_stat_free((void *) blk);
  return Val_unit;
}

CAMLprim value caml_static_resize(value blk, value new_size)
{
  return (value) caml_stat_resize((char *) blk, (asize_t) Long_val(new_size));
}

/* unused since GPR#427 */
CAMLprim value caml_obj_is_block(value arg)
{
  return Val_bool(Is_block(arg));
}

CAMLprim value caml_obj_tag(value arg)
{
  if (Is_long (arg)){
    return Val_int (1000);   /* int_tag */
  }else if ((long) arg & (sizeof (value) - 1)){
    return Val_int (1002);   /* unaligned_tag */
  }else if (Is_in_value_area (arg)){
    return Val_int(Tag_val(arg));
  }else{
    return Val_int (1001);   /* out_of_heap_tag */
  }
}

CAMLprim value caml_obj_set_tag (value arg, value new_tag)
{
  Tag_val (arg) = Int_val (new_tag);
  return Val_unit;
}

/* [size] is a value encoding a number of blocks */
CAMLprim value caml_obj_block(value tag, value size)
{
  value res;
  mlsize_t sz, i;
  tag_t tg;

  sz = Long_val(size);
  tg = Long_val(tag);
  if (sz == 0) return Atom(tg);
  res = caml_alloc(sz, tg);
  for (i = 0; i < sz; i++)
    Field(res, i) = Val_long(0);

  return res;
}

/* Spacetime profiling assumes that this function is only called from OCaml. */
CAMLprim value caml_obj_dup(value arg)
{
  CAMLparam1 (arg);
  CAMLlocal1 (res);
  mlsize_t sz, i;
  tag_t tg;

  sz = Wosize_val(arg);
  if (sz == 0) CAMLreturn (arg);
  tg = Tag_val(arg);
  if (tg >= No_scan_tag) {
    res = caml_alloc(sz, tg);
    memcpy(Bp_val(res), Bp_val(arg), sz * sizeof(value));
  } else if (sz <= Max_young_wosize) {
    uintnat profinfo;
    Get_my_profinfo_with_cached_backtrace(profinfo, sz);
    res = caml_alloc_small_with_my_or_given_profinfo(sz, tg, profinfo);
    for (i = 0; i < sz; i++) Field(res, i) = Field(arg, i);
  } else {
    res = caml_alloc_shr(sz, tg);
    for (i = 0; i < sz; i++) caml_initialize(&Field(res, i), Field(arg, i));
  }
  CAMLreturn (res);
}

/* Shorten the given block to the given size and return void.
   Raise Invalid_argument if the given size is less than or equal
   to 0 or greater than the current size.

   algorithm:
   Change the length field of the header.  Make up a black object
   with the leftover part of the object: this is needed in the major
   heap and harmless in the minor heap. The object cannot be white
   because there may still be references to it in the ref table. By
   using a black object we ensure that the ref table will be emptied
   before the block is reallocated (since there must be a minor
   collection within each major cycle).

   [newsize] is a value encoding a number of fields (words, except
   for float arrays on 32-bit architectures).
*/
CAMLprim value caml_obj_truncate (value v, value newsize)
{
  mlsize_t new_wosize = Long_val (newsize);
  header_t hd = Hd_val (v);
  tag_t tag = Tag_hd (hd);
  color_t color = Color_hd (hd);
  mlsize_t wosize = Wosize_hd (hd);
  mlsize_t i;

  if (tag == Double_array_tag) new_wosize *= Double_wosize;  /* PR#156 */

  if (new_wosize <= 0 || new_wosize > wosize){
    caml_invalid_argument ("Obj.truncate");
  }
  if (new_wosize == wosize) return Val_unit;
  /* PR#61: since we're about to lose our references to the elements
     beyond new_wosize in v, erase them explicitly so that the GC
     can darken them as appropriate. */
  if (tag < No_scan_tag) {
    for (i = new_wosize; i < wosize; i++){
      caml_modify(&Field(v, i), Val_unit);
#ifdef DEBUG
      Field (v, i) = Debug_free_truncate;
#endif
    }
  }
  /* We must use an odd tag for the header of the leftovers so it does not
     look like a pointer because there may be some references to it in
     ref_table. */
  Field (v, new_wosize) =
    Make_header (Wosize_whsize (wosize-new_wosize), Abstract_tag, Caml_black);
  Hd_val (v) =
    Make_header_with_profinfo (new_wosize, tag, color, Profinfo_val(v));
  return Val_unit;
}

CAMLprim value caml_obj_add_offset (value v, value offset)
{
  return v + (unsigned long) Int32_val (offset);
}

/* The following functions are used in stdlib/lazy.ml.
   They are not written in OCaml because they must be atomic with respect
   to the GC.
 */

CAMLprim value caml_lazy_follow_forward (value v)
{
  if (Is_block (v) && Is_in_value_area(v)
      && Tag_val (v) == Forward_tag){
    return Forward_val (v);
  }else{
    return v;
  }
}

CAMLprim value caml_lazy_make_forward (value v)
{
  CAMLparam1 (v);
  CAMLlocal1 (res);

  res = caml_alloc_small (1, Forward_tag);
  Field (res, 0) = v;
  CAMLreturn (res);
}

/* For mlvalues.h and camlinternalOO.ml
   See also GETPUBMET in interp.c
 */

CAMLprim value caml_get_public_method (value obj, value tag)
{
  value meths = Field (obj, 0);
  int li = 3, hi = Field(meths,0), mi;
  while (li < hi) {
    mi = ((li+hi) >> 1) | 1;
    if (tag < Field(meths,mi)) hi = mi-2;
    else li = mi;
  }
  /* return 0 if tag is not there */
  return (tag == Field(meths,li) ? Field (meths, li-1) : 0);
}

/* these two functions might be useful to an hypothetical JIT */

#ifdef CAML_JIT
#ifdef NATIVE_CODE
#define MARK 1
#else
#define MARK 0
#endif
value caml_cache_public_method (value meths, value tag, value *cache)
{
  int li = 3, hi = Field(meths,0), mi;
  while (li < hi) {
    mi = ((li+hi) >> 1) | 1;
    if (tag < Field(meths,mi)) hi = mi-2;
    else li = mi;
  }
  *cache = (li-3)*sizeof(value) + MARK;
  return Field (meths, li-1);
}

value caml_cache_public_method2 (value *meths, value tag, value *cache)
{
  value ofs = *cache & meths[1];
  if (*(value*)(((char*)(meths+3)) + ofs - MARK) == tag)
    return *(value*)(((char*)(meths+2)) + ofs - MARK);
  {
    int li = 3, hi = meths[0], mi;
    while (li < hi) {
      mi = ((li+hi) >> 1) | 1;
      if (tag < meths[mi]) hi = mi-2;
      else li = mi;
    }
    *cache = (li-3)*sizeof(value) + MARK;
    return meths[li-1];
  }
}
#endif /*CAML_JIT*/

static value oo_last_id = Val_int(0);

CAMLprim value caml_set_oo_id (value obj) {
  Field(obj, 1) = oo_last_id;
  oo_last_id += 2;
  return obj;
}

CAMLprim value caml_fresh_oo_id (value v) {
  v = oo_last_id;
  oo_last_id += 2;
  return v;
}

CAMLprim value caml_int_as_pointer (value n) {
  return n - 1;
}

/* Compute how many words in the heap are occupied by blocks accessible
   from a given value */

#define ENTRIES_PER_QUEUE_CHUNK 4096
struct queue_chunk {
  struct queue_chunk *next;
  value entries[ENTRIES_PER_QUEUE_CHUNK];
};


CAMLprim value caml_obj_reachable_words(value v)
{
  static struct queue_chunk first_chunk;
  struct queue_chunk *read_chunk, *write_chunk;
  int write_pos, read_pos, i;

  intnat size = 0;
  header_t hd;
  mlsize_t sz;

  if (Is_long(v) || !Is_in_heap_or_young(v)) return Val_int(0);
  if (Tag_hd(Hd_val(v)) == Infix_tag) v -= Infix_offset_hd(Hd_val(v));
  hd = Hd_val(v);
  sz = Wosize_hd(hd);

  read_chunk = write_chunk = &first_chunk;
  read_pos = 0;
  write_pos = 1;
  write_chunk->entries[0] = v | Colornum_hd(hd);
  Hd_val(v) = Bluehd_hd(hd);

  /* We maintain a queue of "interesting" blocks that have been seen.
     An interesting block is a block in the heap which does not
     represent an infix pointer. Infix pointers are normalized to the
     beginning of their block.  Blocks in the static data area are excluded.

     The function maintains a queue of block pointers.  Concretely,
     the queue is stored as a linked list of chunks, each chunk
     holding a number of pointers to interesting blocks.  Initially,
     it contains only the "root" value.  The first chunk of the queue
     is allocated statically.  More chunks can be allocated as needed
     and released before this function exits.

     When a block is inserted in the queue, it is marked as blue.
     This mark is used to avoid a second visit of the same block.
     The real color is stored in the last 2 bits of the pointer in the
     queue.  (Same technique as in extern.c.)

     Note: we make the assumption that there is no pointer
     from the static data area to the heap.
  */

  /* First pass: mark accessible blocks and compute their total size */
  while (read_pos != write_pos || read_chunk != write_chunk) {
    /* Pop the next element from the queue */
    if (read_pos == ENTRIES_PER_QUEUE_CHUNK) {
      read_pos = 0;
      read_chunk = read_chunk->next;
    }
    v = read_chunk->entries[read_pos++] & ~3;

    hd = Hd_val(v);
    sz = Wosize_hd(hd);

    size += Whsize_wosize(sz);

    if (Tag_hd(hd) < No_scan_tag) {
      /* Push the interesting fields on the queue */
      for (i = 0; i < sz; i++) {
        value v2 = Field(v, i);
        if (Is_block(v2) && Is_in_heap_or_young(v2)) {
          if (Tag_hd(Hd_val(v2)) == Infix_tag){
            v2 -= Infix_offset_hd(Hd_val(v2));
          }
          hd = Hd_val(v2);
          if (Color_hd(hd) != Caml_blue) {
            if (write_pos == ENTRIES_PER_QUEUE_CHUNK) {
              struct queue_chunk *new_chunk =
                malloc(sizeof(struct queue_chunk));
              if (new_chunk == NULL) {
                size = (-1);
                goto release;
              }
              write_chunk->next = new_chunk;
              write_pos = 0;
              write_chunk = new_chunk;
            }
            write_chunk->entries[write_pos++] = v2 | Colornum_hd(hd);
            Hd_val(v2) = Bluehd_hd(hd);
          }
        }
      }
    }
  }

  /* Second pass: restore colors and free extra queue chunks */
 release:
  read_pos = 0;
  read_chunk = &first_chunk;
  while (read_pos != write_pos || read_chunk != write_chunk) {
    color_t colornum;
    if (read_pos == ENTRIES_PER_QUEUE_CHUNK) {
      struct queue_chunk *prev = read_chunk;
      read_pos = 0;
      read_chunk = read_chunk->next;
      if (prev != &first_chunk) free(prev);
    }
    v = read_chunk->entries[read_pos++];
    colornum = v & 3;
    v &= ~3;
    Hd_val(v) = Coloredhd_hd(Hd_val(v), colornum);
  }
  if (read_chunk != &first_chunk) free(read_chunk);

  if (size < 0)
    caml_raise_out_of_memory();
  return Val_int(size);
}
