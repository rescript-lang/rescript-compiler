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
#include "caml/backtrace_prim.h"
#include "caml/config.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
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

#ifdef WITH_SPACETIME

/* The following structures must match the type definitions in the
   [Spacetime] module. */

typedef struct {
  /* (GC header here.) */
  value minor_words;
  value promoted_words;
  value major_words;
  value minor_collections;
  value major_collections;
  value heap_words;
  value heap_chunks;
  value compactions;
  value top_heap_words;
} gc_stats;

typedef struct {
  value profinfo;
  value num_blocks;
  value num_words_including_headers;
} snapshot_entry;

typedef struct {
  /* (GC header here.) */
  snapshot_entry entries[0];
} snapshot_entries;

typedef struct {
  /* (GC header here.) */
  value time;
  value gc_stats;
  value entries;
  value words_scanned;
  value words_scanned_with_profinfo;
  value total_allocations;
} snapshot;

typedef struct {
  uintnat num_blocks;
  uintnat num_words_including_headers;
} raw_snapshot_entry;

static value allocate_outside_heap_with_tag(mlsize_t size_in_bytes, tag_t tag)
{
  /* CR-soon mshinwell: this function should live somewhere else */
  header_t* block;

  CAMLassert(size_in_bytes % sizeof(value) == 0);
  block = caml_stat_alloc(sizeof(header_t) + size_in_bytes);
  *block = Make_header(size_in_bytes / sizeof(value), tag, Caml_black);
  return (value) &block[1];
}

static value allocate_outside_heap(mlsize_t size_in_bytes)
{
  CAMLassert(size_in_bytes > 0);
  return allocate_outside_heap_with_tag(size_in_bytes, 0);
}

static value take_gc_stats(void)
{
  value v_stats;
  gc_stats* stats;

  v_stats = allocate_outside_heap(sizeof(gc_stats));
  stats = (gc_stats*) v_stats;

  stats->minor_words = Val_long(caml_stat_minor_words);
  stats->promoted_words = Val_long(caml_stat_promoted_words);
  stats->major_words =
    Val_long(((uintnat) caml_stat_major_words)
             + ((uintnat) caml_allocated_words));
  stats->minor_collections = Val_long(caml_stat_minor_collections);
  stats->major_collections = Val_long(caml_stat_major_collections);
  stats->heap_words = Val_long(caml_stat_heap_wsz / sizeof(value));
  stats->heap_chunks = Val_long(caml_stat_heap_chunks);
  stats->compactions = Val_long(caml_stat_compactions);
  stats->top_heap_words = Val_long(caml_stat_top_heap_wsz / sizeof(value));

  return v_stats;
}

static value get_total_allocations(void)
{
  value v_total_allocations = Val_unit;
  allocation_point* total = caml_all_allocation_points;

  while (total != NULL) {
    value v_total;
    v_total = allocate_outside_heap_with_tag(3 * sizeof(value), 0);

    /* [v_total] is of type [Raw_spacetime_lib.total_allocations]. */
    Field(v_total, 0) = Val_long(Profinfo_hd(total->profinfo));
    Field(v_total, 1) = total->count;
    Field(v_total, 2) = v_total_allocations;
    v_total_allocations = v_total;

    CAMLassert (total->next == Val_unit
      || (Is_block(total->next) && Tag_val(total->next) == Infix_tag));
    if (total->next == Val_unit) {
      total = NULL;
    }
    else {
      total = (allocation_point*) Hp_val(total->next);
    }
  }

  return v_total_allocations;
}

static value take_snapshot(double time_override, int use_time_override)
{
  value v_snapshot;
  snapshot* heap_snapshot;
  value v_entries;
  snapshot_entries* entries;
  char* chunk;
  value gc_stats;
  uintnat index;
  uintnat target_index;
  value v_time;
  double time;
  uintnat profinfo;
  uintnat num_distinct_profinfos;
  /* Fixed size buffer to avoid needing a hash table: */
  static raw_snapshot_entry* raw_entries = NULL;
  uintnat words_scanned = 0;
  uintnat words_scanned_with_profinfo = 0;
  value v_total_allocations;

  if (!use_time_override) {
    time = caml_sys_time_unboxed(Val_unit);
  }
  else {
    time = time_override;
  }

  gc_stats = take_gc_stats();

  if (raw_entries == NULL) {
    size_t size = (PROFINFO_MASK + 1) * sizeof(raw_snapshot_entry);
    raw_entries = caml_stat_alloc(size);
    memset(raw_entries, '\0', size);
  } else {
    size_t size = (PROFINFO_MASK + 1) * sizeof(raw_snapshot_entry);
    memset(raw_entries, '\0', size);
  }

  num_distinct_profinfos = 0;

  /* CR-someday mshinwell: consider reintroducing minor heap scanning,
     properly from roots, which would then give a snapshot function
     that doesn't do a minor GC.  Although this may not be that important
     and potentially not worth the effort (it's quite tricky). */

  /* Scan the major heap. */
  chunk = caml_heap_start;
  while (chunk != NULL) {
    char* hp;
    char* limit;

    hp = chunk;
    limit = chunk + Chunk_size (chunk);

    while (hp < limit) {
      header_t hd = Hd_hp (hp);
      switch (Color_hd(hd)) {
        case Caml_blue:
          break;

        default:
          if (Wosize_hd(hd) > 0) { /* ignore atoms */
            profinfo = Profinfo_hd(hd);
            words_scanned += Whsize_hd(hd);
            if (profinfo > 0 && profinfo < PROFINFO_MASK) {
              words_scanned_with_profinfo += Whsize_hd(hd);
              CAMLassert (raw_entries[profinfo].num_blocks >= 0);
              if (raw_entries[profinfo].num_blocks == 0) {
                num_distinct_profinfos++;
              }
              raw_entries[profinfo].num_blocks++;
              raw_entries[profinfo].num_words_including_headers +=
                Whsize_hd(hd);
            }
          }
          break;
      }
      hp += Bhsize_hd (hd);
      CAMLassert (hp <= limit);
    }

    chunk = Chunk_next (chunk);
  }

  if (num_distinct_profinfos > 0) {
    v_entries = allocate_outside_heap(
      num_distinct_profinfos*sizeof(snapshot_entry));
    entries = (snapshot_entries*) v_entries;
    target_index = 0;
    for (index = 0; index <= PROFINFO_MASK; index++) {
      CAMLassert(raw_entries[index].num_blocks >= 0);
      if (raw_entries[index].num_blocks > 0) {
        CAMLassert(target_index < num_distinct_profinfos);
        entries->entries[target_index].profinfo = Val_long(index);
        entries->entries[target_index].num_blocks
          = Val_long(raw_entries[index].num_blocks);
        entries->entries[target_index].num_words_including_headers
          = Val_long(raw_entries[index].num_words_including_headers);
        target_index++;
      }
    }
  } else {
    v_entries = Atom(0);
  }

  CAMLassert(sizeof(double) == sizeof(value));
  v_time = allocate_outside_heap_with_tag(sizeof(double), Double_tag);
  Store_double_val(v_time, time);

  v_snapshot = allocate_outside_heap(sizeof(snapshot));
  heap_snapshot = (snapshot*) v_snapshot;

  v_total_allocations = get_total_allocations();

  heap_snapshot->time = v_time;
  heap_snapshot->gc_stats = gc_stats;
  heap_snapshot->entries = v_entries;
  heap_snapshot->words_scanned
    = Val_long(words_scanned);
  heap_snapshot->words_scanned_with_profinfo
    = Val_long(words_scanned_with_profinfo);
  heap_snapshot->total_allocations = v_total_allocations;

  return v_snapshot;
}

void caml_spacetime_save_snapshot (struct channel *chan, double time_override,
                                   int use_time_override)
{
  value v_snapshot;
  value v_total_allocations;
  snapshot* heap_snapshot;

  Lock(chan);

  v_snapshot = take_snapshot(time_override, use_time_override);

  caml_output_val(chan, Val_long(0), Val_long(0));

  caml_extern_allow_out_of_heap = 1;
  caml_output_val(chan, v_snapshot, Val_long(0));
  caml_extern_allow_out_of_heap = 0;

  Unlock(chan);

  heap_snapshot = (snapshot*) v_snapshot;
  caml_stat_free(Hp_val(heap_snapshot->time));
  caml_stat_free(Hp_val(heap_snapshot->gc_stats));
  if (Wosize_val(heap_snapshot->entries) > 0) {
    caml_stat_free(Hp_val(heap_snapshot->entries));
  }
  v_total_allocations = heap_snapshot->total_allocations;
  while (v_total_allocations != Val_unit) {
    value next = Field(v_total_allocations, 2);
    caml_stat_free(Hp_val(v_total_allocations));
    v_total_allocations = next;
  }

  caml_stat_free(Hp_val(v_snapshot));
}

CAMLprim value caml_spacetime_take_snapshot(value v_time_opt, value v_channel)
{
  struct channel * channel = Channel(v_channel);
  double time_override = 0.0;
  int use_time_override = 0;

  if (Is_block(v_time_opt)) {
    time_override = Double_field(Field(v_time_opt, 0), 0);
    use_time_override = 1;
  }

  caml_spacetime_save_snapshot(channel, time_override, use_time_override);

  return Val_unit;
}

extern struct custom_operations caml_int64_ops;  /* ints.c */

static value
allocate_int64_outside_heap(uint64_t i)
{
  value v;

  v = allocate_outside_heap_with_tag(2 * sizeof(value), Custom_tag);
  Custom_ops_val(v) = &caml_int64_ops;
  Int64_val(v) = i;

  return v;
}

static value
copy_string_outside_heap(char const *s)
{
  int len;
  mlsize_t wosize, offset_index;
  value result;

  len = strlen(s);
  wosize = (len + sizeof (value)) / sizeof (value);
  result = allocate_outside_heap_with_tag(wosize * sizeof(value), String_tag);

  Field (result, wosize - 1) = 0;
  offset_index = Bsize_wsize (wosize) - 1;
  Byte (result, offset_index) = offset_index - len;
  memmove(Bytes_val(result), s, len);

  return result;
}

static value
allocate_loc_outside_heap(struct caml_loc_info li)
{
  value result;

  if (li.loc_valid) {
    result = allocate_outside_heap_with_tag(5 * sizeof(value), 0);
    Field(result, 0) = Val_bool(li.loc_is_raise);
    Field(result, 1) = copy_string_outside_heap(li.loc_filename);
    Field(result, 2) = Val_int(li.loc_lnum);
    Field(result, 3) = Val_int(li.loc_startchr);
    Field(result, 4) = Val_int(li.loc_endchr);
  } else {
    result = allocate_outside_heap_with_tag(sizeof(value), 1);
    Field(result, 0) = Val_bool(li.loc_is_raise);
  }

  return result;
}

value caml_spacetime_timestamp(double time_override, int use_time_override)
{
  double time;
  value v_time;

  if (!use_time_override) {
    time = caml_sys_time_unboxed(Val_unit);
  }
  else {
    time = time_override;
  }

  v_time = allocate_outside_heap_with_tag(sizeof(double), Double_tag);
  Store_double_val(v_time, time);

  return v_time;
}

value caml_spacetime_frame_table(void)
{
  /* Flatten the frame table into a single associative list. */

  value list = Val_long(0);  /* the empty list */
  uintnat i;

  if (!caml_debug_info_available()) {
    return list;
  }

  if (caml_frame_descriptors == NULL) {
    caml_init_frame_descriptors();
  }

  for (i = 0; i <= caml_frame_descriptors_mask; i++) {
    frame_descr* descr = caml_frame_descriptors[i];
    if (descr != NULL) {
      value location, return_address, pair, new_list_element, location_list;
      struct caml_loc_info li;
      debuginfo dbg;
      if (descr->frame_size != 0xffff) {
        dbg = caml_debuginfo_extract(descr);
        if (dbg != NULL) {
          location_list = Val_unit;
          while (dbg != NULL) {
            value list_element;

            caml_debuginfo_location(dbg, &li);
            location = allocate_loc_outside_heap(li);

            list_element =
              allocate_outside_heap_with_tag(2 * sizeof(value), 0 /* (::) */);
            Field(list_element, 0) = location;
            Field(list_element, 1) = location_list;
            location_list = list_element;

            dbg = caml_debuginfo_next(dbg);
          }

          return_address = allocate_int64_outside_heap(descr->retaddr);
          pair = allocate_outside_heap_with_tag(2 * sizeof(value), 0);
          Field(pair, 0) = return_address;
          Field(pair, 1) = location_list;

          new_list_element =
            allocate_outside_heap_with_tag(2 * sizeof(value), 0 /* (::) */);
          Field(new_list_element, 0) = pair;
          Field(new_list_element, 1) = list;
          list = new_list_element;
        }
      }
    }
  }

  return list;
}

static void add_unit_to_shape_table(uint64_t *unit_table, value *list)
{
  /* This function reverses the order of the lists giving the layout of each
     node; however, spacetime_profiling.ml ensures they are emitted in
     reverse order, so at the end of it all they're not reversed. */

  uint64_t* ptr = unit_table;

  while (*ptr != (uint64_t) 0) {
    value new_list_element, pair, function_address, layout;

    function_address =
      allocate_int64_outside_heap(*ptr++);

    layout = Val_long(0);  /* the empty list */
    while (*ptr != (uint64_t) 0) {
      int tag;
      int stored_tag;
      value part_of_shape;
      value new_part_list_element;
      value location;
      int has_extra_argument = 0;

      stored_tag = *ptr++;
      /* CR-soon mshinwell: share with emit.mlp */
      switch (stored_tag) {
        case 1:  /* direct call to given location */
          tag = 0;
          has_extra_argument = 1;  /* the address of the callee */
          break;

        case 2:  /* indirect call to given location */
          tag = 1;
          break;

        case 3:  /* allocation at given location */
          tag = 2;
          break;

        default:
          CAMLassert(0);
          abort();  /* silence compiler warning */
      }

      location = allocate_int64_outside_heap(*ptr++);

      part_of_shape = allocate_outside_heap_with_tag(
        sizeof(value) * (has_extra_argument ? 2 : 1), tag);
      Field(part_of_shape, 0) = location;
      if (has_extra_argument) {
        Field(part_of_shape, 1) =
          allocate_int64_outside_heap(*ptr++);
      }

      new_part_list_element =
        allocate_outside_heap_with_tag(2 * sizeof(value), 0 /* (::) */);
      Field(new_part_list_element, 0) = part_of_shape;
      Field(new_part_list_element, 1) = layout;
      layout = new_part_list_element;
    }

    pair = allocate_outside_heap_with_tag(2 * sizeof(value), 0);
    Field(pair, 0) = function_address;
    Field(pair, 1) = layout;

    new_list_element =
      allocate_outside_heap_with_tag(2 * sizeof(value), 0 /* (::) */);
    Field(new_list_element, 0) = pair;
    Field(new_list_element, 1) = *list;
    *list = new_list_element;

    ptr++;
  }
}

value caml_spacetime_shape_table(void)
{
  value list;
  uint64_t* unit_table;
  shape_table *dynamic_table;
  uint64_t** static_table;

  /* Flatten the hierarchy of shape tables into a single associative list
     mapping from function symbols to node layouts.  The node layouts are
     themselves lists. */

  list = Val_long(0);  /* the empty list */

  /* Add static shape tables */
  static_table = caml_spacetime_static_shape_tables;
  while (*static_table != (uint64_t) 0) {
    unit_table = *static_table++;
    add_unit_to_shape_table(unit_table, &list);
  }

  /* Add dynamic shape tables */
  dynamic_table = caml_spacetime_dynamic_shape_tables;

  while (dynamic_table != NULL) {
    unit_table = dynamic_table->table;
    add_unit_to_shape_table(unit_table, &list);
    dynamic_table = dynamic_table->next;
  }

  return list;
}

#else

static value spacetime_disabled()
{
  caml_failwith("Spacetime profiling not enabled");
}

CAMLprim value caml_spacetime_take_snapshot(value ignored)
{
  return Val_unit;
}

CAMLprim value caml_spacetime_marshal_frame_table ()
{
  return spacetime_disabled();
}

CAMLprim value caml_spacetime_frame_table ()
{
  return spacetime_disabled();
}

CAMLprim value caml_spacetime_marshal_shape_table ()
{
  return spacetime_disabled();
}

CAMLprim value caml_spacetime_shape_table ()
{
  return spacetime_disabled();
}

#endif
