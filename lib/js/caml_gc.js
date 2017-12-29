'use strict';


var dummy_stat = /* record */[
  /* minor_words */0,
  /* promoted_words */0,
  /* major_words */0,
  /* minor_collections */0,
  /* major_collections */0,
  /* heap_words */0,
  /* heap_chunks */0,
  /* live_words */0,
  /* live_blocks */0,
  /* free_words */0,
  /* free_blocks */0,
  /* largest_free */0,
  /* fragments */0,
  /* compactions */0,
  /* top_heap_words */0,
  /* stack_size */0
];

function caml_gc_stat() {
  return dummy_stat;
}

function caml_gc_quick_stat() {
  return dummy_stat;
}

function caml_gc_counters() {
  return /* tuple */[
          0,
          0,
          0
        ];
}

function caml_gc_get() {
  return /* record */[
          /* minor_heap_size */0,
          /* major_heap_increment */0,
          /* space_overhead */0,
          /* verbose */0,
          /* max_overhead */0,
          /* stack_limit */0,
          /* allocation_policy */0
        ];
}

function caml_gc_set() {
  return /* () */0;
}

function caml_gc_minor() {
  return /* () */0;
}

function caml_gc_major_slice() {
  return 0;
}

function caml_gc_major() {
  return /* () */0;
}

function caml_gc_full_major() {
  return /* () */0;
}

function caml_gc_compaction() {
  return /* () */0;
}

function caml_final_register(_, _$1) {
  return /* () */0;
}

function caml_final_release() {
  return /* () */0;
}

exports.caml_gc_stat = caml_gc_stat;
exports.caml_gc_quick_stat = caml_gc_quick_stat;
exports.caml_gc_counters = caml_gc_counters;
exports.caml_gc_get = caml_gc_get;
exports.caml_gc_set = caml_gc_set;
exports.caml_gc_minor = caml_gc_minor;
exports.caml_gc_major_slice = caml_gc_major_slice;
exports.caml_gc_major = caml_gc_major;
exports.caml_gc_full_major = caml_gc_full_major;
exports.caml_gc_compaction = caml_gc_compaction;
exports.caml_final_register = caml_final_register;
exports.caml_final_release = caml_final_release;
/* No side effect */
