


function caml_gc_counters(param) {
  return /* tuple */[
          0,
          0,
          0
        ];
}

function caml_gc_set(param) {
  return /* () */0;
}

function caml_gc_minor(param) {
  return /* () */0;
}

function caml_gc_major_slice(param) {
  return 0;
}

function caml_gc_major(param) {
  return /* () */0;
}

function caml_gc_full_major(param) {
  return /* () */0;
}

function caml_gc_compaction(param) {
  return /* () */0;
}

function caml_final_register(param, param$1) {
  return /* () */0;
}

function caml_final_release(param) {
  return /* () */0;
}

export {
  caml_gc_counters ,
  caml_gc_set ,
  caml_gc_minor ,
  caml_gc_major_slice ,
  caml_gc_major ,
  caml_gc_full_major ,
  caml_gc_compaction ,
  caml_final_register ,
  caml_final_release ,
  
}
/* No side effect */
