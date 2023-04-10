/* ********************************************************************* */
/*  */
/* OCaml */
/*  */
/* Jacques Garrigue, Nagoya University */
/*  */
/* Copyright 2014 Institut National de Recherche en Informatique et */
/* en Automatique.  All rights reserved.  This file is distributed */
/* under the terms of the Q Public License version 1.0. */
/*  */
/* ********************************************************************* */

/* PR#6435 */
let count = ref(0)

module F = (
  M: {
    type t
    module Set: Set.S with type elt = t
  },
) => {
  let test = set => count := M.Set.cardinal(set) + count.contents
}

module M = F(Offset)

let () = M.test(Offset.M.Set.singleton("42"))
/*
  here we assume value access does not have side effect 
  however, the module should be included 
  since it may contain side effect unless we 
  analyze it does not have side effect
*/
let v = Pr6726.Test.v

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (f, a, b) => Mt_global.collect_eq(test_id, suites, f, a, b)

let () = eq(__LOC__, count.contents, 1)

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
