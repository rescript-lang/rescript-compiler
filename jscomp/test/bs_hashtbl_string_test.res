@@warning("-22")
type seed = int
external caml_hash_mix_string: (seed, string) => seed = "?hash_mix_string"
external final_mix: seed => seed = "?hash_final_mix"

let hash_string = s => final_mix(caml_hash_mix_string(0, s))

let hashString: string => int = %raw(`function(str){ 
                                              var hash = 5381,
                                              i    = str.length | 0;

                                              while(i !== 0) {
                                              hash = (hash * 33) ^ str.charCodeAt(--i);
                                              }
                                              return hash}                                              
                                            `)

module String = unpack(Belt.Id.hashable(~eq=(x: string, y) => x == y, ~hash=Hashtbl.hash))

module String1 = unpack(Belt.Id.hashable(~eq=(x: string, y) => x == y, ~hash=hashString))
module String2 = unpack(
  Belt.Id.hashable(~eq=(x: string, y) => x == y, ~hash=(x: string) => hash_string(x))
)

module Int = unpack(Belt.Id.hashable(~eq=(x: int, y) => x == y, ~hash=Hashtbl.hash))
module N = Belt.HashMap
let empty = N.make(~id=module(Int), ~hintSize=500_000)

let bench = () => {
  let count = 1_000_000
  /* let add = N.setDone in */
  let mem = N.has
  for i in 0 to count {
    N.set(empty, i, i)
  }
  for i in 0 to count {
    assert(mem(empty, i))
  }
  N.logStats(empty)
}

let count = 1_000_000
let initial_size = 1_000_000
/* module B = Belt.Bag */
/*
    (empty : _ Belt.HashMap.t)
    #.add (string_of_int i) i
    #.add (string_of_int i) i
*/
module M = Belt.HashMap
let bench2 = (type t, m: Belt.Id.hashable<string, t>) => {
  let empty = M.make(~id=m, ~hintSize=initial_size)
  module String = unpack(m)
  /* let hash = String.hash in
   let eq = String.eq in */
  /* let table = M.getData empty in */
  for i in 0 to count {
    M.set(empty, string_of_int(i), i)
  }
  for i in 0 to count {
    assert(M.has(empty, string_of_int(i)))
  }
  for i in 0 to count {
    M.remove(empty, string_of_int(i))
  }
  assert (M.size(empty) == 0)
}

/* Belt.HashMap.logStats empty */
module Md = Belt.Map
module Md0 = Belt.Map.Dict
let bench3 = (type t, m: Belt.Id.comparable<string, t>) => {
  let empty = Md.make(~id=m)
  module String = unpack(m)
  let cmp = String.cmp
  let table = ref(Md.getData(empty))
  for i in 0 to count {
    table := Md0.set(~cmp, table.contents, string_of_int(i), i)
  }
  for i in 0 to count {
    assert(Md0.has(~cmp, table.contents, string_of_int(i)))
  }
  for i in 0 to count {
    table := Md0.remove(~cmp, table.contents, string_of_int(i))
  }
  assert (Md0.size(table.contents) == 0)
}

module Sx = unpack(Belt.Id.comparable(~cmp=(x: string, y) => compare(x, y)))
module H = Belt.HashMap.String
let bench4 = () => {
  let table = H.make(~hintSize=initial_size)

  for i in 0 to count {
    H.set(table, string_of_int(i), i)
  }
  for i in 0 to count {
    assert(H.has(table, string_of_int(i)))
  }
  for i in 0 to count {
    H.remove(table, string_of_int(i))
  }
  assert(H.isEmpty(table))
}

module H0 = Belt.HashMap
let bench5 = () => {
  let table = H0.make(~id=module(Int), ~hintSize=initial_size)
  /* let table_data = M.getData table in */
  /* let hash = Int.hash in
   let eq = Int.eq in */
  %time(
    for i in 0 to count {
      H0.set(table, i, i)
    }
  )
  %time(
    for i in 0 to count {
      assert(H0.has(table, i))
    }
  )
  %time(
    for i in 0 to count {
      H0.remove(table, i)
    }
  )
  assert(H0.isEmpty(table))
}

module HI = Belt.HashMap.Int
let bench6 = () => {
  let table = HI.make(~hintSize=initial_size)

  for i in 0 to count {
    HI.set(table, i, i)
  }
  for i in 0 to count {
    assert(HI.has(table, i))
  }
  for i in 0 to count {
    HI.remove(table, i)
  }
  assert (HI.size(table) == 0)
}

module S = Belt.HashSet.Int
let bench7 = () => {
  let table = /* [%time */
  S.make(~hintSize=initial_size * 2)
  /* ] */

  /* [%time */
  for i in 0 to count {
    S.add(table, i)
  }
  /* ] */

  /* [%time */
  for i in 0 to count {
    assert(S.has(table, i))
  }
  /* ] */

  /* [%time */
  for i in 0 to count {
    S.remove(table, i)
  }
  /* ] */

  assert (S.size(table) == 0)
}

/* ;; [%time bench4 ()]
   ;; [%time bench4 ()]
   ;; [%time bench2 (module String1)]
   ;; [%time bench2 (module String2)]

   ;; [%time bench3 (module S)]
   ;; [%time bench5()] */
/* ;; [%time bench6 ()] */
%time(bench7())
/* ;; [%time bench7 ()]
;; [%time bench7 ()]
;; [%time bench7 ()]
;; [%time bench7 ()]
;; [%time bench7 ()] */
