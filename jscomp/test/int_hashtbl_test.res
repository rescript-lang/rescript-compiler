open Hashtbl

module type S = Hashtbl.S with type key = int

/*
let to_list (module H : S) (tbl : 'a H.t) = 
  H.fold (fun k v acc -> (k,v)::acc) tbl []
*/

let f = (module(H: Hashtbl.S with type key = int)) => {
  /* let module Hashtbl = (val Hashtbl) in */
  let tbl = H.create(17)
  H.add(tbl, 1, '1')
  H.add(tbl, 2, '2')
  \"@@"(
    List.sort(((a: int, _), (b, _)) => compare(a, b)),
    H.fold((k, v, acc) => list{(k, v), ...acc}, tbl, list{}),
  )
}

let g = (module(H: S), count) => {
  let tbl = H.create(17)
  for i in 0 to count {
    H.replace(tbl, i * 2, string_of_int(i))
  }
  for i in 0 to count {
    H.replace(tbl, i * 2, string_of_int(i))
  }
  let v = H.fold((k, v, acc) => list{(k, v), ...acc}, tbl, list{})
  let v = \"@@"(List.sort(((x, _), (y: int, _)) => compare(x, y)), v)
  Array.of_list(v)
}

module Int_hash = Hashtbl.Make({
  type t = int
  let hash = x => Hashtbl.hash(x)
  let equal = (x: int, y) => x == y
})

let suites = {
  open Mt
  list{
    ("simple", _ => Eq(list{(1, '1'), (2, '2')}, f(module(Int_hash)))),
    (
      "more_iterations",
      _ => {
        let count = 1000
        Eq(Array.init(count + 1, i => (2 * i, string_of_int(i))), g(module(Int_hash), count))
      },
    ),
  }
}

Mt.from_pair_suites(__MODULE__, suites)
