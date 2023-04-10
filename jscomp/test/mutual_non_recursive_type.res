module U = {
  type t = OT
  let f = x => x
}

open U

type t =
  | Ta(t) /* * u compilation error [nonrec applices to all] */
  | Tb(int)
and u = | @ocaml.doc(" one attribute nonrecursive will affect all ") H(t) /* refers to old t */

let v: u = H(OT)
