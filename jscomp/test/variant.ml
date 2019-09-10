type pair = int * int

type v = | A1 | A2 | B of int | C of int * int | D of pair

let a1 = A1
let a2 = A2

let b = B 34

let c = C (4,2)

let d = D (4,2)

let foo = function
| A1 -> 1
| A2 -> 2
| B n -> n
| C (n,m) -> n+m
| D (n,m) -> n+m

let fooA1 = function
| A1 -> 1
| _ -> 42

let fooC = function
| C (n,m) -> n+m
| _ -> 42

let switchNum = function
| 0 -> "0"
| 1 -> "1"
| 2 -> "2"
| _ -> "_"

module Path = struct
  type t =
      Pident of string
    | Pdot of t * string * int
    | Papply of t * t
  let same = (=)
  let compare = compare
end

module Make(M : sig type t = Path.t end) = struct
  type t = M.t
  let find (x:t) = ()
end

module M = Make(Path)

let rollback_path subst p =
  let _ = M.find p in
  try "try"
  with Not_found ->
    match p with
      Pident _ | Papply _ -> "Pident | Papply"
    | Pdot _ -> "Pdot"


 exception EA1
 exception EA2
 exception EB of int
 exception EC of int * int
 exception ED of pair

let fooExn f = try f () with
| EA1 -> 1
| EA2 -> 2
| EB n -> n
| EC (n,m) -> n+m
| ED (n,m) -> n+m
