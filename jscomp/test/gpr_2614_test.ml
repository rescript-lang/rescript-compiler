
[@@@ocaml.warning "-a"]

type t = {

  mutable hi : int
    [@bs.as "Content-Type"];
  mutable low : int
    [@bs.as "l"];
  mutable x : int;
    [@bs.as "open"]
} [@@bs.deriving abstract]
  (* [@@bs.x] *)


let v = t ~hi:3 ~low:2 ~x:2


let (a,b,c) = (v |. hi, v |. low, v |. x)

(**

  v |. (hi, lo)
*)
let ff () =
  v |. hiSet 3;
  v |. lowSet 2


type a = {
  mutable low : string option
      [@bs.as "lo-x"]
;
  hi : int
} [@@bs.deriving abstract]


(**
external a : ?low:int -> hi:int -> a
low: a -> int option [@@bs.return undefined_to_opt]
lowSet : a -> int -> unit
*)
let h0 =
  a ~hi:2 ~low:"x"

let h1 =   a ~hi:2 ~low:"x" ()

let h2 = a ~hi:2 ()

let hh x =
  x |. lowSet "3";
  x |. low

(** should we make the type of

    lowSet : a -> string option -> unit
    lowSet : a -> string -> unit
*)

let hh2 x =
  match x |. low with
  | None -> 0
  | Some _ -> 1

type css =
  {
    a0 : int option ;
    a1 : int option ;
    a2 : int option ;
    a3 : int option ;
    a4 : int option ;
    a5 : int option ;
    a6 : int option ;
    a7 : int option ;
    a8 : int option ;
    a9 : int option
    [@bs.as "xx-yy"];
    a10 : int option ;
    a11 : int option ;
    a12 : int option ;
    a13 : int option ;
    a14 : int option ;
    a15 : int option ;
  }
  [@@bs.deriving abstract]


let u = css ~a9:3 ()
let v =
  match u |. a9 with
  | None -> 0
  | Some x -> x 
