(* A regression test for both PR#4141 and PR#5819: when a recursive
   variable is defined by a { record with ... } expression.
*)

type t = {
  self : t;
  t0 : int;
  t1 : int;
  t2 : int;
  t3 : int;
  t4 : int;
};;
let rec t = {
  self = t;
  t0 = 42;
  t1 = 42;
  t2 = 42;
  t3 = 42;
  t4 = 42;
};;

let rec self = { t with self=self } in
Printf.printf "%d\n" self.self.t0
;;
