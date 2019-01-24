type ab = A | B
type xy = X | Y

type _ repr = AB : ab repr | XY : xy repr

(* Correctly reports fragility w.r.t. [repr], [ab] and [xy]. *)

let vocal_fragile (type t) (r1 : t repr) (r2 : t repr) (t : t) =
  match r1, r2, t with
  | AB, _, A -> ()
  | _, XY, X -> ()
  | _, _, _ -> ()

(* Fails to report fragility on [ab] and [xy]. *)

let silent_fragile1 (type t) (r1 : t repr) (r2 : t repr) (t : t) =
  match r1, r2, t with
  | AB, _, A -> ()
  | _, XY, X -> ()
  | _, AB, _ -> ()
  | XY, _, _ -> ()

let silent_fragile2 (type t) (r1 : t repr) (r2 : t repr) (t : t) =
  match r1, r2, t with
  | AB, _, A -> ()
  | _, XY, X -> ()
  | AB, _, _ -> ()
  | _, XY, _ -> ()
