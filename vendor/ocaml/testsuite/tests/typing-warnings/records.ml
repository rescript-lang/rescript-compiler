(* Use type information *)
module M1 = struct
  type t = {x: int; y: int}
  type u = {x: bool; y: bool}
end;;

module OK = struct
  open M1
  let f1 (r:t) = r.x (* ok *)
  let f2 r = ignore (r:t); r.x (* non principal *)

  let f3 (r: t) =
    match r with {x; y} -> y + y (* ok *)
end;;

module F1 = struct
  open M1
  let f r = match r with {x; y} -> y + y
end;; (* fails *)

module F2 = struct
  open M1
  let f r =
    ignore (r: t);
    match r with
       {x; y} -> y + y
end;; (* fails for -principal *)

(* Use type information with modules*)
module M = struct
  type t = {x:int}
  type u = {x:bool}
end;;
let f (r:M.t) = r.M.x;; (* ok *)
let f (r:M.t) = r.x;; (* warning *)
let f ({x}:M.t) = x;; (* warning *)

module M = struct
  type t = {x: int; y: int}
end;;
module N = struct
  type u = {x: bool; y: bool}
end;;
module OK = struct
  open M
  open N
  let f (r:M.t) = r.x
end;; 

module M = struct
  type t = {x:int}
  module N = struct type s = t = {x:int} end
  type u = {x:bool}
end;;
module OK = struct
  open M.N
  let f (r:M.t) = r.x
end;;

(* Use field information *)
module M = struct
  type u = {x:bool;y:int;z:char}
  type t = {x:int;y:bool}
end;;
module OK = struct
  open M
  let f {x;z} = x,z
end;; (* ok *)
module F3 = struct
  open M
  let r = {x=true;z='z'}
end;; (* fail for missing label *)

module OK = struct
  type u = {x:int;y:bool}
  type t = {x:bool;y:int;z:char}
  let r = {x=3; y=true}
end;; (* ok *)

(* Corner cases *)

module F4 = struct
  type foo = {x:int; y:int}
  type bar = {x:int}
  let b : bar = {x=3; y=4}
end;; (* fail but don't warn *)

module M = struct type foo = {x:int;y:int} end;;
module N = struct type bar = {x:int;y:int} end;;
let r = { M.x = 3; N.y = 4; };; (* error: different definitions *)

module MN = struct include M include N end
module NM = struct include N include M end;;
let r = {MN.x = 3; NM.y = 4};; (* error: type would change with order *)

(* Lpw25 *)

module M = struct
  type foo = { x: int; y: int }
  type bar = { x:int; y: int; z: int}
end;;
module F5 = struct
  open M
  let f r = ignore (r: foo); {r with x = 2; z = 3}
end;;
module M = struct
  include M
  type other = { a: int; b: int }
end;;
module F6 = struct
  open M
  let f r = ignore (r: foo); { r with x = 3; a = 4 }
end;;
module F7 = struct
  open M
  let r = {x=1; y=2}
  let r: other = {x=1; y=2}
end;;

module A = struct type t = {x: int} end
module B = struct type t = {x: int} end;;
let f (r : B.t) = r.A.x;; (* fail *)

(* Spellchecking *)

module F8 = struct
  type t = {x:int; yyy:int}
  let a : t = {x=1;yyz=2}
end;;

(* PR#6004 *)

type t = A
type s = A

class f (_ : t) = object end;;
class g = f A;; (* ok *)

class f (_ : 'a) (_ : 'a) = object end;;
class g = f (A : t) A;; (* warn with -principal *)


(* PR#5980 *)

module Shadow1 = struct
  type t = {x: int}
  module M = struct
    type s = {x: string}
  end
  open M  (* this open is unused, it isn't reported as shadowing 'x' *)
  let y : t = {x = 0}
end;;
module Shadow2 = struct
  type t = {x: int}
  module M = struct
    type s = {x: string}
  end
  open M  (* this open shadows label 'x' *)
  let y = {x = ""}
end;;

(* PR#6235 *)

module P6235 = struct
  type t = { loc : string; }
  type v = { loc : string; x : int; }
  type u = [ `Key of t ]
  let f (u : u) = match u with `Key {loc} -> loc
end;;

(* Remove interaction between branches *)

module P6235' = struct
  type t = { loc : string; }
  type v = { loc : string; x : int; }
  type u = [ `Key of t ]
  let f = function
    | (_ : u) when false -> ""
    |`Key {loc} -> loc
end;;
