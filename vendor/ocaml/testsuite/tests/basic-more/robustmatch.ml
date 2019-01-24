(* TEST
   flags += "-w +4+8+9+11+12+52+56+57"
   include testing
*)

module GPR1493 = struct
  type t1 = { x : int; y : string; }
  type t2 = { a : int; b : string; c : string list; }

  type t = ..
  type t += C1 of t1 | C2 of t2

  let f (x : t) =
    match x with
    | C1 { x; y } -> ()
    | C2 { a;b;c } -> ()
    | _ -> ()
end

module Coherence_illustration = struct
  type ab = A | B

  module M : sig
    type mab = A | B

    type _ t = AB : ab t | MAB : mab t
  end = struct
    type mab = ab = A | B

    type _ t = AB : ab t | MAB : mab t

    let f (type x) (t1 : x t) (t2 : x t) (x : x) =
      match t1, t2, x with
      | AB, AB, A -> ()
      | MAB, _, A -> ()
      | _,  AB, B -> ()
      | _, MAB, B -> ()
  end

  open M

  let f (type x) (t1 : x t) (t2 : x t) (x : x) =
    match t1, t2, x with
    | AB,  AB, A -> ()
    | MAB, _, A -> ()
    | _,  AB, B -> ()
    | _, MAB, B -> ()
end

module M1 = struct
  type _ repr = R1 : int repr | R2 : string repr

  let f (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, 0 -> ()
    | _, R2, "coucou" -> ()
end

module M2 = struct
  type c = A | B | C
  type _ repr = R1 : c repr | R2 : string repr

  let f (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, A -> ()
    | _, R2, "coucou" -> ()

  let g (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | _, R2, "coucou" -> ()
    | R1, _, A -> ()

  let h (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | _, R2, "coucou" -> ()
    | R1, _, _ -> ()
end

module M3 = struct
  type c1 = A | B | C
  type c2 = X | Y | Z
  type _ repr = R1 : c1 repr | R2 : c2 repr

  let f (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, A -> ()
    | _, R2, X -> ()

  let g (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, A -> ()
    | _, R2, X -> ()
    | R1, _, _ -> ()

  let h (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, _ -> ()
    | _, R2, X -> ()
end

module M3_gadt = struct
  type c1 = A | B | C
  type _ c2 = X : int c2 | Y : char c2 | Z : char c2
  type _ repr = R1 : c1 repr | R2 : int c2 repr

  let f (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, A -> ()
    | _, R2, X -> ()

  let g (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, A -> ()
    | _, R2, X -> ()
    | R1, _, _ -> ()

  let h (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, _ -> ()
    | _, R2, X -> ()
end

module M3_gadt_bis = struct
  type _ c1 = A : int c1 | B : int c1 | C : char c1
  type _ c2 = X : int c2 | Y : char c2 | Z : char c2
  type _ repr = R1 : int c1 repr | R2 : int c2 repr

  let f (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, A -> ()
    | _, R2, X -> ()

  let g (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, A -> ()
    | _, R2, X -> ()
    | R1, _, B -> ()

  let h (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, _ -> ()
    | _, R2, X -> ()
end

module M3_gadt_bis_harder = struct
  type _ c1 = A : int c1 | B : int c1 | C : char c1
  type _ c2 = X : int c2 | Y : char c2 | Z : char c2
  type _ repr = R1 : 'a c1 repr | R2 : 'a c2 repr

  let f (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, A -> ()
    | _, R2, X -> ()

  let g (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, A -> ()
    | _, R2, X -> ()
    | R1, _, _ -> ()

  let h (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, _ -> ()
    | _, R2, X -> ()

  let h (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, C -> ()
    | _, R2, Y -> ()
end

module M4 = struct
  type _ repr = R1 : int repr | R2 : int array repr

  let f (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | _, R1, 0 -> ()
    | R2, _, [||] -> ()
    | _, R1, 1 -> ()

  let g (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, _ -> ()
    | _, R2, [||] -> ()

  let h (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | _, R2, [||] -> ()
    | R1, _, 0 -> ()
    | R1, _, _ -> ()

  let i (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | _, R2, [||] -> ()
    | R1, _, 0 -> ()
    | R1, _, _ -> ()
    | _, R2, _ -> ()

  let j (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | _, R2, [||] -> ()
    | R1, _, 0 -> ()
    | _, _, _ -> ()
end

module M5 = struct
  type _ repr = R1 : char repr | R2 : string repr

  let f (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, 'c' -> ()
    | _, R2, "coucou" -> ()
end

module M6 = struct
  type _ repr = R1 : [ `A | `B ] repr | R2 : string repr

  let f (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, `A -> ()
    | _, R2, "coucou" -> ()
end

module M7 = struct
  type _ repr = R1 : (int * string) repr | R2 : (int * string * char) repr

  let f (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, (3, "") -> ()
    | _, R2, (1, "coucou", 'a') -> ()
end

module M8 = struct
  type r1 = { x : int; y : string }
  type r2 = { a : int; b : string; c : char }
  type _ repr = R1 : r1 repr | R2 : r2 repr

  let f (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, { x = 3; y = "" } -> ()
    | _, R2, { a = 1; b = "coucou"; c = 'a' } -> ()

  let g (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R2, _, { a = 1; b = "coucou"; c = 'a' } -> ()
    | _, R1, { x = 3; y = "" } -> ()
end

module M9 = struct
  type _ repr = R1 : (int * string) repr | R2 : int repr

  let f (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, (3, "") -> ()
    | _, R2, 1 -> ()
end

module M10 = struct
  type r = { x : int; y : string }
  type _ repr = R1 : r repr | R2 : int repr

  let f (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, { x = 3; y = "" } -> ()
    | _, R2, 1 -> ()
end

module M11 = struct
  type _ repr = R1 : int lazy_t repr | R2 : int repr

  let f (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, lazy 1 -> ()
    | _, R2, 1 -> ()
end

module M12 = struct
  type _ repr = R1 : unit repr | R2 : string repr

  let f (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, () -> ()
    | _, R2, "coucou" -> ()
    | _, R2, "foo" -> ()
end
