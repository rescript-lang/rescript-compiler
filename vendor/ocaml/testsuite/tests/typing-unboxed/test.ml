
external a : (int [@untagged]) -> unit = "a" "a_nat"
external b : (int32 [@unboxed]) -> unit = "b" "b_nat"
external c : (int64 [@unboxed]) -> unit = "c" "c_nat"
external d : (nativeint [@unboxed]) -> unit = "d" "d_nat"
external e : (float [@unboxed]) -> unit = "e" "e_nat"

type t = private int

external f : (t [@untagged]) -> unit = "f" "f_nat"

module M : sig
  external a : int -> (int [@untagged]) = "a" "a_nat"
  external b : (int [@untagged]) -> int = "b" "b_nat"
end = struct
  external a : int -> (int [@untagged]) = "a" "a_nat"
  external b : (int [@untagged]) -> int = "b" "b_nat"
end;;

module Global_attributes = struct
  [@@@ocaml.warning "-3"]

  external a : float -> float = "a" "noalloc" "a_nat" "float"
  external b : float -> float = "b" "noalloc" "b_nat"
  external c : float -> float = "c" "c_nat" "float"
  external d : float -> float = "d" "noalloc"
  external e : float -> float = "e"

  (* Should output a warning: no native implementation provided *)
  external f : (int32 [@unboxed]) -> (int32 [@unboxed]) = "f" "noalloc"
  external g : int32 -> int32 = "g" "g_nat" [@@unboxed] [@@noalloc]

  external h : (int [@untagged]) -> (int [@untagged]) = "h" "h_nat" "noalloc"
  external i : int -> int = "i" "i_nat" [@@untagged] [@@noalloc]
end;;

module Old_style_warning = struct
  [@@@ocaml.warning "+3"]
  external a : float -> float = "a" "noalloc" "a_nat" "float"
  external b : float -> float = "b" "noalloc" "b_nat"
  external c : float -> float = "c" "c_nat" "float"
  external d : float -> float = "d" "noalloc"
  external e : float -> float = "c" "float"
end

(* Bad: attributes not reported in the interface *)

module Bad1 : sig
  external f : int -> int = "f" "f_nat"
end = struct
  external f : int -> (int [@untagged]) = "f" "f_nat"
end;;

module Bad2 : sig
  external f : int -> int = "a" "a_nat"
end = struct
  external f : (int [@untagged]) -> int = "f" "f_nat"
end;;

module Bad3 : sig
  external f : float -> float = "f" "f_nat"
end = struct
  external f : float -> (float [@unboxed]) = "f" "f_nat"
end;;

module Bad4 : sig
  external f : float -> float = "a" "a_nat"
end = struct
  external f : (float [@unboxed]) -> float = "f" "f_nat"
end;;

(* Bad: attributes in the interface but not in the implementation *)

module Bad5 : sig
  external f : int -> (int [@untagged]) = "f" "f_nat"
end = struct
  external f : int -> int = "f" "f_nat"
end;;

module Bad6 : sig
  external f : (int [@untagged]) -> int = "f" "f_nat"
end = struct
  external f : int -> int = "a" "a_nat"
end;;

module Bad7 : sig
  external f : float -> (float [@unboxed]) = "f" "f_nat"
end = struct
  external f : float -> float = "f" "f_nat"
end;;

module Bad8 : sig
  external f : (float [@unboxed]) -> float = "f" "f_nat"
end = struct
  external f : float -> float = "a" "a_nat"
end;;

(* Bad: unboxed or untagged with the wrong type *)

external g : (float [@untagged]) -> float = "g" "g_nat";;
external h : (int [@unboxed]) -> float = "h" "h_nat";;

(* Bad: unboxing the function type *)
external i : int -> float [@unboxed] = "i" "i_nat";;

(* Bad: unboxing a "deep" sub-type. *)
external j : int -> (float [@unboxed]) * float = "j" "j_nat";;

(* This should be rejected, but it is quite complicated to do
   in the current state of things *)

external k : int -> (float [@unboxd]) = "k" "k_nat";;

(* Bad: old style annotations + new style attributes *)

external l : float -> float = "l" "l_nat" "float" [@@unboxed];;
external m : (float [@unboxed]) -> float = "m" "m_nat" "float";;
external n : float -> float = "n" "noalloc" [@@noalloc];;

(* Warnings: unboxed / untagged without any native implementation *)
external o : (float[@unboxed]) -> float = "o";;
external p : float -> (float[@unboxed]) = "p";;
external q : (int[@untagged]) -> float = "q";;
external r : int -> (int[@untagged]) = "r";;
external s : int -> int = "s" [@@untagged];;
external t : float -> float = "t" [@@unboxed];;

(* PR#7424 *)
type 'a b = B of 'a b b [@@unboxed];;
