(** This test is here to check the latex code generated for variants *)

type s = A | B (** only B is documented here *) | C

type t =
  | A
    (** doc for A.
        {[0]}
        With three paragraphs.
        {[1]}
        To check styling
    *)
  | B
  (** doc for B *)

(** Some documentation for u*)
type u =
| A (** doc for A *) | B of unit (** doc for B *)


(** With records *)
type w =
| A of { x: int }
    (** doc for A *)
| B of { y:int }
    (** doc for B *)

(** With args *)
type z =
| A of int
    (** doc for A *)
| B of int
    (** doc for B *)

(** Gadt notation *)
type a =
    A: a (** doc for A*)

(** Lonely constructor *)
type b =
  B (** doc for B *)

type no_documentation = A | B | C
