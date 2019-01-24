(**
  This test focuses on the printing of documentation for inline record
  within the latex generator.
*)


(** A nice exception *)
exception Simple

(** A less simple exception *)
exception Less of int

(** An open sum type *)
type ext = ..

(** A simple record type for reference *)
type r = { lbl: int (** Field documentation for non-inline, [lbl : int] *);
          more:int list (** More documentation for r, [more : int list] *) }


(** A sum type with one inline record *)
type t = A of {lbl: int (** [A] field documentation *)
              ; more:int list (** More [A] field documentation *) }
(** Constructor documentation *)

(** A sum type with two inline records *)
type s =
  | B of { a_label_for_B : int (** [B] field documentation *);
               more_label_for_B:int list (** More [B] field documentation *) }
  (** Constructor B documentation *)
  | C of { c_has_label_too: float (** [C] field documentation*);
           more_than_one: unit (** ... documentations *)  }
  (** Constructor C documentation *)

(** A gadt constructor *)
type any = D: { any:'a (** [A] field [any:'a] for [D] in [any]. *) } -> any
(** Constructor D documentation *)

exception Error of {name:string (** Error field documentation [name:string] *) }

type ext +=
  | E of { yet_another_field: unit (** Field documentation for [E] in ext *) }
  (** Constructor E documentation *)
  | F of { even_more: int -> int (** Some field documentations for [F] *) }
  (** Constructor F documentation *)
  | G of { last: int -> int (** The last and least field documentation *) }
  (** Constructor G documentation *)
(** Two new constructors for ext *)
