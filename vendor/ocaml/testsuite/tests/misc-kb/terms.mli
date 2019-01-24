type term =
    Var of int
  | Term of string * term list

val union: 'a list -> 'a list -> 'a list
val vars: term -> int list
val vars_of_list: term list -> int list
val substitute: (int * term) list -> term -> term
val replace: term -> int list -> term -> term
val replace_nth: int -> term list -> int list -> term -> term list
val matching: term -> term -> (int * term) list
val compsubst: (int * term) list -> (int * term) list -> (int * term) list
val occurs: int -> term -> bool
val unify: term -> term -> (int * term) list
val infixes: string list
val pretty_term: term -> unit
val pretty_close: term -> unit
