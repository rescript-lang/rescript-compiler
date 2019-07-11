val collect_eq : int ref -> Mt.pair_suites ref -> string -> 'b -> 'b -> unit
val collect_neq : int ref -> Mt.pair_suites ref -> string -> 'b -> 'b -> unit

val collect_approx :
  int ref -> Mt.pair_suites ref -> string -> float -> float -> unit
