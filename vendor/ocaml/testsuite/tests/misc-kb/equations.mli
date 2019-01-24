open Terms

type rule =
  { number: int;
    numvars: int;
    lhs: term;
    rhs: term }

val mk_rule: int -> term -> term -> rule
val check_rules: rule list -> int
val pretty_rule: rule -> unit
val pretty_rules: rule list -> unit
val reduce: term -> term -> term -> term
val reducible: term -> term -> bool
val mreduce: rule list -> term -> term
val mrewrite1: rule list -> term -> term
val mrewrite1_sons: rule list -> term list -> term list
val mrewrite_all: rule list -> term -> term
