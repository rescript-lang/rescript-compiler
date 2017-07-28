type (_, _) comp =
  | Eq : ('a, 'a) comp
  | Diff : ('a, 'b) comp
;;

module U = struct type t = T end;;

module M : sig
  type t = T
  val comp : (U.t, t) comp
end = struct
  include U
  let comp = Eq
end;;

match M.comp with | Diff -> false;;

module U = struct type t = {x : int} end;;

module M : sig
  type t = {x : int}
  val comp : (U.t, t) comp
end = struct
  include U
  let comp = Eq
end;;

match M.comp with | Diff -> false;;
