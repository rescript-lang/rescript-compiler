type (_, _) t =
    Any : ('a, 'b) t
  | Eq : ('a, 'a) t
;;

module M :
sig
  type s = private [> `A]
  val eq : (s, [`A | `B]) t
end =
struct
  type s = [`A | `B]
  let eq = Eq
end;;

let f : (M.s, [`A | `B]) t -> string = function
  | Any -> "Any"
;;

let () = print_endline (f M.eq) ;;

module N :
sig
  type s = private < a : int; .. >
  val eq : (s, <a : int; b : bool>) t
end =
struct
  type s = <a : int; b : bool>
  let eq = Eq
end
;;

let f : (N.s, <a : int; b : bool>) t -> string = function
  | Any -> "Any"
;;
