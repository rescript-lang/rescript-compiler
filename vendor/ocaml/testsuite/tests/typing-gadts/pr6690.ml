type 'a visit_action

type insert

type 'a local_visit_action

type ('a, 'result, 'visit_action) context =
  | Local : ('a, ('a * insert) as 'result, 'a local_visit_action) context
  | Global : ('a, 'a, 'a visit_action) context
;;

let vexpr (type visit_action)
    : (_, _, visit_action) context -> _ -> visit_action =
  function
  | Local -> fun _ -> raise Exit
  | Global -> fun _ -> raise Exit
;;
[%%expect{|
type 'a visit_action
type insert
type 'a local_visit_action
type ('a, 'result, 'visit_action) context =
    Local : ('a, 'a * insert, 'a local_visit_action) context
  | Global : ('a, 'a, 'a visit_action) context
Line _, characters 4-9:
Error: This pattern matches values of type
         ($0, $0 * insert, $0 local_visit_action) context
       but a pattern was expected which matches values of type
         ($0, $0 * insert, visit_action) context
       The type constructor $0 would escape its scope
|}, Principal{|
type 'a visit_action
type insert
type 'a local_visit_action
type ('a, 'result, 'visit_action) context =
    Local : ('a, 'a * insert, 'a local_visit_action) context
  | Global : ('a, 'a, 'a visit_action) context
Line _, characters 4-10:
Error: This pattern matches values of type ($1, $1, visit_action) context
       but a pattern was expected which matches values of type
         ($0, $0 * insert, visit_action) context
       Type $1 is not compatible with type $0
|}];;

let vexpr (type visit_action)
    : ('a, 'result, visit_action) context -> 'a -> visit_action =
  function
  | Local -> fun _ -> raise Exit
  | Global -> fun _ -> raise Exit
;;
[%%expect{|
Line _, characters 4-9:
Error: This pattern matches values of type
         ($'a, $'a * insert, $'a local_visit_action) context
       but a pattern was expected which matches values of type
         ($'a, $'a * insert, visit_action) context
       The type constructor $'a would escape its scope
|}, Principal{|
Line _, characters 4-10:
Error: This pattern matches values of type ($1, $1, visit_action) context
       but a pattern was expected which matches values of type
         ($0, $0 * insert, visit_action) context
       Type $1 is not compatible with type $0
|}];;

let vexpr (type result) (type visit_action)
    : (unit, result, visit_action) context -> unit -> visit_action =
  function
  | Local -> fun _ -> raise Exit
  | Global -> fun _ -> raise Exit
;;
[%%expect{|
val vexpr : (unit, 'a, 'b) context -> unit -> 'b = <fun>
|}];;
