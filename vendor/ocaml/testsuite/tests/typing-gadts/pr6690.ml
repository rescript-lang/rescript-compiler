type 'a visit_action

type insert

type 'a local_visit_action

type ('a, 'result, 'visit_action) context =
  | Local : ('a, ('a * insert) as 'result, 'a local_visit_action) context
  | Global : ('a, 'a, 'a visit_action) context
;;

let vexpr (type visit_action) : (_, _, visit_action) context -> _ -> visit_action =
  function
  | Local -> fun _ -> raise Exit
  | Global -> fun _ -> raise Exit
;;

let vexpr (type visit_action) : ('a, 'result, visit_action) context -> 'a -> visit_action =
  function
  | Local -> fun _ -> raise Exit
  | Global -> fun _ -> raise Exit
;;

let vexpr (type result) (type visit_action) : (unit, result, visit_action) context -> unit -> visit_action =
  function
  | Local -> fun _ -> raise Exit
  | Global -> fun _ -> raise Exit
;;
