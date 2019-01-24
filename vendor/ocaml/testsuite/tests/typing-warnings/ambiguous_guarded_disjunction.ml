(* Ignore OCAMLRUNPARAM=b to be reproducible *)
Printexc.record_backtrace false;;

let () = print_endline "\n\
  <----------------------------------------------------------------------\n\
  To check the result file for this test, it suffices to look for \"val\"\n\
  lines corresponding to toplevel answers. If they start with\n\
  \n\
  \    val ambiguous_...\n\
  \n\
  then just above there should be the warning text for Warning 57\n\
  (we try to avoid all other warnings). If they start with\n\
  \n\
  \   val not_ambiguous_...\n\
  \n\
  then just above there should be *no* warning text.\n\
  ---------------------------------------------------------------------->\n\
";;


type expr = Val of int | Rest;;

let ambiguous_typical_example = function
  | ((Val x, _) | (_, Val x)) when x < 0 -> ()
  | (_, Rest) -> ()
  | (_, Val x) ->
      (* the reader might expect *)
      assert (x >= 0);
      (* to hold here, but it is wrong! *)
      ()
;;

let () = print_endline "Note that an Assert_failure is expected just below.";;
let fails = ambiguous_typical_example (Val 2, Val (-1))
;;

let not_ambiguous__no_orpat = function
  | Some x when x > 0 -> ()
  | Some _ -> ()
  | None -> ()
;;

let not_ambiguous__no_guard = function
  | `A -> ()
  | (`B | `C) -> ()
;;

let not_ambiguous__no_patvar_in_guard b = function
  | (`B x | `C x) when b -> ignore x
  | _ -> ()
;;

let not_ambiguous__disjoint_cases = function
  | (`B x | `C x) when x -> ()
  | _ -> ()
;;

(* the curious (..., _, Some _) | (..., Some _, _) device used in
   those tests serves to avoid warning 12 (this sub-pattern
   is unused), by making sure that, even if the two sides of the
   disjunction overlap, none is fully included in the other. *)
let not_ambiguous__prefix_variables = function
  | (`B (x, _, Some y) | `B (x, Some y, _)) when x -> ignore y
  | _ -> ()
;;

let ambiguous__y = function
  | (`B (x, _, Some y) | `B (x, Some y, _)) when y -> ignore x
  | _ -> ()
;;

(* it should be understood that the ambiguity warning only protects
     (p | q) when guard -> ...
   it will never warn on
     (p | q) -> if guard ...
   This is not a limitation. The point is that people have an
   intuitive understanding of [(p | q) when guard -> ...] that differs
   from the reality, while there is no such issue with
   [(p | q) -> if guard ...].
*)
let not_ambiguous__rhs_not_protected = function
  | (`B (x, _, Some y) | `B (x, Some y, _)) -> if y then ignore x else ()
  | _ -> ()
;;

let ambiguous__x_y = function
  | (`B (x, _, Some y) | `B (x, Some y, _)) when x < y -> ()
  | _ -> ()
;;

let ambiguous__x_y_z = function
  | (`B (x, z, Some y) | `B (x, Some y, z)) when x < y || Some x = z -> ()
  | _ -> ()
;;

let not_ambiguous__disjoint_in_depth = function
  | `A (`B x | `C x) when x -> ()
  | _ -> ()
;;

let not_ambiguous__prefix_variables_in_depth = function
  | `A (`B (x, `C1) | `B (x, `C2)) when x -> ()
  | _ -> ()
;;

let ambiguous__in_depth = function
  | `A (`B (Some x, _) | `B (_, Some x)) when x -> ()
  | _ -> ()
;;

let not_ambiguous__several_orpats = function
  | `A ((`B (x, Some _, _) | `B (x, _, Some _)),
        (`C (y, Some _, _) | `C (y, _, Some _)),
        (`D1 (_, z, Some _, _) | `D2 (_, z, _, Some _))) when x < y && x < z ->
      ()
  | _ -> ()
;;

let ambiguous__first_orpat = function
  | `A ((`B (Some x, _) | `B (_, Some x)),
        (`C (Some y, Some _, _) | `C (Some y, _, Some _))) when x < y -> ()
  | _ -> ()
;;

let ambiguous__second_orpat = function
  | `A ((`B (Some x, Some _, _) | `B (Some x, _, Some _)),
        (`C (Some y, _) | `C (_, Some y))) when x < y -> ()
  | _ -> ()
;;

(* check that common prefixes work as expected *)
let not_ambiguous__pairs = function
  | (x, Some _, _) | (x, _, Some _) when x -> ()
  | _ -> ()
;;

let not_ambiguous__vars =
  begin[@warning "-12"] function
  | (x | x) when x -> ()
  | _ -> ()
  end
;;

let not_ambiguous__as p = function
  | (([], _) as x | ((_, []) as x)) when p x -> ()
  | _ -> ()
;;

let not_ambiguous__as_var p = function
  | (([], _) as x | x) when p x -> ()
  | _ -> ()
;;

let not_ambiguous__var_as p = function
  | (x, Some _, _) | (([], _) as x, _, Some _) when p x -> ()
  | _ -> ()
;;

let not_ambiguous__lazy = function
  | (([], _), lazy x) | ((_, []), lazy x) when x -> ()
  | _ -> ()

;;

type t = A of int * int option * int option | B;;

let not_ambiguous__constructor = function
  | A (x, Some _, _) | A (x, _, Some _) when x > 0 -> ()
  | A _ | B -> ()
;;


type amoi = Z of int | Y of int * int  | X of amoi * amoi
;;

let ambiguous__amoi a = match a with
| X (Z x,Y (y,0))
| X (Z y,Y (x,_))
 when x+y > 0 -> 0
| X _|Y _|Z _ -> 1
;;

module type S = sig val b : bool end
;;

let ambiguous__module_variable x b =  match x with
  | (module M:S),_,(1,_)
  | _,(module M:S),(_,1) when M.b && b -> 1
  | _ -> 2
;;

let not_ambiguous__module_variable x b =  match x with
  | (module M:S),_,(1,_)
  | _,(module M:S),(_,1) when b -> 1
  | _ -> 2
;;

(* Mixed case *)

type t = A of int * int | B of int * int
;;

let ambiguous_xy_but_not_ambiguous_z g = function
  | A (x as z,(0 as y))|A (0 as y as z,x)|B (x,(y as z)) when g x (y+z) -> 1
  | _ -> 2
;;
