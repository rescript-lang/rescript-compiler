(* Ignore OCAMLRUNPARAM=b to be reproducible *)
Printexc.record_backtrace false;;

exception A;;
type a = A;;

A;;
raise A;;
fun (A : a) -> ();;
function Not_found -> 1 | A -> 2 | _ -> 3;;
try raise A with A -> 2;;
