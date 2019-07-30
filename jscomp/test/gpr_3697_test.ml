type 'a t = Fix of 'a t lazy_t

let rec fix () = Fix (lazy (fix ()))


let rec unfixLeak (Fix f) =
  unfixLeak @@ Lazy.force f


let  unfix p = 
  while true do 
    p := match !p with (Fix lazy h) -> h
  done 
(* ;; unfixLeak (fix ())   *)

(* ;; unfix (ref (fix ())) *)