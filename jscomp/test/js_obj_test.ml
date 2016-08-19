


type  x = < say : int -> int >




let f  (u : x ) = u # say 32

let f_js u = u#@say 32

let suites = Mt.[
  "caml_obj", (fun _ -> Eq (33, f (object method say x = 1 + x end)));
  "js_obj", (fun _ ->
  Eq(34, f_js [%bs.obj{ say = fun [@bs]  x -> x + 2 } ]));
  "js_obj2",(fun _ -> 
  Eq(34,  [%bs.obj { say = fun [@bs]  x -> x + 2 } #@say 32 ]));
    
]

;; Mt.from_pair_suites __FILE__ suites

(* class type say = object  *)
(*     method say : int -> int *)
(* end *)
(* create real js object with [this] semantics *)
(* fun _ -> let module N =  *)
(*     struct *)
(*       external mk : say:'a -> say Js.t = ""[@@bs.obj]  *)
(*     end  *)
(*   in  *)
