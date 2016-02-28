

let f x = 
  if Js.to_bool x then true else false

let f2 x = 
  if Js.to_bool x then Js.true_ else Js.false_

let f4 x = 
  if  x then Js.true_ else Js.false_

let f3 = 
  if Js.to_bool Js.true_ then true else false

let suites = Mt.[
    "caml_bool_eq_caml_bool", (fun _ -> Eq ((Js.unsafe_js_expr "1" : bool), f Js.true_));
    "js_bool_eq_js_bool",(fun _ -> Eq((Js.unsafe_js_expr "true" : Js.boolean), f4 true));
    "js_bool_neq_acml_bool",(fun _ -> 
        Eq( false, (f Js.true_ = Js.unsafe_js_expr "true" (* not type check*))));
]

(* let f u = Js.unsafe_js_expr u *)

;; Mt.from_pair_suites __FILE__ suites
