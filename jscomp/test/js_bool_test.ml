

let f x = 
  if Js.to_bool x then true else false

let f2 x = 
  if Js.to_bool x then Js.true_ else Js.false_

let f4 x = 
  if  x then Js.true_ else Js.false_

let f3 = 
  if Js.to_bool Js.true_ then true else false

let u : bool = [%bs.raw{| 1|}] 

let v : Js.boolean = [%bs.raw{| true|}]

let suites = Mt.[
    "caml_bool_eq_caml_bool", (fun _ -> Eq (u, f Js.true_));
    "js_bool_eq_js_bool",(fun _ -> Eq(v, f4 true));
    "js_bool_eq_acml_bool",(fun _ -> 
        Eq( true, (f Js.true_ = [%bs.raw {|true|} ] (* not type check*))));
]

(* let f u = Js.unsafe_js_expr u *)

;; Mt.from_pair_suites __FILE__ suites
