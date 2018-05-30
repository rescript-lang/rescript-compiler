

let f x =
  if x then true else false

let f2 x =
  if x then true else false

let f4 x =
  if  x then true else false

let f3 =
  if true then true else false

let u : bool = [%bs.raw{| 1|}]

let v : bool = [%bs.raw{| true|}]

let suites = Mt.[
    "caml_bool_eq_caml_bool", (fun _ -> Eq (u, f true));
    "js_bool_eq_js_bool",(fun _ -> Eq(v, f4 true));
    "js_bool_neq_acml_bool",(fun _ ->
        Eq( true, (f true = [%bs.raw {|true|} ] (* not type check*))));
]

let ff u =
    if u = true then 1
    else 2

let fi (x : int) y =  x = y
let fb (x : bool) y = x = y
let fadd (x : int) y = x + y
let ffadd (x : float) y = x +. y

let ss x = "xx" > x 

let bb x = 
  ( true > x, 
    true < x,
    true >= x ,
    true <= x,
    false > x ,
    false < x ,
    false >= x, 
    false <= x 
   ) 

let consts =     
  ( true && false ,
    false && false,
    true && true,
    false && true,

    true || false ,
    false || false,
    true || true,
    false || true
  )

let bool_array = [|true; false|] 
;; Mt.from_pair_suites __FILE__ suites
