

external map : 'a array -> ('a -> 'b [@bs])  -> 'b array = "map" [@@bs.send]
external mapi : 'a array -> ('a -> int -> 'b [@bs])  -> 'b array = "map" [@@bs.send]

external parseInt : string -> int = "parseInt" [@@bs.val]
external parseInt_radix : string -> int -> int = "parseInt" [@@bs.val]

let f v = 
  if v mod 2 = 0 then 
    fun v -> v * v 
  else  fun v -> v + v 

let v  = mapi [|1;2;3 |] (Js_unsafe.js_fn_mk2 f)

let vv  = mapi [|1;2;3 |] (Js_unsafe.js_fn_mk2 (+))

let hh = map [|"1";"2";"3"|] (Js_unsafe.js_fn_mk1 parseInt)

let u = Js_unsafe.js_fn_mk0 (fun _ -> 3)

;; Mt.from_pair_suites __FILE__ Mt.[
    __LOC__, (fun _ -> Eq(v, [|0; 1;  4 |] ));
    __LOC__, (fun _ -> Eq(vv, [|1;3;5|]));
    __LOC__, (fun _ -> Eq(hh, [|1;2;3|]));
    __LOC__, (fun _ -> Eq(  
         
         map (map [| 1;2;3|]  (Js_unsafe.js_fn_mk1 (fun x -> fun y -> x + y))) 
          (Js_unsafe.js_fn_mk1 @@ fun y -> (y 0)  * (y 1) ), [|2; 6 ; 12|]
      ));
    __LOC__, (fun _ -> Eq(
        mapi [|1;2;3|] (Js_unsafe.js_fn_mk2 (fun x  -> let y =  x * x in fun i -> y + i )), 
        [|1; 5 ; 11|]        
      ))
]


