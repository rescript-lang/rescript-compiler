let keys :  Obj.t -> string array [@bs] = [%bs.raw " function (x){return Object.keys(x)}" ]



[%%bs.raw{|
  function $$high_order(x){
   return function(y,z){
      return x + y + z 
   }
  }
|}]
external high_order: int -> (int -> int -> int  [@bs]) = "$$high_order" [@@bs.val]

let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc (x, y) = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

type _ kind = 
  | Int : int kind 
  | Str : string kind 

external config : kind:('a kind [@bs.ignore] ) -> hi:int -> low:'a ->  _ = "" [@@bs.obj]

let int_config = config ~kind:Int ~hi:3  ~low:32

let string_config = config ~kind:Str ~hi:3  ~low:"32"

let () = 
  eq __LOC__ (6, ((high_order 1 ) 2 3 [@bs]))


let same_type = 
  ([int_config; [%obj{hi= 3 ; low = 32}]],
   [string_config ; [%obj{hi = 3 ; low = "32"}]]
  )

let v_obj = object method hi__x () = Js.log "hei" end [@bs]


let () = 
  eq __LOC__ (Array.length (Js_obj.keys int_config), 2 );
  eq __LOC__ (Array.length (Js_obj.keys string_config), 2 );
  eq __LOC__ (Js_obj.keys v_obj |> Js.Array.indexOf "hi_x" , -1 );
  eq __LOC__ (Js_obj.keys v_obj |> Js.Array.indexOf "hi", 0 )

let u = ref 3 

let side_effect_config = config ~kind:(incr u; Int) ~hi:3 ~low:32

let () = 
  eq __LOC__ (!u, 4)

type null_obj

external hh : null_obj   -> int = "" [@@bs.send] (* it also work *)
external ff : null_obj -> unit  -> int = "" [@@bs.send]
external ff_pipe :  unit  -> int = "" [@@bs.send.pipe: null_obj]
external ff_pipe2 :   int = "" [@@bs.send.pipe: null_obj]
let vv z = hh z 

let v z = ff z ()

let vvv z = z |> ff_pipe ()

let vvvv z = z |> ff_pipe2 
let create_prim () =  [%obj{ x' = 3 ; x'' = 3; x'''' = 2}]

let () = Mt.from_pair_suites __FILE__ !suites
