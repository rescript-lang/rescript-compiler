
type proto = {
  syntax : string option;
  imports : int; 
  file_options : int; 
  package : int; 
  messages : int; 
  enums : int; 
  extends : int;
}

let v = {
  syntax = None;
  imports = 0; 
  file_options = 0 ; 
  package  = 0; 
  messages = 0; 
  enums = 0; 
  extends = 0;
}

let uv = {
  syntax = None;
  imports = 1; 
  file_options = 0 ; 
  package  = 0; 
  messages = 0; 
  enums = 0; 
  extends = 0;
}
let u_v = {v with imports = 0}

let f g h = { (g h) with imports = 0 }

(*

module O = struct
  external object_ : Obj.t = "Object" [@@bs.val]
  let is_object : Obj.t -> bool = fun x -> (Obj.magic x)##constructor == object_
  type keys
  type key = Obj.t
  external keys : Obj.t -> keys = "Object.keys" [@@bs.val]
  external length : keys -> int = "%array_length"
  external get_key : keys -> int -> key = "%array_unsafe_get"
  external get_value : Obj.t -> key -> Obj.t = "%array_unsafe_get"
end

let o1 = [%bs.obj {x=1; y=2}]
let o2 = [%bs.obj {x=3; y=4}]
let o3 = [%bs.obj {x=3; y=4}]


let o1_is_object = O.is_object(Obj.repr o1)
let list_is_object = O.is_object(Obj.repr [1;2;3])
let () = Js.log2 "o1_is_object: " o1_is_object
let () = Js.log2 "list_is_object: " list_is_object

let keys1 = O.keys(Obj.repr o1)
let () = Js.log2 "keys1: " keys1

let cmp1 = compare o1 o2
let cmp2 = compare o2 o1
let cmp3 = compare o2 o3

let () = Js.log2 "cmp1: " cmp1
let () = Js.log2 "cmp2: " cmp2
let () = Js.log2 "cmp3: " cmp3

let () = for i = 0 to O.length keys1 - 1 do
  Js.log2 ("key" ^ string_of_int i ^ ": ") (O.get_key keys1 i);
  Js.log2 ("val" ^ string_of_int i ^ ": ") (O.get_value (Obj.repr o1) (O.get_key keys1 i))  
  done
*)

let suites = Mt.[
    "eq_with", (fun _ -> Eq (v, u_v))    
]

;; Mt.from_pair_suites __FILE__ suites 

