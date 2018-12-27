
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


type 'a t 
type error
external resolve : 'a -> 'a t = "Promise.resolve" [@@bs.val]
external catch : 
  (error -> 'a t [@bs.uncurry]) -> 'a t  = "catch" [@@bs.send.pipe: 'a t]


(** rejectXXError for the FFI .. which is similar to [bs.this] *)
let handler  = fun  e -> 
  match Obj.magic e with 
  | Js.Exn.Error v -> Js.log "js error"; resolve 0
  | Not_found -> Js.log "hi"; resolve 0
  | _ -> assert false 

let myHandler : 'a . 'a -> int option = function [@bs.open]
  |  Not_found -> 1 
  | Js.Exn.Error _ -> 2   


let f x = 
  x |> catch handler


let () = 
  match Js.Json.parseExn {| 1. +  |} with
  | exception e -> 
    eq __LOC__ true 
      (Js.Option.isSomeValue 
        (fun[@bs] xxx y -> Js.Int.equal xxx y) 2 (myHandler e))
  | _ -> assert false 

;; Mt.from_pair_suites __MODULE__ !suites  