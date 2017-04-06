
type 'a t 
type error
external resolve : 'a -> 'a t = "Promise.resolve" [@@bs.val]
external catch : 
  (error -> 'a t [@bs.uncurry]) -> 'a t  = "catch" [@@bs.send.pipe: 'a t]


(** rejectXXError for the FFI .. which is similar to [bs.this] *)
let handler  = fun [@bs.exn] e -> 
  match Obj.magic e with 
  | Js.Exn.Error v -> Js.log "js error"; resolve 0
  | Not_found -> Js.log "hi"; resolve 0
  | _ -> assert false 


let f x = 
  x |> catch handler
