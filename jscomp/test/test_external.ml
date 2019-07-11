type any

(* just pass it through -- we need find an elegant way to walk around ocaml's
   type system*)

external ( ~~ ) : 'a -> 'b = "%identity"
(** It's okay to do this in javascript, you will never get segfault*)

type document

external doc : unit -> document = "" [@@bs.val "document"]
external alert : string -> unit = "" [@@bs.val "alert"]

type v = int -> int

external f : string -> v = "x" [@@bs.val "ff"]

let xx = doc ()
let () = alert "hehha"
let b = f "x" 3
