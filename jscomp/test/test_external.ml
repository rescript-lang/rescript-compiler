type any (* just pass it through -- 
            we need find an elegant way to walk around ocaml's type system*)
external (~~) : 'a -> 'b = "%identity" 
(** It's okay to do this in javascript, you will never get segfault*)

type document
external doc : unit -> document = "" [@@js.call "document"]
external alert : string -> unit = "" [@@js.call "alert"]
let xx = doc ()

let () = alert "hehha"
