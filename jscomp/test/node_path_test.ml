
external join : string array -> string = "" 
[@@bs.module "path"]  [@@bs.splice]

let () = Js.log (join [| "." ; __MODULE__  |])


(* let f x = join x *)
