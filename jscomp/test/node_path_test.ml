
external join : string array -> string = "" 
[@@bs.module "path"]  [@@bs.splice]

let () = Js.log (join [| "." ; __FILE__  |])


(* let f x = join x *)
