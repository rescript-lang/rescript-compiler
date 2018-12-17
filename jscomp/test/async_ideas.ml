#if 0 then
type error 

external readFile : string -> (error -> string -> unit) -> unit = "read"
[@@bs.val]

let f x =
    let [@a] (err,content) = readFile "hei" in 
    let%a (err,content) = readFile content in 
    let  (err,content) [@a] = readFile content in 
    let  (err,content)[@a] = readFile content in 
    let (err,content) = readFile content [@a] in
    let (err,content) = readFile content [@a] in
    let [%bs err, content ] = readFile content in 
    let [%bs ERROR, content ] = readFile content in 
    let [%bs? _, content ] = readFile content in 
    match readFile content with 
    | [%bs ERROR, content] -> 
        ()
    | [%bs? _, content] -> () 
(** advantages of attribute 
- could have payload, more customizations

Questions:
- How to handle and ?
*)
#end