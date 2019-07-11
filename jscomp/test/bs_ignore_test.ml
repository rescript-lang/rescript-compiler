[%%bs.raw
{|
function add(x,y){
  return x + y
}
|}]

type _ kind = Float : float kind | String : string kind

external add : ('a kind[@bs.ignore]) -> 'a -> 'a -> 'a = "add" [@@bs.val]

let () =
  Js.log (add Float 3.0 2.0) ;
  Js.log (add String "x" "y")

[%%bs.raw
{|
function add_dyn(kind,x,y){
  switch(kind){
  case "string" : return x + y;
  case "float" : return x + y;
  }
}
|}]

let string_of_kind (type t) (kind : t kind) =
  match kind with Float -> "float" | String -> "string"

external add_dyn : ('a kind[@bs.ignore]) -> string -> 'a -> 'a -> 'a
  = "add_dyn"
  [@@bs.val]

let add2 k x y = add_dyn k (string_of_kind k) x y

let () =
  Js.log (add2 Float 3.0 2.0) ;
  Js.log (add2 String "x" "y")
