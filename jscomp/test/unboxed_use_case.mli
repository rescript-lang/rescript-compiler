


type _ para

type t = H : _ para -> t 
[@@unboxed] 

type any =
  | Any : 'a -> any  [@@unboxed]

val hi : any array   