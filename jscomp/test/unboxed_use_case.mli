


type _ para

type t = H : _ para -> t 
[@@unboxed] 

type any =
  | Any : 'a -> any  [@@unboxed]

val hi : any array   


(* https://inbox.ocaml.org/caml-list/CAAxsn=HhhmAAYfSCLzWgMW0Q-duTZNQBLQYDx8yETwWTjm16tw@mail.gmail.com/t/#u *)
type ('a, 'b) t3 = 
  [`A of (int, string) t3_aux
  | `B ]
and ('a,'b)  t3_aux = {
  field : ('a,'b) t3
} [@@ocaml.unboxed]
;;

val v0 : ('a,'b) t3 
val v1 : ('a, 'b) t3