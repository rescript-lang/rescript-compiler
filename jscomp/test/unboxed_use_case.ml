

type r = { f : 'a . ('a -> 'a ) } [@@unboxed]


let map_pair r (p1, p2) = (r.f p1, r.f p2)


let u = { f = fun x -> x }


let sample = map_pair u (3, true)


type _ para = C
type t = H : _ para -> t [@@unboxed]


type any =
  | Any : 'a -> any   [@@unboxed]


let hi =  [|Any 3; Any 2 ; Any "x"|]

let dump (any : any) = match any with Any v -> Js.log v ;;

let () = 
    dump (Any 3);
    dump (Any "x")


(* https://inbox.ocaml.org/caml-list/CAAxsn=HhhmAAYfSCLzWgMW0Q-duTZNQBLQYDx8yETwWTjm16tw@mail.gmail.com/t/#u *)
type ('a, 'b) t3 = 
    [`A of (int, string) t3_aux
    | `B ]
and ('a,'b)  t3_aux = {
  field : ('a,'b) t3
} [@@ocaml.unboxed]
;;


let rec v0 : _ t3 = `A {field = v0 }
let rec v1 : _ t3 = `A {field = `B }

(* 
module rec R:
   sig
     class ['a] container : 'a ->
       object
         method map : 'b. ('a -> 'b) -> 'b R.container_aux
       end
     type 'a container_aux = 
        { container: 'a container }
        [@@unboxed]
   end =
   struct
     class ['a] container (v:'a) = object
       method map : 'b. ('a -> 'b) -> 'b R.container_aux =
         fun f -> { R.container = new R.container (f v) }
     end
     type 'a container_aux = 
        { container: 'a container }
        [@@unboxed]
   end *)


module rec R1 : sig 
   class type ['a] container =
   object 
     method map : 'b. ('a -> 'b) -> 'b R1.container_aux 
   end 
 type 'a container_aux = 
    { container: 'a container }
    [@@unboxed]
end = R1 

let f ( x: int R1.container ) = 
  (* let u = (x##map) in  *)
  x##map (fun x -> string_of_int x) 