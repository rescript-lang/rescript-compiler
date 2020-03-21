
let obj = object [@bs]
  method hi  a b = a + b
  method say a b = a - b
  method xx a b = a - b
end

let f x (a:int) (b:int) = x##hi a b 

let h = obj##hi


;; f obj 3 4 |. Js.log



let x h = h##raw ~x:0 ~y:0

class type pro = object [@bs]
  method exit : code:int -> unit
end

let f1 (u : pro Js.t) = u##exit ~code:2

(* let obj3 =
  let module J =
    struct
      external unsafe_expr :
        hi:((< hi: (name:'hi0 -> age:'hi1 -> 'hi) Js_OO.Meth.arity2   >  Js.t
               as 'self_type)
              -> name:'hi0 -> age:'hi1 -> 'hi)
          Js_OO.Callback.arity3 ->
          < hi: (name:'hi0 -> age:'hi1 -> 'hi) Js_OO.Meth.arity2   >  Js.t =
          ""
          "\132\149\166\190\000\000\000\t\000\000\000\005\000\000\000\012\000\000\000\012\145\160\160A\144\"hi@"
    end in
    J.unsafe_expr
      ~hi:(
                 (fun [@bs.this] _ -> fun ~name -> fun ~age -> Js.log name)
             ) *)
let obj3 = object [@bs](self)
  method hi ~(name : string) ~(age : int) = name |. Js.log
  method hh () =  self##hi ~name:"x" ~age:20
end 
(* TODO: not supported yet
 *)
 (* Type < hi : name:string -> age:int -> 'b; .. > as 'a
 is not compatible with type
   < hh : (unit -> 'c [@bs.meth]);
     hi : (name:string -> age:int -> unit [@bs.meth]) >
   Js.t  *)
type add_meth = int -> int -> int [@bs.meth] 

let obj2 : <
  hi : add_meth;
  say : add_meth;
  xx : add_meth
> Js.t = object [@bs] (self : 'a)
  method hi  a b = a + b
  method say a b = self##hi a  b - 1
  method xx a b = a - b
end 