module rec A : sig 
  val even : int -> bool 
end = struct 
  let  even  n = 
    if n = 0 then true
    else if n = 1 then false 
    else B.odd (n - 1)
end 
and B : sig 
  val odd : int -> bool 
end = struct 
  let odd n = 
    if n = 1 then true 
    else if n = 0 then false 
    else A.even (n - 1)
end

module rec AA : sig 
  val even : int -> bool
  val x : unit -> int  
end = struct 
  let  even  n = 
    if n = 0 then true
    else if  n = 1 then false
    else BB.odd (n - 1)
  let x () =  BB.y () + 3
end 
and BB : sig 
  val odd : int -> bool 
  val y : unit -> int 
end = struct 
  let odd n = 
    if n = 1 then true 
    else if n = 0 then false
    else AA.even (n - 1)
  let y () = 32
end


module rec Even : sig 
  type t = Zero | Succ of Odd.t 
end = struct 
  type t = Zero | Succ of Odd.t 
end 
and Odd : sig 
  type t = Succ of Even.t 
end = struct 
  type t = Succ of Even.t 
end 

module rec AAA : sig
  type t = Leaf of string | Node of ASet.t
  val compare: t -> t -> int
end
= struct
  type t = Leaf of string | Node of ASet.t
  let compare t1 t2 =
    match (t1, t2) with
      (Leaf s1, Leaf s2) -> Pervasives.compare s1 s2
    | (Leaf _, Node _) -> 1
    | (Node _, Leaf _) -> -1
    | (Node n1, Node n2) -> ASet.compare n1 n2
end
and ASet : Set.S with type elt = AAA.t
  = Set.Make(AAA)



let suites  = Mt.[
    "test1", (fun _ -> Eq ( (true,true,false,false), (A.even 2, AA.even 4, B.odd 2, BB.odd 4  )));
    "test2", (fun _ -> Eq( (BB.y (), 32))) ;
    "test3", (fun _ -> Eq(AA.x (), 35));
    "test4", (fun _ -> Eq ( true, A.even 2 ));
    "test4", (fun _ -> Eq ( true, AA.even 4 ));
    "test5", (fun _ -> Eq(false, B.odd 2));
    "test6", (fun _ -> Eq(2 ,ASet.cardinal (ASet.of_list [Leaf "a" ; Leaf "b" ; Leaf "a"])))
];;

Mt.from_pair_suites __MODULE__ suites

