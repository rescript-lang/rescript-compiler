type 'a vlist = [`Nil | `Cons of 'a * 'a vlist];;

let rec map f : 'a vlist -> 'b vlist = function
   | `Nil -> `Nil
   | `Cons(a, l) -> `Cons(f a [@bs], map f l)
 ;;


 let split_cases = function
   | `Nil | `Cons _ as x -> `A x
   | `Snoc _ as x -> `B x
 ;;


 type myvariant = [`Tag1 of int | `Tag2 of bool]


 let f = function
 | #myvariant -> "myvariant"
 | `Tag3 -> "Tag3";;

 let g1 = function `Tag1 _ -> "Tag1" | `Tag2 _ -> "Tag2";;


 let g = function
 | #myvariant as x -> g1 x
 | `Tag3 -> "Tag3";;

 type abc = [`A | `B | `C] ;;

 let f1 = function
 | `As -> "A"
 | #abc -> "other" ;;