class type t = object end;;
class ['a] o1 = object (self : #t as 'a) end;;
type 'a obj = ( < .. > as 'a);;
class type ['a] o2 = object ('a obj) end;;
class ['a] o3 = object (self : 'a obj) end;;
class ['a] o4 = object (self) method m = (self : 'a obj) end;;
(*
let o = object (self : 'a obj) end;;
let o = object (self) method m = (self : 'a obj) end;;
*)
