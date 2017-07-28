(* Le sous-typage est "syntaxique" *)
fun (x : < x : int >) y z -> (y :> 'a), (x :> 'a), (z :> 'a);;
(* - : (< x : int > as 'a) -> 'a -> 'a * 'a = <fun> *)

(* Bizarrerie du typage des classes *)
class ['a] c () = object
  method f = (new c (): int c)
end and ['a] d () = object
  inherit ['a] c ()
end;;
(* class ['a] c : unit -> object constraint 'a = int method f : 'a c end *)
(* and ['a] d : unit -> object constraint 'a = int method f : 'a c end *)

(* 'a libre dans classe d *)
class ['a] c () = object
  method f (x : 'a) = ()
end and d () = object
  inherit ['a] c ()
end;;

(* Instancie #c *)
class virtual c () = object
end and ['a] d () = object
  constraint 'a = #c
  method f (x : #c) = (x#x : int)
end;;
(* class virtual c : unit -> object  end *)
(* and ['a] d : *)
(*  unit -> object constraint 'a = < x : int; .. > method f : 'a -> int end *)

class ['a] c () = object
  constraint 'a = int
end and ['a] d () = object
  constraint 'a = 'b #c
end;;
(* class ['a] c : unit -> object constraint 'a = int end
   and ['a] d : unit -> object constraint 'a = int #c end *)

(* Self en parametre *)
class ['a] c (x : 'a) = object (self : 'b)
  constraint 'a = 'b
  method f = self
end;;
new c;;
(* class ['a] c :
  'a -> object ('a) constraint 'a = < f : 'a; .. > method f : 'a end *)
(* - : ('a c as 'a) -> 'a = <fun> *)

class x () = object
  method virtual f : int
end;;
(* The class x should be virtual:  its methods f is undefined *)

(* Methode g en trop *)
class virtual c ((x : 'a): < f : int >) = object (_ : 'a) end
and virtual d x = object (_ : 'a)
  inherit c x
  method g = true
end;;

(* Contrainte non respectee *)
class ['a] c () = object
  constraint 'a = int
  method f x = (x : bool c)
end;;

(* Differentes contraintes *)
class ['a, 'b] c () = object
  constraint 'a = int -> 'c
  constraint 'b = 'a * <x : 'b> * 'c * 'd
  method f (x : 'a) (y : 'b) = ()
end;;
class ['a, 'b] d () = object
  inherit ['a, 'b] c ()
end;;

(* Contrainte non generique *)
let x = ref [];;
class ['a] c () = object
  method f = (x : 'a)
end;;

(* Abreviations *)
type 'a c = <f : 'a c; g : 'a d>
and 'a d = <f : int c>;;
type 'a c = <f : 'a c; g : 'a d>
and 'a d = <f : 'a c>;;
type 'a c = <f : 'a c>
and 'a d = <f : int c>;;
type 'a u = < x : 'a>
and 'a t = 'a t u;;
type 'a u = 'a
and 'a t = 'a t u;;
type 'a u = 'a;;
type t = t u * t u;;

type t = <x : 'a> as 'a;;
type 'a u = 'a;;
fun (x : t) (y : 'a u) -> x = y;;
fun (x : t) (y : 'a u) -> y = x;;
(* - : t -> t u -> bool = <fun> *)

(* Modules *)
module M =
  struct
    class ['a, 'b] c (x: int) (y: 'b) = object
      constraint 'a = int -> bool
      val x : float list = []
      val y = y
      method f (x : 'a) = ()
      method g = y
    end
  end;;
module M' = (M :
  sig
     class virtual ['a, 'b] c : int -> 'b -> object
       constraint 'a = int -> bool
       val x : float list
       val y : 'b
       method f : 'a -> unit
       method g : 'b
     end
   end);;
class ['a, 'b] d () y = object inherit ['a, 'b] M.c 7 y end;;
class ['a, 'b] e () y = object inherit ['a, 'b] M'.c 1 y end;;
(new M.c 3 "a")#g;;
(new d () 10)#g;;
(new e () 7.1)#g;;
open M;;
(new c 5 true)#g;;

(* #cl quand cl est fermee *)
module M = struct class ['a] c () = object method f (x : 'a) = () end end;;
module M' =
  (M : sig class ['a] c : unit -> object method f : 'a -> unit end end);;
fun x -> (x :> 'a #M.c);;
fun x -> (x :> 'a #M'.c);;
class ['a] c (x : 'b #c) = object end;;
class ['a] c (x : 'b #c) = object end;;

(* Ordre de calcul *)
class c () = object method f = 1 end and d () = object method f = 2 end;;
class e () = object inherit c () inherit d () end;;
(new e ())#f;;
class c () = object val x = - true val y = -. () end;;

class c () = object method f = 1 method g = 1 method h = 1 end;;
class d () = object method h = 2 method i = 2 method j = 2 end;;
class e () = object
  method f = 3
  inherit c ()
  method g = 3
  method i = 3
  inherit d ()
  method j = 3
end;;
let e = new e ();;
e#f, e#g, e#h, e#i, e#j;;

class c a = object val x = 1 val y = 1 val z = 1 val a = a end;;
class d b = object val z = 2 val t = 2 val u = 2 val b = b end;;
class e () = object
  val x = 3
  inherit c 5
  val y = 3
  val t = 3
  inherit d 7
  val u = 3
  method x = x
  method y = y
  method z = z
  method t = t
  method u = u
  method a = a
  method b = b
end;;
let e = new e ();;
e#x, e#y, e#z, e#t, e#u, e#a, e#b;;

class c (x : int) (y : int) = object
  val x = x
  val y = y
  method x = x
  method y = y
end;;
class d x y = object inherit c x y end;;
let c = new c 1 2 in c#x, c#y;;
let d = new d 1 2 in d#x, d#y;;

(* Parametres n'apparaissant pas dans le type de l'objet *)
class ['a] c (x : 'a) = object end;;
new c;;

(* Variables privees *)
(*
module type M = sig
  class c : unit -> object val x : int end
  class d : unit -> object inherit c val private x : int val x : bool end
end;;
class c (x : int) =
  val private mutable x = x
  method get = x
  method set y = x <- y
end;;
let c = new c 5;;
c#get;;
c#set 7; c#get;;


class c () = val x = 1 val y = 1 method c = x end;;
class d () = inherit c () val private x method d = x end;;
class e () =
  val x = 2 val y = 2 inherit d () method x = x method y = y
end;;
let e = new e () in e#x, e#y, e#c, e#d;;
*)

(* Oubli de variables dans l'interface *)
module M :
  sig
    class c : unit -> object
      method xc : int
    end
  end =
  struct
    class c () = object
      val x = 1
      method xc = x
    end
  end;;
class d () = object
  val x = 2
  method xd = x
  inherit M.c ()
end;;
let d = new d () in d#xc, d#xd;;

class virtual ['a] matrix (sz, init : int * 'a) = object
  val m = Array.make_matrix sz sz init
  method add (mtx : 'a matrix) = (mtx#m.(0).(0) : 'a)
end;;

class c () = object method m = new c () end;;
(new c ())#m;;
module M = struct class c () = object method m = new c () end end;;
(new M.c ())#m;;

type uu = A of int | B of (<leq: 'a> as 'a);;

class virtual c () = object (_ : 'a) method virtual m : 'a end;;
module S = (struct
  let f (x : #c) = x
end : sig
  val f : (#c as 'a) -> 'a
end);;
module S = (struct
  let f (x : #c) = x
end : sig
  val f : #c -> #c
end);;

module M = struct type t = int class t () = object end end;;

fun x -> (x :> < m : 'a -> 'a > as 'a);;

fun x -> (x : int -> bool :> 'a -> 'a);;
fun x -> (x : int -> bool :> int -> int);;
fun x -> (x : < > :> < .. >);;
fun x -> (x : < .. > :> < >);;

let x = ref [];;
module F(X : sig end) =
  struct type t = int let _ = (x : < m : t> list ref) end;;
x;;

type 'a t;;
fun (x : 'a t as 'a) -> ();;
fun (x : 'a t) -> (x : 'a); ();;
type 'a t = < x : 'a >;;
fun (x : 'a t as 'a) -> ();;
fun (x : 'a t) -> (x : 'a); ();;

class ['a] c () = object
  constraint 'a = < .. > -> unit
  method m = (fun x -> () : 'a)
end;;
class ['a] c () = object
  constraint 'a = unit -> < .. >
  method m (f : 'a) = f ()
end;;

class c () = object (self)
  method private m = 1
  method n = self#m
end;;

class d () = object (self)
  inherit c ()
  method o = self#m
end;;

let x = new d () in x#n, x#o;;

class c () = object method virtual m : int method private m = 1 end;;

(* Marshaling (cf. PR#5436) *)

let r = ref 0;;
let id o = Oo.id o - !r;;
r := Oo.id (object end);;
id (object end);;
id (object end);;
let o = object end in
  let s = Marshal.to_string o [] in
  let o' : < > = Marshal.from_string s 0 in
  let o'' : < > = Marshal.from_string s 0 in
  (id o, id o', id o'');;

let o = object val x = 33 method m = x end in
  let s = Marshal.to_string o [Marshal.Closures] in
  let o' : <m:int> = Marshal.from_string s 0 in
  let o'' : <m:int> = Marshal.from_string s 0 in
  (id o, id o', id o'', o#m, o'#m);;

let o = object val x = 33 val y = 44 method m = x end in
  let s = Marshal.to_string (o,o) [Marshal.Closures] in
  let (o1, o2) : (<m:int> * <m:int>) = Marshal.from_string s 0 in
  let (o3, o4) : (<m:int> * <m:int>) = Marshal.from_string s 0 in
  (id o, id o1, id o2, id o3, id o4, o#m, o1#m);;

(* Recursion (cf. PR#5291) *)

class a = let _ = new b in object end
and b = let _ = new a in object end;;

class a = let _ = new a in object end;;
