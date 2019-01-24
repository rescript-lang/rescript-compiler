(*
   Polymorphic methods are now available in the main branch.
   Enjoy.
*)

(* Tests for explicit polymorphism *)
open StdLabels;;

type 'a t = { t : 'a };;
type 'a fold = { fold : 'b. f:('b -> 'a -> 'b) -> init:'b -> 'b };;
let f l = { fold = List.fold_left l };;
(f [1;2;3]).fold ~f:(+) ~init:0;;
[%%expect {|
type 'a t = { t : 'a; }
type 'a fold = { fold : 'b. f:('b -> 'a -> 'b) -> init:'b -> 'b; }
val f : 'a list -> 'a fold = <fun>
- : int = 6
|}];;

class ['b] ilist l = object
  val l = l
  method add x = {< l = x :: l >}
  method fold : 'a. f:('a -> 'b -> 'a) -> init:'a -> 'a =
    List.fold_left l
end
;;
[%%expect {|
class ['b] ilist :
  'b list ->
  object ('c)
    val l : 'b list
    method add : 'b -> 'c
    method fold : f:('a -> 'b -> 'a) -> init:'a -> 'a
  end
|}];;

class virtual ['a] vlist = object (_ : 'self)
  method virtual add : 'a -> 'self
  method virtual fold : 'b. f:('b -> 'a -> 'b) -> init:'b -> 'b
end
;;
[%%expect {|
class virtual ['a] vlist :
  object ('c)
    method virtual add : 'a -> 'c
    method virtual fold : f:('b -> 'a -> 'b) -> init:'b -> 'b
  end
|}];;

class ilist2 l = object
  inherit [int] vlist
  val l = l
  method add x = {< l = x :: l >}
  method fold = List.fold_left l
end
;;
[%%expect {|
class ilist2 :
  int list ->
  object ('a)
    val l : int list
    method add : int -> 'a
    method fold : f:('b -> int -> 'b) -> init:'b -> 'b
  end
|}];;

let ilist2 l = object
  inherit [_] vlist
  val l = l
  method add x = {< l = x :: l >}
  method fold = List.fold_left l
end
;;
[%%expect {|
val ilist2 : 'a list -> 'a vlist = <fun>
|}];;

class ['a] ilist3 l = object
  inherit ['a] vlist
  val l = l
  method add x = {< l = x :: l >}
  method fold = List.fold_left l
end
;;
[%%expect {|
class ['a] ilist3 :
  'a list ->
  object ('c)
    val l : 'a list
    method add : 'a -> 'c
    method fold : f:('b -> 'a -> 'b) -> init:'b -> 'b
  end
|}];;

class ['a] ilist4 (l : 'a list) = object
  val l = l
  method virtual add : _
  method add x = {< l = x :: l >}
  method virtual fold : 'b. f:('b -> 'a -> 'b) -> init:'b -> 'b
  method fold = List.fold_left l
end
;;
[%%expect {|
class ['a] ilist4 :
  'a list ->
  object ('c)
    val l : 'a list
    method add : 'a -> 'c
    method fold : f:('b -> 'a -> 'b) -> init:'b -> 'b
  end
|}];;

class ['a] ilist5 (l : 'a list) = object (self)
  val l = l
  method add x = {< l = x :: l >}
  method virtual fold : 'b. f:('b -> 'a -> 'b) -> init:'b -> 'b
  method virtual fold2 : 'b. f:('b -> 'a -> 'b) -> init:'b -> 'b
  method fold2 ~f ~init = self#fold ~f ~init:(self#fold ~f ~init)
  method fold = List.fold_left l
end
;;
[%%expect {|
class ['a] ilist5 :
  'a list ->
  object ('c)
    val l : 'a list
    method add : 'a -> 'c
    method fold : f:('b -> 'a -> 'b) -> init:'b -> 'b
    method fold2 : f:('b -> 'a -> 'b) -> init:'b -> 'b
  end
|}];;

class ['a] ilist6 l = object (self)
  inherit ['a] vlist
  val l = l
  method add x = {< l = x :: l >}
  method virtual fold2 : 'b. f:('b -> 'a -> 'b) -> init:'b -> 'b
  method fold2 ~f ~init = self#fold ~f ~init:(self#fold ~f ~init)
  method fold = List.fold_left l
end
;;
[%%expect {|
class ['a] ilist6 :
  'a list ->
  object ('c)
    val l : 'a list
    method add : 'a -> 'c
    method fold : f:('b -> 'a -> 'b) -> init:'b -> 'b
    method fold2 : f:('b -> 'a -> 'b) -> init:'b -> 'b
  end
|}];;

class virtual ['a] olist = object
  method virtual fold : 'c. f:('a -> 'c -> 'c) -> init:'c -> 'c
end
;;
[%%expect {|
class virtual ['a] olist :
  object method virtual fold : f:('a -> 'c -> 'c) -> init:'c -> 'c end
|}];;

class ['a] onil = object
  inherit ['a] olist
  method fold ~f ~init = init
end
;;
[%%expect {|
class ['a] onil :
  object method fold : f:('a -> 'c -> 'c) -> init:'c -> 'c end
|}];;

class ['a] ocons ~hd ~tl = object (_ : 'b)
  inherit ['a] olist
  val hd : 'a = hd
  val tl : 'a olist = tl
  method fold ~f ~init = f hd (tl#fold ~f ~init)
end
;;
[%%expect {|
class ['a] ocons :
  hd:'a ->
  tl:'a olist ->
  object
    val hd : 'a
    val tl : 'a olist
    method fold : f:('a -> 'c -> 'c) -> init:'c -> 'c
  end
|}];;

class ['a] ostream ~hd ~tl = object (_ : 'b)
  inherit ['a] olist
  val hd : 'a = hd
  val tl : _ #olist = (tl : 'a ostream)
  method fold ~f ~init = f hd (tl#fold ~f ~init)
  method empty = false
end
;;
[%%expect {|
class ['a] ostream :
  hd:'a ->
  tl:'a ostream ->
  object
    val hd : 'a
    val tl : < empty : bool; fold : 'c. f:('a -> 'c -> 'c) -> init:'c -> 'c >
    method empty : bool
    method fold : f:('a -> 'c -> 'c) -> init:'c -> 'c
  end
|}];;

class ['a] ostream1 ~hd ~tl = object (self : 'b)
  inherit ['a] olist
  val hd = hd
  val tl : 'b = tl
  method hd = hd
  method tl = tl
  method fold ~f ~init =
    self#tl#fold ~f ~init:(f self#hd init)
end
[%%expect {|
class ['a] ostream1 :
  hd:'a ->
  tl:'b ->
  object ('b)
    val hd : 'a
    val tl : 'b
    method fold : f:('a -> 'c -> 'c) -> init:'c -> 'c
    method hd : 'a
    method tl : 'b
  end
|}, Principal{|
Line _, characters 4-16:
Warning 18: this use of a polymorphic method is not principal.
class ['a] ostream1 :
  hd:'a ->
  tl:'b ->
  object ('b)
    val hd : 'a
    val tl : 'b
    method fold : f:('a -> 'c -> 'c) -> init:'c -> 'c
    method hd : 'a
    method tl : 'b
  end
|}];;

class vari = object
  method virtual m : 'a. ([< `A|`B|`C] as 'a) -> int
  method m = function `A -> 1 | `B|`C  -> 0
end
;;
[%%expect {|
class vari : object method m : [< `A | `B | `C ] -> int end
|}];;

class vari = object
  method m : 'a. ([< `A|`B|`C] as 'a) -> int = function `A -> 1 | `B|`C -> 0
end
;;
[%%expect {|
class vari : object method m : [< `A | `B | `C ] -> int end
|}];;

module V =
  struct
    type v = [`A | `B | `C]
    let m : [< v] -> int = function `A -> 1 | #v -> 0
  end
;;
[%%expect {|
module V : sig type v = [ `A | `B | `C ] val m : [< v ] -> int end
|}];;

class varj = object
  method virtual m : 'a. ([< V.v] as 'a) -> int
  method m = V.m
end
;;
[%%expect {|
class varj : object method m : [< V.v ] -> int end
|}];;


module type T = sig
  class vari : object method m : 'a. ([< `A | `B | `C] as 'a) -> int end
end
;;
[%%expect {|
module type T =
  sig class vari : object method m : [< `A | `B | `C ] -> int end end
|}];;

module M0 = struct
  class vari = object
    method virtual m : 'a. ([< `A|`B|`C] as 'a) -> int
    method m = function `A -> 1 | `B|`C -> 0
  end
end
;;
[%%expect {|
module M0 :
  sig class vari : object method m : [< `A | `B | `C ] -> int end end
|}];;

module M : T = M0
;;
[%%expect {|
module M : T
|}];;

let v = new M.vari;;
[%%expect {|
val v : M.vari = <obj>
|}];;

v#m `A;;
[%%expect {|
- : int = 1
|}];;


class point ~x ~y = object
  val x : int = x
  val y : int = y
  method x = x
  method y = y
end
;;
[%%expect {|
class point :
  x:int ->
  y:int -> object val x : int val y : int method x : int method y : int end
|}];;

class color_point ~x ~y ~color = object
  inherit point ~x ~y
  val color : string = color
  method color = color
end
;;
[%%expect {|
class color_point :
  x:int ->
  y:int ->
  color:string ->
  object
    val color : string
    val x : int
    val y : int
    method color : string
    method x : int
    method y : int
  end
|}];;

class circle (p : #point) ~r = object
  val p = (p :> point)
  val r = r
  method virtual distance : 'a. (#point as 'a) -> float
  method distance p' =
    let dx = p#x - p'#x and dy = p#y - p'#y in
    let d = sqrt (float (dx * dx + dy * dy)) -. float r in
    if d < 0. then 0. else d
end
;;
[%%expect {|
class circle :
  #point ->
  r:int ->
  object val p : point val r : int method distance : #point -> float end
|}];;

let p0 = new point ~x:3 ~y:5
let p1 = new point ~x:10 ~y:13
let cp = new color_point ~x:12 ~y:(-5) ~color:"green"
let c = new circle p0 ~r:2
let d = floor (c#distance cp)
;;
let f (x : < m : 'a. 'a -> 'a >) = (x : < m : 'b. 'b -> 'b >)
;;
let f (x : < m : 'a. 'a -> 'a list >) = (x : < m : 'b. 'b -> 'c >)
;;
[%%expect {|
val p0 : point = <obj>
val p1 : point = <obj>
val cp : color_point = <obj>
val c : circle = <obj>
val d : float = 11.
val f : < m : 'a. 'a -> 'a > -> < m : 'b. 'b -> 'b > = <fun>
Line _, characters 41-42:
Error: This expression has type < m : 'b. 'b -> 'b list >
       but an expression was expected of type < m : 'b. 'b -> 'c >
       The universal variable 'b would escape its scope
|}];;

class id = object
  method virtual id : 'a. 'a -> 'a
  method id x = x
end
;;
[%%expect {|
class id : object method id : 'a -> 'a end
|}];;

class type id_spec = object
  method id : 'a -> 'a
end
;;
[%%expect {|
class type id_spec = object method id : 'a -> 'a end
|}];;

class id_impl = object (_ : #id_spec)
  method id x = x
end
;;
[%%expect {|
class id_impl : object method id : 'a -> 'a end
|}];;

class a = object
  method m = (new b : id_spec)#id true
end
and b = object (_ : #id_spec)
  method id x = x
end
;;
[%%expect {|
class a : object method m : bool end
and b : object method id : 'a -> 'a end
|}];;


class ['a] id1 = object
  method virtual id : 'b. 'b -> 'a
  method id x = x
end
;;
[%%expect {|
Line _, characters 12-17:
Error: This method has type 'a -> 'a which is less general than 'b. 'b -> 'a
|}];;

class id2 (x : 'a) = object
  method virtual id : 'b. 'b -> 'a
  method id x = x
end
;;
[%%expect {|
Line _, characters 12-17:
Error: This method has type 'a -> 'a which is less general than 'b. 'b -> 'a
|}];;

class id3 x = object
  val x = x
  method virtual id : 'a. 'a -> 'a
  method id _ = x
end
;;
[%%expect {|
Line _, characters 12-17:
Error: This method has type 'b -> 'b which is less general than 'a. 'a -> 'a
|}];;

class id4 () = object
  val mutable r = None
  method virtual id : 'a. 'a -> 'a
  method id x =
    match r with
      None -> r <- Some x; x
    | Some y -> y
end
;;
[%%expect {|
Line _, characters 12-79:
Error: This method has type 'b -> 'b which is less general than 'a. 'a -> 'a
|}];;

class c = object
  method virtual m : 'a 'b. 'a -> 'b -> 'a
  method m x y = x
end
;;
[%%expect {|
class c : object method m : 'a -> 'b -> 'a end
|}];;


let f1 (f : id) = f#id 1, f#id true
;;
let f2 f = (f : id)#id 1, (f : id)#id true
;;
let f3 f = f#id 1, f#id true
;;
let f4 f = ignore(f : id); f#id 1, f#id true
;;
[%%expect {|
val f1 : id -> int * bool = <fun>
val f2 : id -> int * bool = <fun>
Line _, characters 24-28:
Error: This expression has type bool but an expression was expected of type
         int
|}];;

class c = object
  method virtual m : 'a. (#id as 'a) -> int * bool
  method m (f : #id) = f#id 1, f#id true
end
;;
[%%expect {|
class c : object method m : #id -> int * bool end
|}];;


class id2 = object (_ : 'b)
  method virtual id : 'a. 'a -> 'a
  method id x = x
  method mono (x : int) = x
end
;;
let app = new c #m (new id2)
;;
type 'a foo = 'a foo list
;;
[%%expect {|
class id2 : object method id : 'a -> 'a method mono : int -> int end
val app : int * bool = (1, true)
Line _, characters 0-25:
Error: The type abbreviation foo is cyclic
|}];;

class ['a] bar (x : 'a) = object end
;;
type 'a foo = 'a foo bar
;;
[%%expect {|
class ['a] bar : 'a -> object  end
type 'a foo = 'a foo bar
|}];;

fun x -> (x : < m : 'a. 'a * 'b > as 'b)#m;;
fun x -> (x : < m : 'a. 'b * 'a list> as 'b)#m;;
let f x = (x : < m : 'a. 'b * (< n : 'a; .. > as 'a) > as 'b)#m;;
fun (x : < p : 'a. < m : 'a ; n : 'b ; .. > as 'a > as 'b) -> x#p;;
fun (x : <m:'a. 'a * <p:'b. 'b * 'c * 'd> as 'c> as 'd) -> x#m;;
(* printer is wrong on the next (no official syntax) *)
fun (x : <m:'a.<p:'a;..> >) -> x#m;;
[%%expect {|
- : (< m : 'a. 'a * 'b > as 'b) -> 'c * 'b = <fun>
- : (< m : 'a. 'b * 'a list > as 'b) -> 'b * 'c list = <fun>
val f :
  (< m : 'b. 'a * (< n : 'b; .. > as 'b) > as 'a) ->
  'a * (< n : 'c; .. > as 'c) = <fun>
- : (< p : 'b. < m : 'b; n : 'a; .. > as 'b > as 'a) ->
    (< m : 'c; n : 'a; .. > as 'c)
= <fun>
- : (< m : 'a. 'a * < p : 'b. 'b * 'd * 'c > as 'd > as 'c) ->
    ('f * < p : 'b. 'b * 'e * 'c > as 'e)
= <fun>
- : < m : 'a. < p : 'a; .. > as 'b > -> 'b = <fun>
|}, Principal{|
- : (< m : 'a. 'a * 'b > as 'b) -> 'c * (< m : 'a. 'a * 'd > as 'd) = <fun>
- : (< m : 'a. 'b * 'a list > as 'b) ->
    (< m : 'a. 'c * 'a list > as 'c) * 'd list
= <fun>
val f :
  (< m : 'b. 'a * (< n : 'b; .. > as 'b) > as 'a) ->
  (< m : 'd. 'c * (< n : 'd; .. > as 'd) > as 'c) * (< n : 'e; .. > as 'e) =
  <fun>
- : (< p : 'b. < m : 'b; n : 'a; .. > as 'b > as 'a) ->
    (< m : 'c; n : < p : 'e. < m : 'e; n : 'd; .. > as 'e > as 'd; .. > as 'c)
= <fun>
- : (< m : 'a. 'a * < p : 'b. 'b * 'd * 'c > as 'd > as 'c) ->
    ('f *
     < p : 'b.
             'b * 'e *
             (< m : 'a. 'a * < p : 'b0. 'b0 * 'h * 'g > as 'h > as 'g) >
     as 'e)
= <fun>
- : < m : 'a. < p : 'a; .. > as 'b > -> 'b = <fun>
|}];;

type sum = T of < id: 'a. 'a -> 'a > ;;
fun (T x) -> x#id;;
[%%expect {|
type sum = T of < id : 'a. 'a -> 'a >
- : sum -> 'a -> 'a = <fun>
|}];;

type record = { r: < id: 'a. 'a -> 'a > } ;;
fun x -> x.r#id;;
fun {r=x} -> x#id;;
[%%expect {|
type record = { r : < id : 'a. 'a -> 'a >; }
- : record -> 'a -> 'a = <fun>
- : record -> 'a -> 'a = <fun>
|}];;

class myself = object (self)
  method self : 'a. 'a -> 'b = fun _ -> self
end;;
[%%expect {|
class myself : object ('b) method self : 'a -> 'b end
|}];;

class number = object (self : 'self)
  val num = 0
  method num = num
  method succ = {< num = num + 1 >}
  method prev =
    self#switch ~zero:(fun () -> failwith "zero") ~prev:(fun x -> x)
  method switch : 'a. zero:(unit -> 'a) -> prev:('self -> 'a) -> 'a =
    fun ~zero ~prev ->
      if num = 0 then zero () else prev {< num = num - 1 >}
end
;;
[%%expect {|
class number :
  object ('b)
    val num : int
    method num : int
    method prev : 'b
    method succ : 'b
    method switch : zero:(unit -> 'a) -> prev:('b -> 'a) -> 'a
  end
|}];;

let id x = x
;;
class c = object
  method id : 'a. 'a -> 'a = id
end
;;
class c' = object
  inherit c
  method id = id
end
;;
class d = object
  inherit c as c
  val mutable count = 0
  method id x = count <- count+1; x
  method count = count
  method old : 'a. 'a -> 'a = c#id
end
;;
class ['a] olist l = object
  val l = l
  method fold : 'b. f:('a -> 'b -> 'b) -> init:'b -> 'b
      = List.fold_right l
  method cons a = {< l = a :: l >}
end
;;
let sum (l : 'a #olist) = l#fold ~f:(fun x acc -> x+acc) ~init:0
;;
let count (l : 'a #olist) = l#fold ~f:(fun _ acc -> acc+1) ~init:0
;;
let append (l : 'a #olist) (l' : 'b #olist) =
  l#fold ~init:l' ~f:(fun x acc -> acc#cons x)
;;
[%%expect {|
val id : 'a -> 'a = <fun>
class c : object method id : 'a -> 'a end
class c' : object method id : 'a -> 'a end
class d :
  object
    val mutable count : int
    method count : int
    method id : 'a -> 'a
    method old : 'a -> 'a
  end
class ['a] olist :
  'a list ->
  object ('c)
    val l : 'a list
    method cons : 'a -> 'c
    method fold : f:('a -> 'b -> 'b) -> init:'b -> 'b
  end
val sum : int #olist -> int = <fun>
val count : 'a #olist -> int = <fun>
val append : 'a #olist -> ('a #olist as 'b) -> 'b = <fun>
|}];;

type 'a t = unit
;;
class o = object method x : 'a. ([> `A] as 'a) t -> unit = fun _ -> () end
;;
[%%expect {|
type 'a t = unit
class o : object method x : [> `A ] t -> unit end
|}];;

class c = object method m = new d () end and d ?(x=0) () = object end;;
class d ?(x=0) () = object end and c = object method m = new d () end;;
[%%expect {|
class c : object method m : d end
and d : ?x:int -> unit -> object  end
class d : ?x:int -> unit -> object  end
and c : object method m : d end
|}];;

class type numeral = object method fold : ('a -> 'a) -> 'a -> 'a end
class zero = object (_ : #numeral) method fold f x = x end
class next (n : #numeral) =
  object (_ : #numeral) method fold f x = n#fold f (f x) end
;;
[%%expect {|
class type numeral = object method fold : ('a -> 'a) -> 'a -> 'a end
class zero : object method fold : ('a -> 'a) -> 'a -> 'a end
class next : #numeral -> object method fold : ('a -> 'a) -> 'a -> 'a end
|}];;

class type node_type =  object
  method as_variant : [> `Node of node_type]
end;;
class node : node_type = object (self)
  method as_variant : 'a. [> `Node of node_type] as 'a
                    = `Node (self :>  node_type)
end;;
class node = object (self : #node_type)
  method as_variant = `Node (self :> node_type)
end;;
[%%expect {|
class type node_type = object method as_variant : [> `Node of node_type ] end
class node : node_type
class node : object method as_variant : [> `Node of node_type ] end
|}];;

type bad = {bad : 'a. 'a option ref};;
let bad = {bad = ref None};;
type bad2 = {mutable bad2 : 'a. 'a option ref option};;
let bad2 = {bad2 = None};;
bad2.bad2 <- Some (ref None);;
[%%expect {|
type bad = { bad : 'a. 'a option ref; }
Line _, characters 17-25:
Error: This field value has type 'b option ref which is less general than
         'a. 'a option ref
|}];;

(* Type variable scope *)

let f (x: <m:'a.<p: 'a * 'b> as 'b>) (y : 'b) = ();;
let f (x: <m:'a. 'a * (<p:int*'b> as 'b)>) (y : 'b) = ();;
[%%expect {|
val f : < m : 'a. < p : 'a * 'c > as 'c > -> 'b -> unit = <fun>
val f : < m : 'a. 'a * (< p : int * 'b > as 'b) > -> 'b -> unit = <fun>
|}, Principal{|
val f : < m : 'a. < p : 'a * 'c > as 'c > -> 'b -> unit = <fun>
val f :
  < m : 'a. 'a * (< p : int * 'b > as 'b) > ->
  (< p : int * 'c > as 'c) -> unit = <fun>
|}];;

(* PR#1374 *)

type 'a t= [`A of 'a];;
class c = object (self)
  method m :  'a. ([> 'a t] as 'a) -> unit
    = fun x -> self#m x
end;;
class c = object (self)
  method m : 'a. ([> 'a t] as 'a) -> unit = function
    | `A x' -> self#m x'
    | _ -> failwith "c#m"
end;;
class c = object (self)
  method m :  'a. ([> 'a t] as 'a) -> 'a = fun x -> self#m x
end;;
[%%expect {|
type 'a t = [ `A of 'a ]
class c : object method m : ([> 'a t ] as 'a) -> unit end
class c : object method m : ([> 'a t ] as 'a) -> unit end
class c : object method m : ([> 'a t ] as 'a) -> 'a end
|}];;

(* use before instancing *)
class c = object method m : 'a. 'a option -> ([> `A] as 'a) = fun x -> `A end;;
[%%expect {|
class c : object method m : ([> `A ] as 'a) option -> 'a end
|}];;

(* various old bugs *)
class virtual ['a] visitor =
object method virtual caseNil : 'a end
and virtual int_list =
object method virtual visit : 'a.('a visitor -> 'a) end;;
[%%expect {|
Line _, characters 30-51:
Error: The universal type variable 'a cannot be generalized:
       it escapes its scope.
|}];;

type ('a,'b) list_visitor = < caseNil : 'a; caseCons : 'b -> 'b list -> 'a >
type 'b alist = < visit : 'a. ('a,'b) list_visitor -> 'a >
[%%expect {|
type ('a, 'b) list_visitor = < caseCons : 'b -> 'b list -> 'a; caseNil : 'a >
type 'b alist = < visit : 'a. ('a, 'b) list_visitor -> 'a >
|}];;

(* PR#1607 *)
class type ct = object ('s)
  method fold : ('b -> 's -> 'b) -> 'b -> 'b
end
type t = {f : 'a 'b. ('b -> (#ct as 'a) -> 'b) -> 'b};;
[%%expect {|
class type ct = object ('a) method fold : ('b -> 'a -> 'b) -> 'b -> 'b end
type t = { f : 'a 'b. ('b -> (#ct as 'a) -> 'b) -> 'b; }
|}];;

(* PR#1663 *)
type t = u and u = t;;
[%%expect {|
Line _, characters 0-10:
Error: The definition of t contains a cycle:
       u
|}];;

(* PR#1731 *)
class ['t] a = object constraint 't = [> `A of 't a] end
type t = [ `A of t a ];;
[%%expect {|
class ['a] a : object constraint 'a = [> `A of 'a a ] end
type t = [ `A of t a ]
|}];;

(* Wrong in 3.06 *)
type ('a,'b) t constraint 'a = 'b and ('a,'b) u = ('a,'b) t;;
[%%expect {|
Line _, characters 50-59:
Error: Constraints are not satisfied in this type.
       Type ('a, 'b) t should be an instance of ('c, 'c) t
|}];;

(* Full polymorphism if we do not expand *)
type 'a t = 'a and u = int t;;
[%%expect {|
type 'a t = 'a
and u = int t
|}];;

(* Loose polymorphism if we expand *)
type 'a t constraint 'a = int;;
type 'a u = 'a and 'a v = 'a u t;;
type 'a u = 'a and 'a v = 'a u t constraint 'a = int;;
[%%expect {|
type 'a t constraint 'a = int
Line _, characters 26-32:
Error: Constraints are not satisfied in this type.
       Type 'a u t should be an instance of int t
|}];;

(* Behaviour is unstable *)
type g = int;;
type 'a t = unit constraint 'a = g;;
type 'a u = 'a and 'a v = 'a u t;;
type 'a u = 'a and 'a v = 'a u t constraint 'a = int;;
[%%expect {|
type g = int
type 'a t = unit constraint 'a = g
Line _, characters 26-32:
Error: Constraints are not satisfied in this type.
       Type 'a u t should be an instance of g t
|}];;

(* Example of wrong expansion *)
type 'a u = < m : 'a v > and 'a v = 'a list u;;
[%%expect {|
Line _, characters 0-24:
Error: In the definition of v, type 'a list u should be 'a u
|}];;

(* PR#1744: Ctype.matches *)
type 'a t = 'a
type 'a u = A of 'a t;;
[%%expect {|
type 'a t = 'a
type 'a u = A of 'a t
|}];;

(* Unification of cyclic terms *)
type 'a t = < a : 'a >;;
fun (x : 'a t as 'a) -> (x : 'b t);;
type u = 'a t as 'a;;
[%%expect {|
type 'a t = < a : 'a >
- : ('a t as 'a) -> 'a t = <fun>
type u = 'a t as 'a
|}, Principal{|
type 'a t = < a : 'a >
- : ('a t as 'a) -> ('b t as 'b) t = <fun>
type u = 'a t as 'a
|}];;


(* Variant tests *)
type t = A | B;;
function `A,_ -> 1 | _,A -> 2 | _,B -> 3;;
function `A,_ -> 1 | _,(A|B) -> 2;;
function Some `A, _ -> 1 | Some _, A -> 2 | None, A -> 3 | _, B -> 4;;
function Some `A, A -> 1 | Some `A, B -> 1
       | Some _, A -> 2  | None, A -> 3 | _, B -> 4;;
function A, `A -> 1 | A, `B -> 2 | B, _ -> 3;;
function `A, A -> 1 | `B, A -> 2 | _, B -> 3;;
function (`A|`B), _ -> 0 | _,(`A|`B) -> 1;;
function `B,1 -> 1 | _,1 -> 2;;
function 1,`B -> 1 | 1,_ -> 2;;
[%%expect {|
type t = A | B
- : [> `A ] * t -> int = <fun>
- : [> `A ] * t -> int = <fun>
- : [> `A ] option * t -> int = <fun>
- : [> `A ] option * t -> int = <fun>
- : t * [< `A | `B ] -> int = <fun>
- : [< `A | `B ] * t -> int = <fun>
Line _, characters 0-41:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(`<some other tag>, `<some other tag>)
- : [> `A | `B ] * [> `A | `B ] -> int = <fun>
Line _, characters 0-29:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(_, 0)
Line _, characters 21-24:
Warning 11: this match case is unused.
- : [< `B ] * int -> int = <fun>
Line _, characters 0-29:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(0, _)
Line _, characters 21-24:
Warning 11: this match case is unused.
- : int * [< `B ] -> int = <fun>
|}];;

(* pass typetexp, but fails during Typedecl.check_recursion *)
type ('a, 'b) a = 'a -> unit constraint 'a = [> `B of ('a, 'b) b as 'b]
and  ('a, 'b) b = 'b -> unit constraint 'b = [> `A of ('a, 'b) a as 'a];;
[%%expect {|
Line _, characters 0-71:
Error: The definition of a contains a cycle:
       [> `B of ('a, 'b) b as 'b ] as 'a
|}];;

(* PR#1917: expanding may change original in Ctype.unify2 *)
(* Note: since 3.11, the abbreviations are not used when printing
   a type where they occur recursively inside. *)
class type ['a, 'b] a = object
  method b: ('a, 'b) #b as 'b
  method as_a: ('a, 'b) a
end and ['a, 'b] b = object
  method a: ('a, 'b) #a as 'a
  method as_b: ('a, 'b) b
end;;
[%%expect {|
class type ['a, 'b] a =
  object
    constraint 'a = < as_a : ('a, 'b) a as 'c; b : 'b; .. >
    constraint 'b = < a : 'a; as_b : ('a, 'b) b; .. >
    method as_a : 'c
    method b : 'b
  end
and ['a, 'b] b =
  object
    constraint 'a = < as_a : ('a, 'b) a; b : 'b; .. >
    constraint 'b = < a : 'a; as_b : ('a, 'b) b; .. >
    method a : 'a
    method as_b : ('a, 'b) b
  end
|}];;

class type ['b] ca = object ('s) inherit ['s, 'b] a end;;
class type ['a] cb = object ('s) inherit ['a, 's] b end;;
[%%expect {|
class type ['a] ca =
  object ('b)
    constraint 'a = < a : 'b; as_b : ('b, 'a) b; .. >
    method as_a : ('b, 'a) a
    method b : 'a
  end
class type ['a] cb =
  object ('b)
    constraint 'a = < as_a : ('a, 'b) a; b : 'b; .. >
    method a : 'a
    method as_b : ('a, 'b) b
  end
|}];;

type bt = 'b ca cb as 'b
;;
[%%expect {|
type bt = 'a ca cb as 'a
|}];;

(* final classes, etc... *)
class c = object method m = 1 end;;
let f () = object (self:c) method m = 1 end;;
let f () = object (self:c) method private n = 1 method m = self#n end;;
let f () = object method private n = 1 method m = {<>}#n end;;
let f () = object (self:c) method n = 1 method m = 2 end;;
let f () = object (_:'s) constraint 's = < n : int > method m = 1 end;;
class c = object (_ : 's)
  method x = 1
  method private m =
    object (self: 's) method x = 3 method private m = self end
end;;
let o = object (_ : 's)
  method x = 1
  method private m =
    object (self: 's) method x = 3 method private m = self end
end;;
[%%expect {|
class c : object method m : int end
val f : unit -> c = <fun>
val f : unit -> c = <fun>
Line _, characters 11-60:
Warning 15: the following private methods were made public implicitly:
 n.
val f : unit -> < m : int; n : int > = <fun>
Line _, characters 11-56:
Error: This object is expected to have type c but actually has type
         < m : int; n : 'a >
       The first object type has no method n
|}];;


(* Unsound! *)
fun (x : <m : 'a. 'a * <m: 'b. 'a * 'foo> > as 'foo) ->
  (x : <m : 'a. 'a * (<m:'b. 'a * <m:'c. 'c * 'bar> > as 'bar) >);;
type 'a foo = <m: 'b. 'a * 'a foo>
type foo' =   <m: 'a. 'a * 'a foo>
type 'a bar = <m: 'b. 'a * <m: 'c. 'c * 'a bar> >
type bar' =   <m: 'a. 'a * 'a bar >
let f (x : foo') = (x : bar');;
[%%expect {|
Line _, characters 3-4:
Error: This expression has type < m : 'a. 'a * < m : 'a * 'b > > as 'b
       but an expression was expected of type
         < m : 'a. 'a * (< m : 'a * < m : 'c. 'c * 'd > > as 'd) >
       Types for method m are incompatible
|}];;

fun (x : <m : 'a. 'a * ('a * <m : 'a. 'a * 'foo> as 'foo)>) ->
  (x : <m : 'b. 'b * ('b * <m : 'c. 'c * ('c * 'bar)>)> as 'bar);;
fun (x : <m : 'a. 'a * ('a * <m : 'a. 'a * 'foo> as 'foo)>) ->
  (x : <m : 'b. 'b * ('b * <m : 'c. 'c * ('b * 'bar)>)> as 'bar);;
fun (x : <m : 'a. 'a * ('a * 'foo)> as 'foo) ->
  (x : <m : 'b. 'b * ('b * <m:'c. 'c * 'bar> as 'bar)>);;
let f x =
    (x : <m : 'a. 'a -> ('a * <m:'c. 'c -> 'bar> as 'bar)>
       :> <m : 'a. 'a -> ('a * 'foo)> as 'foo);;
[%%expect {|
Line _, characters 3-4:
Error: This expression has type
         < m : 'b. 'b * ('b * < m : 'c. 'c * 'a > as 'a) >
       but an expression was expected of type
         < m : 'b. 'b * ('b * < m : 'c. 'c * ('c * 'd) >) > as 'd
       Types for method m are incompatible
|}];;

module M
: sig val f : (<m : 'b. 'b * ('b * <m:'c. 'c * 'bar> as 'bar)>) -> unit end
= struct let f (x : <m : 'a. 'a * ('a * 'foo)> as 'foo) = () end;;
module M
: sig type t = <m : 'b. 'b * ('b * <m:'c. 'c * 'bar> as 'bar)> end
= struct type t = <m : 'a. 'a * ('a * 'foo)> as 'foo end;;
[%%expect {|
Line _, characters 2-64:
Error: Signature mismatch:
       Modules do not match:
         sig val f : (< m : 'a. 'a * ('a * 'b) > as 'b) -> unit end
       is not included in
         sig
           val f : < m : 'b. 'b * ('b * < m : 'c. 'c * 'a > as 'a) > -> unit
         end
       Values do not match:
         val f : (< m : 'a. 'a * ('a * 'b) > as 'b) -> unit
       is not included in
         val f : < m : 'b. 'b * ('b * < m : 'c. 'c * 'a > as 'a) > -> unit
|}];;

module M : sig type 'a t type u = <m: 'a. 'a t> end
= struct type 'a t = int type u = <m: int> end;;
module M : sig type 'a t val f : <m: 'a. 'a t> -> int end
= struct type 'a t = int let f (x : <m:int>) = x#m end;;
(* The following should be accepted too! *)
module M : sig type 'a t val f : <m: 'a. 'a t> -> int end
= struct type 'a t = int let f x = x#m end;;
[%%expect {|
module M : sig type 'a t type u = < m : 'a. 'a t > end
module M : sig type 'a t val f : < m : 'a. 'a t > -> int end
module M : sig type 'a t val f : < m : 'a. 'a t > -> int end
|}];;

let f x y =
  ignore (x :> <m:'a.'a -> 'c * < > > as 'c);
  ignore (y :> <m:'b.'b -> 'd * < > > as 'd);
  x = y;;
[%%expect {|
val f :
  (< m : 'a. 'a -> (< m : 'a. 'a -> 'c * <  > > as 'c) * < .. >; .. > as 'b) ->
  'b -> bool = <fun>
|}];;


(* Subtyping *)

type t = [`A|`B];;
type v = private [> t];;
fun x -> (x : t :> v);;
type u = private [< t];;
fun x -> (x : u :> v);;
fun x -> (x : v :> u);;
type v = private [< t];;
fun x -> (x : u :> v);;
type p = <x:p>;;
type q = private <x:p; ..>;;
fun x -> (x : q :> p);;
fun x -> (x : p :> q);;
[%%expect {|
type t = [ `A | `B ]
type v = private [> t ]
- : t -> v = <fun>
type u = private [< t ]
- : u -> v = <fun>
Line _, characters 9-21:
Error: Type v = [> `A | `B ] is not a subtype of u = [< `A | `B ]
|}];;

let f1 x =
  (x : <m:'a. (<p:int;..> as 'a) -> int>
    :> <m:'b. (<p:int;q:int;..> as 'b) -> int>);;
let f2 x =
  (x : <m:'a. (<p:<a:int>;..> as 'a) -> int>
    :> <m:'b. (<p:<a:int;b:int>;..> as 'b) -> int>);;
let f3 x =
  (x : <m:'a. (<p:<a:int;b:int>;..> as 'a) -> int>
    :> <m:'b. (<p:<a:int>;..> as 'b) -> int>);;
let f4 x = (x : <p:<a:int;b:int>;..> :> <p:<a:int>;..>);;
let f5 x =
  (x : <m:'a. [< `A of <p:int> ] as 'a> :> <m:'a. [< `A of < > ] as 'a>);;
let f6 x =
  (x : <m:'a. [< `A of < > ] as 'a> :> <m:'a. [< `A of <p:int> ] as 'a>);;
[%%expect {|
Line _, characters 2-88:
Error: Type < m : 'a. (< p : int; .. > as 'a) -> int > is not a subtype of
         < m : 'b. (< p : int; q : int; .. > as 'b) -> int >
       Type < p : int; q : int; .. > as 'c is not a subtype of
         < p : int; .. > as 'd
|}];;

(* Keep sharing the epsilons *)
let f x = if true then (x : < m : 'a. 'a -> 'a >) else x;;
fun x -> (f x)#m;; (* Warning 18 *)
let f (x, y) = if true then (x : < m : 'a. 'a -> 'a >) else x;;
fun x -> (f (x,x))#m;; (* Warning 18 *)
let f x = if true then [| (x : < m : 'a. 'a -> 'a >) |] else [|x|];;
fun x -> (f x).(0)#m;; (* Warning 18 *)
[%%expect {|
val f : < m : 'a. 'a -> 'a > -> < m : 'a. 'a -> 'a > = <fun>
- : < m : 'a. 'a -> 'a > -> 'b -> 'b = <fun>
val f : < m : 'a. 'a -> 'a > * 'b -> < m : 'a. 'a -> 'a > = <fun>
- : < m : 'a. 'a -> 'a > -> 'b -> 'b = <fun>
val f : < m : 'a. 'a -> 'a > -> < m : 'a. 'a -> 'a > array = <fun>
- : < m : 'a. 'a -> 'a > -> 'b -> 'b = <fun>
|}, Principal{|
val f : < m : 'a. 'a -> 'a > -> < m : 'a. 'a -> 'a > = <fun>
Line _, characters 9-16:
Warning 18: this use of a polymorphic method is not principal.
- : < m : 'a. 'a -> 'a > -> 'b -> 'b = <fun>
val f : < m : 'a. 'a -> 'a > * 'b -> < m : 'a. 'a -> 'a > = <fun>
Line _, characters 9-20:
Warning 18: this use of a polymorphic method is not principal.
- : < m : 'a. 'a -> 'a > -> 'b -> 'b = <fun>
val f : < m : 'a. 'a -> 'a > -> < m : 'a. 'a -> 'a > array = <fun>
Line _, characters 9-20:
Warning 18: this use of a polymorphic method is not principal.
- : < m : 'a. 'a -> 'a > -> 'b -> 'b = <fun>
|}];;

(* Not really principal? *)
class c = object method id : 'a. 'a -> 'a = fun x -> x end;;
type u = c option;;
let just = function None -> failwith "just" | Some x -> x;;
let f x = let l = [Some x; (None : u)] in (just(List.hd l))#id;;
let g x =
  let none = (fun y -> ignore [y;(None:u)]; y) None in
  let x = List.hd [Some x; none] in (just x)#id;;
let h x =
  let none = let y = None in ignore [y;(None:u)]; y in
  let x = List.hd [Some x; none] in (just x)#id;;
[%%expect {|
class c : object method id : 'a -> 'a end
type u = c option
val just : 'a option -> 'a = <fun>
val f : c -> 'a -> 'a = <fun>
val g : c -> 'a -> 'a = <fun>
val h : < id : 'a; .. > -> 'a = <fun>
|}, Principal{|
class c : object method id : 'a -> 'a end
type u = c option
val just : 'a option -> 'a = <fun>
Line _, characters 42-62:
Warning 18: this use of a polymorphic method is not principal.
val f : c -> 'a -> 'a = <fun>
Line _, characters 36-47:
Warning 18: this use of a polymorphic method is not principal.
val g : c -> 'a -> 'a = <fun>
val h : < id : 'a; .. > -> 'a = <fun>
|}];;

(* Only solved for parameterless abbreviations *)
type 'a u = c option;;
let just = function None -> failwith "just" | Some x -> x;;
let f x = let l = [Some x; (None : _ u)] in (just(List.hd l))#id;;
[%%expect {|
type 'a u = c option
val just : 'a option -> 'a = <fun>
val f : c -> 'a -> 'a = <fun>
|}];;

(* polymorphic recursion *)

let rec f : 'a. 'a -> _ = fun x -> 1 and g x = f x;;
type 'a t = Leaf of 'a | Node of ('a * 'a) t;;
let rec depth : 'a. 'a t -> _ =
  function Leaf _ -> 1 | Node x -> 1 + depth x;;
let rec depth : 'a. 'a t -> _ =
  function Leaf _ -> 1 | Node x -> 1 + d x
and d x = depth x;; (* fails *)
let rec depth : 'a. 'a t -> _ =
  function Leaf x -> x | Node x -> 1 + depth x;; (* fails *)
let rec depth : 'a. 'a t -> _ =
  function Leaf x -> x | Node x -> depth x;; (* fails *)
let rec depth : 'a 'b. 'a t -> 'b =
  function Leaf x -> x | Node x -> depth x;; (* fails *)
let rec r : 'a. 'a list * 'b list ref = [], ref []
and q () = r;;
let f : 'a. _ -> _ = fun x -> x;;
let zero : 'a. [> `Int of int | `B of 'a] as 'a  = `Int 0;; (* ok *)
let zero : 'a. [< `Int of int] as 'a = `Int 0;; (* fails *)
[%%expect {|
val f : 'a -> int = <fun>
val g : 'a -> int = <fun>
type 'a t = Leaf of 'a | Node of ('a * 'a) t
val depth : 'a t -> int = <fun>
Line _, characters 2-42:
Error: This definition has type 'a t -> int which is less general than
         'a0. 'a0 t -> int
|}];;

(* compare with records (should be the same) *)
type t = {f: 'a. [> `Int of int | `B of 'a] as 'a}
let zero = {f = `Int 0} ;;
type t = {f: 'a. [< `Int of int] as 'a}
let zero = {f = `Int 0} ;; (* fails *)
[%%expect {|
type t = { f : 'a. [> `B of 'a | `Int of int ] as 'a; }
val zero : t = {f = `Int 0}
type t = { f : 'a. [< `Int of int ] as 'a; }
Line _, characters 16-22:
Error: This expression has type [> `Int of int ]
       but an expression was expected of type [< `Int of int ]
       Types for tag `Int are incompatible
|}];;

(* Yet another example *)
let rec id : 'a. 'a -> 'a = fun x -> x
and neg i b = (id (-i), id (not b));;
[%%expect {|
val id : 'a -> 'a = <fun>
val neg : int -> bool -> int * bool = <fun>
|}];;

(* De Xavier *)

type t = A of int | B of (int*t) list | C of (string*t) list
[%%expect {|
type t = A of int | B of (int * t) list | C of (string * t) list
|}];;

let rec transf f = function
  | A x -> f x
  | B l -> B (transf_alist f l)
  | C l -> C (transf_alist f l)
and transf_alist : 'a. _ -> ('a*t) list -> ('a*t) list = fun f -> function
  | [] -> []
  | (k,v)::tl -> (k, transf f v) :: transf_alist f tl
;;
[%%expect {|
val transf : (int -> t) -> t -> t = <fun>
val transf_alist : (int -> t) -> ('a * t) list -> ('a * t) list = <fun>
|}];;

(* PR#4862 *)

type t = {f: 'a. ('a list -> int) Lazy.t}
let l : t = { f = lazy (raise Not_found)};;
[%%expect {|
type t = { f : 'a. ('a list -> int) Lazy.t; }
val l : t = {f = <lazy>}
|}];;

(* variant *)
type t = {f: 'a. 'a -> unit};;
let f ?x y = () in {f};;
let f ?x y = y in {f};; (* fail *)
[%%expect {|
type t = { f : 'a. 'a -> unit; }
- : t = {f = <fun>}
Line _, characters 19-20:
Error: This field value has type unit -> unit which is less general than
         'a. 'a -> unit
|}];;

(* Polux Moon caml-list 2011-07-26 *)
module Polux = struct
  type 'par t = 'par
  let ident v = v
  class alias = object method alias : 'a . 'a t -> 'a = ident end
  let f (x : <m : 'a. 'a t>) = (x : <m : 'a. 'a>)
end;;
[%%expect {|
module Polux :
  sig
    type 'par t = 'par
    val ident : 'a -> 'a
    class alias : object method alias : 'a t -> 'a end
    val f : < m : 'a. 'a t > -> < m : 'a. 'a >
  end
|}];;

(* PR#5560 *)

let (a, b) = (raise Exit : int * int);;
type t = { foo : int }
let {foo} = (raise Exit : t);;
type s = A of int
let (A x) = (raise Exit : s);;
[%%expect {|
Exception: Pervasives.Exit.
|}];;

(* PR#5224 *)

type 'x t = < f : 'y. 'y t >;;
[%%expect {|
Line _, characters 0-28:
Error: In the definition of t, type 'y t should be 'x t
|}];;

(* PR#6056, PR#6057 *)
let using_match b =
  let f =
    match b with
    | true -> fun x -> x
    | false -> fun x -> x
  in
  f 0,f
;;
[%%expect {|
val using_match : bool -> int * ('a -> 'a) = <fun>
|}];;

match (fun x -> x), fun x -> x with x, y -> x, y;;
match fun x -> x with x -> x, x;;
[%%expect {|
- : ('a -> 'a) * ('b -> 'b) = (<fun>, <fun>)
- : ('a -> 'a) * ('b -> 'b) = (<fun>, <fun>)
|}];;

(* PR#6747 *)
(* ok *)
let n = object
  method m : 'x 'o. ([< `Foo of 'x] as 'o) -> 'x = fun x -> assert false
end;;
[%%expect {|
val n : < m : 'x 'a. ([< `Foo of 'x ] as 'a) -> 'x > = <obj>
|}];;
(* ok, but not with -principal *)
let n =
  object method m : 'x. [< `Foo of 'x] -> 'x = fun x -> assert false end;;
[%%expect {|
val n : < m : 'x. [< `Foo of 'x ] -> 'x > = <obj>
|}, Principal{|
Line _, characters 47-68:
Error: This method has type ([< `Foo of 'b ] as 'a) -> 'b
       which is less general than 'x. 'a -> 'x
|}];;
(* fail *)
let (n : < m : 'a. [< `Foo of int] -> 'a >) =
  object method m : 'x. [< `Foo of 'x] -> 'x = fun x -> assert false end;;
[%%expect {|
Line _, characters 2-72:
Error: This expression has type < m : 'x. [< `Foo of 'x ] -> 'x >
       but an expression was expected of type
         < m : 'a. [< `Foo of int ] -> 'a >
       The universal variable 'x would escape its scope
|}, Principal{|
Line _, characters 47-68:
Error: This method has type ([< `Foo of 'b ] as 'a) -> 'b
       which is less general than 'x. 'a -> 'x
|}];;
(* fail *)
let (n : 'b -> < m : 'a . ([< `Foo of int] as 'b) -> 'a >) = fun x ->
  object method m : 'x. [< `Foo of 'x] -> 'x = fun x -> assert false end;;
[%%expect {|
Line _, characters 2-72:
Error: This expression has type < m : 'x. [< `Foo of 'x ] -> 'x >
       but an expression was expected of type
         < m : 'a. [< `Foo of int ] -> 'a >
       The universal variable 'x would escape its scope
|}, Principal{|
Line _, characters 47-68:
Error: This method has type ([< `Foo of 'b ] as 'a) -> 'b
       which is less general than 'x. 'a -> 'x
|}];;

(* PR#6171 *)
let f b (x: 'x) =
  let module M = struct type t = A end in
  if b then x else M.A;;
[%%expect {|
Line _, characters 19-22:
Error: This expression has type M.t but an expression was expected of type 'x
       The type constructor M.t would escape its scope
|}];;


(* PR#6987 *)
type 'a t = V1 of 'a

type ('c,'t) pvariant = [ `V of ('c * 't t) ]

class ['c] clss =
  object
    method mthod : 't . 'c -> 't t -> ('c, 't) pvariant = fun c x ->
      `V (c, x)
  end;;

let f2 = fun o c x -> match x with | V1 _ -> x

let rec f1 o c x =
  match (o :> _ clss)#mthod c x with
  | `V c -> f2 o c x;;
[%%expect{|
type 'a t = V1 of 'a
type ('c, 't) pvariant = [ `V of 'c * 't t ]
class ['c] clss : object method mthod : 'c -> 't t -> ('c, 't) pvariant end
val f2 : 'a -> 'b -> 'c t -> 'c t = <fun>
val f1 :
  < mthod : 't. 'a -> 't t -> [< `V of 'a * 't t ]; .. > ->
  'a -> 'b t -> 'b t = <fun>
|}]

(* PR#7285 *)
type (+'a,-'b) foo = private int;;
let f (x : int) : ('a,'a) foo = Obj.magic x;;
let x = f 3;;
[%%expect{|
type (+'a, -'b) foo = private int
val f : int -> ('a, 'a) foo = <fun>
val x : ('_weak1, '_weak1) foo = 3
|}]


(* PR#7344*)
let rec f : unit -> < m: 'a. 'a -> 'a> = fun () ->
  let x = f () in
  ignore (x#m 1);
  ignore (x#m "hello");
  assert false;;
[%%expect{|
val f : unit -> < m : 'a. 'a -> 'a > = <fun>
|}]

(* PR#7395 *)
type u
type 'a t = u;;
let c (f : u -> u) =
 object
   method apply: 'a. 'a t -> 'a t = fun x -> f x
 end;;
[%%expect{|
type u
type 'a t = u
val c : (u -> u) -> < apply : 'a. 'a t -> 'a t > = <fun>
|}]

(* PR#7496 *)
let f (x : < m: 'a. ([< `Foo of int & float] as 'a) -> unit>)
         : < m: 'a. ([< `Foo of int & float] as 'a) -> unit> = x;;

type t = { x : 'a. ([< `Foo of int & float ] as 'a) -> unit };;
let f t = { x = t.x };;
[%%expect{|
val f :
  < m : 'a. ([< `Foo of int & float ] as 'a) -> unit > ->
  < m : 'b. ([< `Foo of int & float ] as 'b) -> unit > = <fun>
type t = { x : 'a. ([< `Foo of int & float ] as 'a) -> unit; }
val f : t -> t = <fun>
|}]

type t = <m:int>
type g = <n:string; t>
type h = <x:string; y:int; g>
[%%expect{|
type t = < m : int >
type g = < m : int; n : string >
type h = < m : int; n : string; x : string; y : int >
|}]

type t = <g>
and g = <a:t>
[%%expect{|
Line _, characters 10-11:
Error: The type constructor g
is not yet completely defined
|}]

type t = int
type g = <t>
[%%expect{|
type t = int
Line _, characters 10-11:
Error: The type int is not an object type
|}]

type t = <a:int>
type g = <t; t; t;>
[%%expect{|
type t = < a : int >
type g = < a : int >
|}]

type c = <a:int; d:string>
let s:c = object method a=1; method d="123" end
[%%expect{|
type c = < a : int; d : string >
val s : c = <obj>
|}]

type 'a t = < m: 'a >
type s = < int t >
module M = struct type t = < m: int > end
type u = < M.t >
type r = < a : int; < b : int > >
type e = < >
type r1 = < a : int; e >
type r2 = < a : int; < < < > > > >
[%%expect{|
type 'a t = < m : 'a >
type s = < m : int >
module M : sig type t = < m : int > end
type u = < m : int >
type r = < a : int; b : int >
type e = <  >
type r1 = < a : int >
type r2 = < a : int >
|}]

type gg = <a:int->float; a:int>
[%%expect{|
Line _, characters 27-30:
Error: Method 'a' has type int, which should be int -> float
|}]

type t = <a:int; b:string>
type g = <b:float; t;>
[%%expect{|
type t = < a : int; b : string >
Line _, characters 19-20:
Error: Method 'b' has type string, which should be float
|}]

module A = struct
  class type ['a] t1 = object method f : 'a end
end
type t = < int A.t1 >
[%%expect{|
module A : sig class type ['a] t1 = object method f : 'a end end
type t = < f : int >
|}]

type t = < int #A.t1 >
[%%expect{|
Line _, characters 11-20:
Error: Illegal open object type
|}]

let g = fun (y : ('a * 'b)) x -> (x : < <m: 'a> ; <m: 'b> >)
[%%expect{|
val g : 'a * 'a -> < m : 'a > -> < m : 'a > = <fun>
|}]

type 'a t = <m: 'a ; m: int>
[%%expect{|
type 'a t = < m : 'a > constraint 'a = int
|}]

(* GPR#1142 *)
module M () = struct
  let f : 'a -> 'a = assert false
  let g : 'a -> 'a = raise Not_found
end

[%%expect{|
module M : functor () -> sig val f : 'a -> 'a val g : 'a -> 'a end
|}]
