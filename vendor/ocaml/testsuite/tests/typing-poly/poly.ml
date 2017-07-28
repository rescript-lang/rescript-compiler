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

class ['b] ilist l = object
  val l = l
  method add x = {< l = x :: l >}
  method fold : 'a. f:('a -> 'b -> 'a) -> init:'a -> 'a =
    List.fold_left l
end
;;
class virtual ['a] vlist = object (_ : 'self)
  method virtual add : 'a -> 'self
  method virtual fold : 'b. f:('b -> 'a -> 'b) -> init:'b -> 'b
end
;;
class ilist2 l = object
  inherit [int] vlist
  val l = l
  method add x = {< l = x :: l >}
  method fold = List.fold_left l
end
;;
let ilist2 l = object
  inherit [_] vlist
  val l = l
  method add x = {< l = x :: l >}
  method fold = List.fold_left l
end
;;
class ['a] ilist3 l = object
  inherit ['a] vlist
  val l = l
  method add x = {< l = x :: l >}
  method fold = List.fold_left l
end
;;
class ['a] ilist4 (l : 'a list) = object
  val l = l
  method virtual add : _
  method add x = {< l = x :: l >}
  method virtual fold : 'b. f:('b -> 'a -> 'b) -> init:'b -> 'b
  method fold = List.fold_left l
end
;;
class ['a] ilist5 (l : 'a list) = object (self)
  val l = l
  method add x = {< l = x :: l >}
  method virtual fold : 'b. f:('b -> 'a -> 'b) -> init:'b -> 'b
  method virtual fold2 : 'b. f:('b -> 'a -> 'b) -> init:'b -> 'b
  method fold2 ~f ~init = self#fold ~f ~init:(self#fold ~f ~init)
  method fold = List.fold_left l
end
;;
class ['a] ilist6 l = object (self)
  inherit ['a] vlist
  val l = l
  method add x = {< l = x :: l >}
  method virtual fold2 : 'b. f:('b -> 'a -> 'b) -> init:'b -> 'b
  method fold2 ~f ~init = self#fold ~f ~init:(self#fold ~f ~init)
  method fold = List.fold_left l
end
;;
class virtual ['a] olist = object
  method virtual fold : 'c. f:('a -> 'c -> 'c) -> init:'c -> 'c
end
;;
class ['a] onil = object
  inherit ['a] olist
  method fold ~f ~init = init
end
;;
class ['a] ocons ~hd ~tl = object (_ : 'b)
  inherit ['a] olist
  val hd : 'a = hd
  val tl : 'a olist = tl
  method fold ~f ~init = f hd (tl#fold ~f ~init)
end
;;
class ['a] ostream ~hd ~tl = object (_ : 'b)
  inherit ['a] olist
  val hd : 'a = hd
  val tl : _ #olist = (tl : 'a ostream)
  method fold ~f ~init = f hd (tl#fold ~f ~init)
  method empty = false
end
;;
class ['a] ostream1 ~hd ~tl = object (self : 'b)
  inherit ['a] olist
  val hd = hd
  val tl : 'b = tl
  method hd = hd
  method tl = tl
  method fold ~f ~init =
    self#tl#fold ~f ~init:(f self#hd init)
end
;;

class vari = object
  method virtual m : 'a. ([< `A|`B|`C] as 'a) -> int
  method m = function `A -> 1 | `B|`C  -> 0
end
;;
class vari = object
  method m : 'a. ([< `A|`B|`C] as 'a) -> int = function `A -> 1 | `B|`C -> 0
end
;;
module V =
  struct
    type v = [`A | `B | `C]
    let m : [< v] -> int = function `A -> 1 | #v -> 0
  end
;;
class varj = object
  method virtual m : 'a. ([< V.v] as 'a) -> int
  method m = V.m
end
;;

module type T = sig
  class vari : object method m : 'a. ([< `A | `B | `C] as 'a) -> int end
end
;;
module M0 = struct
  class vari = object
    method virtual m : 'a. ([< `A|`B|`C] as 'a) -> int
    method m = function `A -> 1 | `B|`C -> 0
  end
end
;;
module M : T = M0
;;
let v = new M.vari;;
v#m `A;;

class point ~x ~y = object
  val x : int = x
  val y : int = y
  method x = x
  method y = y
end
;;
class color_point ~x ~y ~color = object
  inherit point ~x ~y
  val color : string = color
  method color = color
end
;;
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

class id = object
  method virtual id : 'a. 'a -> 'a
  method id x = x
end
;;

class type id_spec = object
  method id : 'a -> 'a
end
;;
class id_impl = object (_ : #id_spec)
  method id x = x
end
;;

class a = object
  method m = (new b : id_spec)#id true
end
and b = object (_ : #id_spec)
  method id x = x
end
;;

class ['a] id1 = object
  method virtual id : 'b. 'b -> 'a
  method id x = x
end
;;
class id2 (x : 'a) = object
  method virtual id : 'b. 'b -> 'a
  method id x = x
end
;;
class id3 x = object
  val x = x
  method virtual id : 'a. 'a -> 'a
  method id _ = x
end
;;
class id4 () = object
  val mutable r = None
  method virtual id : 'a. 'a -> 'a
  method id x =
    match r with
      None -> r <- Some x; x
    | Some y -> y
end
;;
class c = object
  method virtual m : 'a 'b. 'a -> 'b -> 'a
  method m x y = x
end
;;

let f1 (f : id) = f#id 1, f#id true
;;
let f2 f = (f : id)#id 1, (f : id)#id true
;;
let f3 f = f#id 1, f#id true
;;
let f4 f = ignore(f : id); f#id 1, f#id true
;;

class c = object
  method virtual m : 'a. (#id as 'a) -> int * bool
  method m (f : #id) = f#id 1, f#id true
end
;;

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

class ['a] bar (x : 'a) = object end
;;
type 'a foo = 'a foo bar
;;

fun x -> (x : < m : 'a. 'a * 'b > as 'b)#m;;
fun x -> (x : < m : 'a. 'b * 'a list> as 'b)#m;;
let f x = (x : < m : 'a. 'b * (< n : 'a; .. > as 'a) > as 'b)#m;;
fun (x : < p : 'a. < m : 'a ; n : 'b ; .. > as 'a > as 'b) -> x#p;;
fun (x : <m:'a. 'a * <p:'b. 'b * 'c * 'd> as 'c> as 'd) -> x#m;;
(* printer is wrong on the next (no official syntax) *)
fun (x : <m:'a.<p:'a;..> >) -> x#m;;

type sum = T of < id: 'a. 'a -> 'a > ;;
fun (T x) -> x#id;;

type record = { r: < id: 'a. 'a -> 'a > } ;;
fun x -> x.r#id;;
fun {r=x} -> x#id;;

class myself = object (self)
  method self : 'a. 'a -> 'b = fun _ -> self
end;;

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

type 'a t = unit
;;
class o = object method x : 'a. ([> `A] as 'a) t -> unit = fun _ -> () end
;;

class c = object method m = new d () end and d ?(x=0) () = object end;;
class d ?(x=0) () = object end and c = object method m = new d () end;;

class type numeral = object method fold : ('a -> 'a) -> 'a -> 'a end
class zero = object (_ : #numeral) method fold f x = x end
class next (n : #numeral) =
  object (_ : #numeral) method fold f x = n#fold f (f x) end
;;

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

type bad = {bad : 'a. 'a option ref};;
let bad = {bad = ref None};;
type bad2 = {mutable bad2 : 'a. 'a option ref option};;
let bad2 = {bad2 = None};;
bad2.bad2 <- Some (ref None);;

(* Type variable scope *)

let f (x: <m:'a.<p: 'a * 'b> as 'b>) (y : 'b) = ();;
let f (x: <m:'a. 'a * (<p:int*'b> as 'b)>) (y : 'b) = ();;

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

(* usage avant instance *)
class c = object method m : 'a. 'a option -> ([> `A] as 'a) = fun x -> `A end;;

(* various old bugs *)
class virtual ['a] visitor =
object method virtual caseNil : 'a end
and virtual int_list =
object method virtual visit : 'a.('a visitor -> 'a) end;;

type ('a,'b) list_visitor = < caseNil : 'a; caseCons : 'b -> 'b list -> 'a >
type 'b alist = < visit : 'a. ('a,'b) list_visitor -> 'a >

(* PR#1607 *)
class type ct = object ('s)
  method fold : ('b -> 's -> 'b) -> 'b -> 'b
end
type t = {f : 'a 'b. ('b -> (#ct as 'a) -> 'b) -> 'b};;

(* PR#1663 *)
type t = u and u = t;;

(* PR#1731 *)
class ['t] a = object constraint 't = [> `A of 't a] end
type t = [ `A of t a ];;

(* Wrong in 3.06 *)
type ('a,'b) t constraint 'a = 'b and ('a,'b) u = ('a,'b) t;;

(* Full polymorphism if we do not expand *)
type 'a t = 'a and u = int t;;

(* Loose polymorphism if we expand *)
type 'a t constraint 'a = int;;
type 'a u = 'a and 'a v = 'a u t;;
type 'a u = 'a and 'a v = 'a u t constraint 'a = int;;

(* Behaviour is unstable *)
type g = int;;
type 'a t = unit constraint 'a = g;;
type 'a u = 'a and 'a v = 'a u t;;
type 'a u = 'a and 'a v = 'a u t constraint 'a = int;;

(* Example of wrong expansion *)
type 'a u = < m : 'a v > and 'a v = 'a list u;;

(* PR#1744: Ctype.matches *)
type 'a t = 'a
type 'a u = A of 'a t;;

(* Unification of cyclic terms *)
type 'a t = < a : 'a >;;
fun (x : 'a t as 'a) -> (x : 'b t);;
type u = 'a t as 'a;;


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

(* pass typetexp, but fails during Typedecl.check_recursion *)
type ('a, 'b) a = 'a -> unit constraint 'a = [> `B of ('a, 'b) b as 'b]
and  ('a, 'b) b = 'b -> unit constraint 'b = [> `A of ('a, 'b) a as 'a];;

(* PR#1917: expanding may change original in Ctype.unify2 *)
(* Note: since 3.11, the abbreviations are not used when printing
   a type where they occur recursively inside. *)
class type ['a, 'b] a = object
  method b: ('a, 'b) #b as 'b
  method as_a: ('a, 'b) a
end and ['a, 'b] b = object
  method a: ('a, 'b) #a as 'a
  method as_b: ('a, 'b) b
end

class type ['b] ca = object ('s) inherit ['s, 'b] a end
class type ['a] cb = object ('s) inherit ['a, 's] b end

type bt = 'b ca cb as 'b
;;

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


(* Unsound! *)
fun (x : <m : 'a. 'a * <m: 'b. 'a * 'foo> > as 'foo) ->
  (x : <m : 'a. 'a * (<m:'b. 'a * <m:'c. 'c * 'bar> > as 'bar) >);;
type 'a foo = <m: 'b. 'a * 'a foo>
type foo' =   <m: 'a. 'a * 'a foo>
type 'a bar = <m: 'b. 'a * <m: 'c. 'c * 'a bar> >
type bar' =   <m: 'a. 'a * 'a bar >
let f (x : foo') = (x : bar');;

fun (x : <m : 'a. 'a * ('a * <m : 'a. 'a * 'foo> as 'foo)>) ->
  (x : <m : 'b. 'b * ('b * <m : 'c. 'c * ('c * 'bar)>)> as 'bar);;
fun (x : <m : 'a. 'a * ('a * <m : 'a. 'a * 'foo> as 'foo)>) ->
  (x : <m : 'b. 'b * ('b * <m : 'c. 'c * ('b * 'bar)>)> as 'bar);;
fun (x : <m : 'a. 'a * ('a * 'foo)> as 'foo) ->
  (x : <m : 'b. 'b * ('b * <m:'c. 'c * 'bar> as 'bar)>);;
let f x =
    (x : <m : 'a. 'a -> ('a * <m:'c. 'c -> 'bar> as 'bar)>
       :> <m : 'a. 'a -> ('a * 'foo)> as 'foo);;

module M
: sig val f : (<m : 'b. 'b * ('b * <m:'c. 'c * 'bar> as 'bar)>) -> unit end
= struct let f (x : <m : 'a. 'a * ('a * 'foo)> as 'foo) = () end;;
module M
: sig type t = <m : 'b. 'b * ('b * <m:'c. 'c * 'bar> as 'bar)> end
= struct type t = <m : 'a. 'a * ('a * 'foo)> as 'foo end;;

module M : sig type 'a t type u = <m: 'a. 'a t> end
= struct type 'a t = int type u = <m: int> end;;
module M : sig type 'a t val f : <m: 'a. 'a t> -> int end
= struct type 'a t = int let f (x : <m:int>) = x#m end;;
(* The following should be accepted too! *)
module M : sig type 'a t val f : <m: 'a. 'a t> -> int end
= struct type 'a t = int let f x = x#m end;;

let f x y =
  ignore (x :> <m:'a.'a -> 'c * < > > as 'c);
  ignore (y :> <m:'b.'b -> 'd * < > > as 'd);
  x = y;;


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

(* Keep sharing the epsilons *)
let f x = if true then (x : < m : 'a. 'a -> 'a >) else x;;
fun x -> (f x)#m;; (* Warning 18 *)
let f (x, y) = if true then (x : < m : 'a. 'a -> 'a >) else x;;
fun x -> (f (x,x))#m;; (* Warning 18 *)
let f x = if true then [| (x : < m : 'a. 'a -> 'a >) |] else [|x|];;
fun x -> (f x).(0)#m;; (* Warning 18 *)

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

(* Only solved for parameterless abbreviations *)
type 'a u = c option;;
let just = function None -> failwith "just" | Some x -> x;;
let f x = let l = [Some x; (None : _ u)] in (just(List.hd l))#id;;

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

(* compare with records (should be the same) *)
type t = {f: 'a. [> `Int of int | `B of 'a] as 'a}
let zero = {f = `Int 0} ;;
type t = {f: 'a. [< `Int of int] as 'a}
let zero = {f = `Int 0} ;; (* fails *)

(* Yet another example *)
let rec id : 'a. 'a -> 'a = fun x -> x
and neg i b = (id (-i), id (not b));;

(* De Xavier *)

type t = A of int | B of (int*t) list | C of (string*t) list

let rec transf f = function
  | A x -> f x
  | B l -> B (transf_alist f l)
  | C l -> C (transf_alist f l)
and transf_alist : 'a. _ -> ('a*t) list -> ('a*t) list = fun f -> function
  | [] -> []
  | (k,v)::tl -> (k, transf f v) :: transf_alist f tl
;;

(* PR#4862 *)

type t = {f: 'a. ('a list -> int) Lazy.t}
let l : t = { f = lazy (raise Not_found)};;

(* variant *)
type t = {f: 'a. 'a -> unit};;
let f ?x y = () in {f};;
let f ?x y = y in {f};; (* fail *)

(* Polux Moon caml-list 2011-07-26 *)
module Polux = struct
  type 'par t = 'par
  let ident v = v
  class alias = object method alias : 'a . 'a t -> 'a = ident end
  let f (x : <m : 'a. 'a t>) = (x : <m : 'a. 'a>)
end;;

(* PR#5560 *)

let (a, b) = (raise Exit : int * int);;
type t = { foo : int }
let {foo} = (raise Exit : t);;
type s = A of int
let (A x) = (raise Exit : s);;

(* PR#5224 *)

type 'x t = < f : 'y. 'y t >;;

(* PR#6056, PR#6057 *)
let using_match b =
  let f =
    match b with
    | true -> fun x -> x
    | false -> fun x -> x
  in
  f 0,f
;;

match (fun x -> x), fun x -> x with x, y -> x, y;;
match fun x -> x with x -> x, x;;
