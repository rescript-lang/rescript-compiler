[@@@foo]

let (x[@foo]) : unit [@foo] = ()[@foo]
  [@@foo]

type t =
  | Foo of (t[@foo]) [@foo]
[@@foo]

[@@@foo]


module M = struct
  type t = {
    l : (t [@foo]) [@foo]
  }
    [@@foo]
    [@@foo]

  [@@@foo]
end[@foo]
[@@foo]

module type S = sig

  include (module type of (M[@foo]))[@foo] with type t := M.t[@foo]
    [@@foo]

  [@@@foo]

end[@foo]
[@@foo]

[@@@foo]
type 'a with_default
  =  ?size:int       (** default [42] *)
  -> ?resizable:bool (** default [true] *)
  -> 'a

type obj = <
  meth1 : int -> int;
  (** method 1 *)

  meth2: unit -> float (** method 2 *);
>

type var = [
  | `Foo (** foo *)
  | `Bar of int * string (** bar *)
]

[%%foo let x = 1 in x]
let [%foo 2+1] : [%foo bar.baz] = [%foo "foo"]

[%%foo module M = [%bar] ]
let [%foo let () = () ] : [%foo type t = t ] = [%foo class c = object end]

[%%foo: 'a list]
let [%foo: [`Foo] ] : [%foo: t -> t ] = [%foo: < foo : t > ]

[%%foo? _ ]
[%%foo? Some y when y > 0]
let [%foo? (Bar x | Baz x) ] : [%foo? #bar ] = [%foo? { x }]

[%%foo: module M : [%baz]]
let [%foo: include S with type t = t ]
  : [%foo: val x : t  val y : t]
  = [%foo: type t = t ]
let int_with_custom_modifier =
  1234567890_1234567890_1234567890_1234567890_1234567890z
let float_with_custom_modifier =
  1234567890_1234567890_1234567890_1234567890_1234567890.z

let int32     = 1234l
let int64     = 1234L
let nativeint = 1234n

let hex_without_modifier = 0x32f
let hex_with_modifier    = 0x32g

let float_without_modifer = 1.2e3
let float_with_modifer    = 1.2g
let%foo x = 42
let%foo _ = () and _ = ()
let%foo _ = ()

(* Expressions *)
let () =
  let%foo[@foo] x = 3
  and[@foo] y = 4 in
  (let module%foo[@foo] M = M in ()) ;
  (let open%foo[@foo] M in ()) ;
  (fun%foo[@foo] x -> ()) ;
  (function%foo[@foo] x -> ()) ;
  (try%foo[@foo] () with _ -> ()) ;
  (if%foo[@foo] () then () else ()) ;
  while%foo[@foo] () do () done ;
  for%foo[@foo] x = () to () do () done ;
  assert%foo[@foo] true ;
  lazy%foo[@foo] x ;
  object%foo[@foo] end ;
  begin%foo[@foo] 3 end ;
  new%foo[@foo] x ;

  match%foo[@foo] () with
  (* Pattern expressions *)
  | lazy%foo[@foo] x -> ()
  | exception%foo[@foo] x -> ()

(* Class expressions *)
class x =
  fun[@foo] x ->
  let[@foo] x = 3 in
  object[@foo]
    inherit[@foo] x
    val[@foo] x = 3
    val[@foo] virtual x : t
    val![@foo] mutable x = 3
    method[@foo] x = 3
    method[@foo] virtual x : t
    method![@foo] private x = 3
    initializer[@foo] x
  end

(* Class type expressions *)
class type t =
  object[@foo]
    inherit[@foo] t
    val[@foo] x : t
    val[@foo] mutable x : t
    method[@foo] x : t
    method[@foo] private x : t
    constraint[@foo] t = t'
    [@@@abc]
    [%%id]
    [@@@aaa]
  end

(* Type expressions *)
type t =
  (module%foo[@foo] M)

(* Module expressions *)
module M =
  functor[@foo] (M : S) ->
    (val[@foo] x)
    (struct[@foo] end)

(* Module type expression *)
module type S =
  functor[@foo] (M:S) ->
    (module type of[@foo] M) ->
    (sig[@foo] end)

(* Structure items *)
let%foo[@foo] x = 4
and[@foo] y = x

type%foo[@foo] t = int
and[@foo] t = int
type%foo[@foo] t += T

class%foo[@foo] x = x
class type%foo[@foo] x = x
external%foo[@foo] x : _  = ""
exception%foo[@foo] X

module%foo[@foo] M = M
module%foo[@foo] rec M : S = M
and[@foo] M : S = M
module type%foo[@foo] S = S

include%foo[@foo] M
open%foo[@foo] M

(* Signature items *)
module type S = sig
  val%foo[@foo] x : t
  external%foo[@foo] x : t = ""

  type%foo[@foo] t = int
  and[@foo] t' = int
  type%foo[@foo] t += T

  exception%foo[@foo] X

  module%foo[@foo] M : S
  module%foo[@foo] rec M : S
  and[@foo] M : S
  module%foo[@foo] M = M

  module type%foo[@foo] S = S

  include%foo[@foo] M
  open%foo[@foo] M

  class%foo[@foo] x : t
  class type%foo[@foo] x = x

end

type t = ..;;
type t += A;;

[%extension_constructor A];;
([%extension_constructor A] : extension_constructor);;

module M = struct
  type extension_constructor = int
end;;

open M;;

([%extension_constructor A] : extension_constructor);;

(* By using two types we can have a recursive constraint *)
type 'a class_name = .. constraint 'a = < cast: 'a. 'a name -> 'a; ..>
and 'a name =
  Class : 'a class_name -> (< cast: 'a. 'a name -> 'a; ..> as 'a) name
;;

exception Bad_cast
;;

class type castable =
object
  method cast: 'a.'a name -> 'a
end
;;

(* Lets create a castable class with a name*)

class type foo_t =
object
  inherit castable
  method foo: string
end
;;

type 'a class_name += Foo: foo_t class_name
;;

class foo: foo_t =
object(self)
  method cast: type a. a name -> a =
    function
        Class Foo -> (self :> foo_t)
      | _ -> ((raise Bad_cast) : a)
  method foo = "foo"
end
;;

(* Now we can create a subclass of foo *)

class type bar_t =
object
  inherit foo
  method bar: string
end
;;

type 'a class_name += Bar: bar_t class_name
;;

class bar: bar_t =
object(self)
  inherit foo as super
  method cast: type a. a name -> a =
    function
        Class Bar -> (self :> bar_t)
      | other -> super#cast other
  method bar = "bar"
  [@@@id]
  [%%id]
end
;;

(* Now lets create a mutable list of castable objects *)

let clist :castable list ref = ref []
;;

let push_castable (c: #castable) =
  clist := (c :> castable) :: !clist
;;

let pop_castable () =
  match !clist with
      c :: rest ->
        clist := rest;
        c
    | [] -> raise Not_found
;;

(* We can add foos and bars to this list, and retrive them *)

push_castable (new foo);;
push_castable (new bar);;
push_castable (new foo);;

let c1: castable = pop_castable ();;
let c2: castable = pop_castable ();;
let c3: castable = pop_castable ();;

(* We can also downcast these values to foos and bars *)

let f1: foo = c1#cast (Class Foo);; (* Ok *)
let f2: foo = c2#cast (Class Foo);; (* Ok *)
let f3: foo = c3#cast (Class Foo);; (* Ok *)

let b1: bar = c1#cast (Class Bar);; (* Exception Bad_cast *)
let b2: bar = c2#cast (Class Bar);; (* Ok *)
let b3: bar = c3#cast (Class Bar);; (* Exception Bad_cast *)

type foo = ..
;;

type foo +=
    A
  | B of int
;;

let is_a x =
  match x with
    A -> true
  | _ -> false
;;

(* The type must be open to create extension *)

type foo
;;

type foo += A of int (* Error type is not open *)
;;

(* The type parameters must match *)

type 'a foo = ..
;;

type ('a, 'b) foo += A of int (* Error: type parameter mismatch *)
;;

(* In a signature the type does not have to be open *)

module type S =
sig
  type foo
  type foo += A of float
end
;;

(* But it must still be extensible *)

module type S =
sig
  type foo = A of int
  type foo += B of float (* Error foo does not have an extensible type *)
end
;;

(* Signatures can change the grouping of extensions *)

type foo = ..
;;

module M = struct
  type foo +=
      A of int
    | B of string

  type foo +=
      C of int
    | D of float
end
;;

module type S = sig
  type foo +=
      B of string
    | C of int

  type foo += D of float

  type foo += A of int
end
;;

module M_S = (M : S)
;;

(* Extensions can be GADTs *)

type 'a foo = ..
;;

type _ foo +=
    A : int -> int foo
  | B : int foo
;;

let get_num : type a. a foo -> a -> a option = fun f i1 ->
    match f with
        A i2 -> Some (i1 + i2)
     |  _ -> None
;;

(* Extensions must obey constraints *)

type 'a foo = .. constraint 'a = [> `Var ]
;;

type 'a foo += A of 'a
;;

let a = A 9 (* ERROR: Constraints not met *)
;;

type 'a foo += B : int foo (* ERROR: Constraints not met *)
;;

(* Signatures can make an extension private *)

type foo = ..
;;

module M = struct type foo += A of int end
;;

let a1 = M.A 10
;;

module type S = sig type foo += private A of int end
;;

module M_S = (M : S)
;;

let is_s x =
  match x with
    M_S.A _ -> true
  | _ -> false
;;

let a2 = M_S.A 20 (* ERROR: Cannot create a value using a private constructor *)
;;

(* Extensions can be rebound *)

type foo = ..
;;

module M = struct type foo += A1 of int end
;;

type foo += A2 = M.A1
;;

type bar = ..
;;

type bar += A3 = M.A1    (* Error: rebind wrong type *)
;;

module M = struct type foo += private B1 of int end
;;

type foo += private B2 = M.B1
;;

type foo += B3 = M.B1  (* Error: rebind private extension *)
;;

type foo += C = Unknown  (* Error: unbound extension *)
;;

(* Extensions can be rebound even if type is closed *)

module M : sig type foo type foo += A1 of int end
  = struct type foo = .. type foo += A1 of int end

type M.foo += A2 = M.A1

(* Rebinding handles abbreviations *)

type 'a foo = ..
;;

type 'a foo1 = 'a foo = ..
;;

type 'a foo2 = 'a foo = ..
;;

type 'a foo1 +=
    A of int
  | B of 'a
  | C : int foo1
;;

type 'a foo2 +=
    D = A
  | E = B
  | F = C
;;

(* Extensions must obey variances *)

type +'a foo = ..
;;

type 'a foo += A of (int -> 'a)
;;

type 'a foo += B of ('a -> int)
    (* ERROR: Parameter variances are not satisfied *)
;;

type _ foo += C : ('a -> int) -> 'a foo
    (* ERROR: Parameter variances are not satisfied *)
;;

type 'a bar = ..
;;

type +'a bar += D of (int -> 'a) (* ERROR: type variances do not match *)
;;

(* Exceptions are compatible with extensions *)

module M : sig
  type exn +=
      Foo of int * float
    | Bar : 'a list -> exn
end = struct
  exception Bar : 'a list -> exn
  exception Foo of int * float
end
;;

module M : sig
  exception Bar : 'a list -> exn
  exception Foo of int * float
end = struct
  type exn +=
      Foo of int * float
    | Bar : 'a list -> exn
end
;;

exception Foo of int * float
;;

exception Bar : 'a list -> exn
;;

module M : sig
  type exn +=
      Foo of int * float
    | Bar : 'a list -> exn
end = struct
  exception Bar = Bar
  exception Foo = Foo
end
;;

(* Test toplevel printing *)

type foo = ..
;;

type foo +=
    Foo of int * int option
  | Bar of int option
;;

let x = Foo(3, Some 4), Bar(Some 5) (* Prints Foo and Bar successfully *)
;;

type foo += Foo of string
;;

let y = x (* Prints Bar but not Foo (which has been shadowed) *)
;;

exception Foo of int * int option
;;

exception Bar of int option
;;

let x = Foo(3, Some 4), Bar(Some 5) (* Prints Foo and Bar successfully *)
;;

type foo += Foo of string
;;

let y = x (* Prints Bar and part of Foo (which has been shadowed) *)
;;

(* Test Obj functions *)

type foo = ..
;;

type foo +=
    Foo
  | Bar of int
;;

let extension_name e = Obj.extension_name (Obj.extension_constructor e);;
let extension_id e = Obj.extension_id (Obj.extension_constructor e);;

let n1 = extension_name Foo
;;

let n2 = extension_name (Bar 1)
;;

let t = (extension_id (Bar 2)) = (extension_id (Bar 3)) (* true *)
;;

let f = (extension_id (Bar 2)) = (extension_id Foo) (* false *)
;;

let is_foo x = (extension_id Foo) = (extension_id x)

type foo += Foo
;;

let f = is_foo Foo
;;

let _ = Obj.extension_constructor 7 (* Invald_arg *)
;;

let _ = Obj.extension_constructor (object method m = 3 end) (* Invald_arg *)
;;
(* Typed names *)

module Msg : sig

  type 'a tag

  type result = Result : 'a tag * 'a -> result

  val write : 'a tag -> 'a -> unit

  val read : unit -> result

  type 'a tag += Int : int tag

  module type Desc = sig
    type t
    val label : string
    val write : t -> string
    val read : string -> t
  end

  module Define (D : Desc) : sig
    type 'a tag += C : D.t tag
  end

end = struct

  type 'a tag = ..

  type ktag = T : 'a tag -> ktag

  type 'a kind =
  { tag : 'a tag;
    label : string;
    write : 'a -> string;
    read : string -> 'a; }

  type rkind = K : 'a kind -> rkind

  type wkind = { f : 'a . 'a tag -> 'a kind }

  let readTbl : (string, rkind) Hashtbl.t = Hashtbl.create 13

  let writeTbl : (ktag, wkind) Hashtbl.t = Hashtbl.create 13

  let read_raw () : string * string = raise (Failure "Not implemented")

  type result = Result : 'a tag * 'a -> result

  let read () =
    let label, content = read_raw () in
      let K k = Hashtbl.find readTbl label in
        let body = k.read content in
          Result(k.tag, body)

  let write_raw (label : string) (content : string) =
    raise (Failure "Not implemented")

  let write (tag : 'a tag) (body : 'a) =
    let {f} = Hashtbl.find writeTbl (T tag) in
    let k = f tag in
    let content = k.write body in
      write_raw k.label content

  (* Add int kind *)

  type 'a tag += Int : int tag

  let ik =
    { tag = Int;
      label = "int";
      write = string_of_int;
      read = int_of_string }

  let () = Hashtbl.add readTbl "int" (K ik)

  let () =
    let f (type t) (i : t tag) : t kind =
      match i with
        Int -> ik
      | _ -> assert false
    in
      Hashtbl.add writeTbl (T Int) {f}

  (* Support user defined kinds *)

  module type Desc = sig
    type t
    val label : string
    val write : t -> string
    val read : string -> t
  end

  module Define (D : Desc) = struct
    type 'a tag += C : D.t tag
    let k =
      { tag = C;
        label = D.label;
        write = D.write;
        read = D.read }
    let () = Hashtbl.add readTbl D.label (K k)
    let () =
      let f (type t) (c : t tag) : t kind =
        match c with
          C -> k
        | _ -> assert false
      in
        Hashtbl.add writeTbl (T C) {f}
  end

end;;

let write_int i = Msg.write Msg.Int i;;

module StrM = Msg.Define(struct
  type t = string
  let label = "string"
  let read s = s
  let write s = s
end);;

type 'a Msg.tag += String = StrM.C;;

let write_string s = Msg.write String s;;

let read_one () =
  let Msg.Result(tag, body) = Msg.read () in
  match tag with
    Msg.Int -> print_int body
  | String -> print_string body
  | _ -> print_string "Unknown";;
(* Example of algorithm parametrized with modules *)

let sort (type s) set l =
  let module Set = (val set : Set.S with type elt = s) in
  Set.elements (List.fold_right Set.add l Set.empty)

let make_set (type s) cmp =
  let module S = Set.Make(struct
    type t = s
    let compare = cmp
  end) in
  (module S : Set.S with type elt = s)

let both l =
  List.map
    (fun set -> sort set l)
    [ make_set compare; make_set (fun x y -> compare y x) ]

let () =
  print_endline (String.concat "  " (List.map (String.concat "/")
                                              (both ["abc";"xyz";"def"])))


(* Hiding the internal representation *)

module type S = sig
  type t
  val to_string: t -> string
  val apply: t -> t
  val x: t
end

let create (type s) to_string apply x =
  let module M = struct
    type t = s
    let to_string = to_string
    let apply = apply
    let x = x
  end in
  (module M : S with type t = s)

let forget (type s) x =
  let module M = (val x : S with type t = s) in
  (module M : S)

let print x =
  let module M = (val x : S) in
  print_endline (M.to_string M.x)

let apply x =
  let module M = (val x : S) in
  let module N = struct
    include M
    let x = apply x
  end in
  (module N : S)

let () =
  let int = forget (create string_of_int succ 0) in
  let str = forget (create (fun s -> s) (fun s -> s ^ s) "X") in
  List.iter print (List.map apply [int; apply int; apply (apply str)])


(* Existential types + type equality witnesses -> pseudo GADT *)

module TypEq : sig
  type ('a, 'b) t
  val apply: ('a, 'b) t -> 'a -> 'b
  val refl: ('a, 'a) t
  val sym: ('a, 'b) t -> ('b, 'a) t
end = struct
  type ('a, 'b) t = unit
  let apply _ = Obj.magic
  let refl = ()
  let sym () = ()
end


module rec Typ : sig
  module type PAIR = sig
    type t
    type t1
    type t2
    val eq: (t, t1 * t2) TypEq.t
    val t1: t1 Typ.typ
    val t2: t2 Typ.typ
  end

  type 'a typ =
    | Int of ('a, int) TypEq.t
    | String of ('a, string) TypEq.t
    | Pair of (module PAIR with type t = 'a)
end = struct
  module type PAIR = sig
    type t
    type t1
    type t2
    val eq: (t, t1 * t2) TypEq.t
    val t1: t1 Typ.typ
    val t2: t2 Typ.typ
  end

  type 'a typ =
    | Int of ('a, int) TypEq.t
    | String of ('a, string) TypEq.t
    | Pair of (module PAIR with type t = 'a)
end

open Typ

let int = Int TypEq.refl

let str = String TypEq.refl

let pair (type s1) (type s2) t1 t2 =
  let module P = struct
    type t = s1 * s2
    type t1 = s1
    type t2 = s2
    let eq = TypEq.refl
    let t1 = t1
    let t2 = t2
  end in
  let pair = (module P : PAIR with type t = s1 * s2) in
  Pair pair

module rec Print : sig
  val to_string: 'a Typ.typ -> 'a -> string
end = struct
  let to_string (type s) t x =
    match t with
    | Int eq -> string_of_int (TypEq.apply eq x)
    | String eq -> Printf.sprintf "%S" (TypEq.apply eq x)
    | Pair p ->
        let module P = (val p : PAIR with type t = s) in
        let (x1, x2) = TypEq.apply P.eq x in
        Printf.sprintf "(%s,%s)" (Print.to_string P.t1 x1)
                       (Print.to_string P.t2 x2)
end

let () =
  print_endline (Print.to_string int 10);
  print_endline (Print.to_string (pair int (pair str int)) (123, ("A", 456)))


(* #6262: first-class modules and module type aliases *)

module type S1 = sig end
module type S2 = S1

let _f (x : (module S1)) : (module S2) = x

module X = struct
  module type S
end
module Y = struct include X end

let _f (x : (module X.S)) : (module Y.S) = x

(* PR#6194, main example *)
module type S3 = sig val x : bool end;;
let f = function
  | Some (module M : S3) when M.x ->1
  | Some _ [@foooo]-> 2
  | None -> 3
;;
print_endline (string_of_int (f (Some (module struct let x = false end))));;
type 'a ty =
  | Int : int ty
  | Bool : bool ty

let fbool (type t) (x : t) (tag : t ty) =
  match tag with
  | Bool -> x
;;
(* val fbool : 'a -> 'a ty -> 'a = <fun> *)
(** OK: the return value is x of type t **)

let fint (type t) (x : t) (tag : t ty) =
  match tag with
  | Int -> x > 0
;;
(* val fint : 'a -> 'a ty -> bool = <fun> *)
(** OK: the return value is x > 0 of type bool;
This has used the equation t = bool, not visible in the return type **)

let f (type t) (x : t) (tag : t ty) =
  match tag with
  | Int -> x > 0
  | Bool -> x
(* val f : 'a -> 'a ty -> bool = <fun> *)


let g (type t) (x : t) (tag : t ty) =
  match tag with
  | Bool -> x
  | Int -> x > 0
(* Error: This expression has type bool but an expression was expected of type
t = int *)

let id x = x;;
let idb1 = (fun id -> let _ = id true in id) id;;
let idb2 : bool -> bool = id;;
let idb3 ( _ : bool ) = false;;

let g (type t) (x : t) (tag : t ty) =
  match tag with
  | Bool -> idb3 x
  | Int -> x > 0

let g (type t) (x : t) (tag : t ty) =
  match tag with
  | Bool -> idb2 x
  | Int -> x > 0
(* Encoding generics using GADTs *)
(* (c) Alain Frisch / Lexifi *)
(* cf. http://www.lexifi.com/blog/dynamic-types *)

(* Basic tag *)

type 'a ty =
  | Int: int ty
  | String: string ty
  | List: 'a ty -> 'a list ty
  | Pair: ('a ty * 'b ty) -> ('a * 'b) ty
;;

(* Tagging data *)

type variant =
  | VInt of int
  | VString of string
  | VList of variant list
  | VPair of variant * variant

let rec variantize: type t. t ty -> t -> variant =
  fun ty x ->
    (* type t is abstract here *)
    match ty with
    | Int -> VInt x  (* in this branch: t = int *)
    | String -> VString x (* t = string *)
    | List ty1 ->
        VList (List.map (variantize ty1) x)
        (* t = 'a list for some 'a *)
    | Pair (ty1, ty2) ->
        VPair (variantize ty1 (fst x), variantize ty2 (snd x))
        (* t = ('a, 'b) for some 'a and 'b *)

exception VariantMismatch

let rec devariantize: type t. t ty -> variant -> t =
  fun ty v ->
    match ty, v with
    | Int, VInt x -> x
    | String, VString x -> x
    | List ty1, VList vl ->
        List.map (devariantize ty1) vl
    | Pair (ty1, ty2), VPair (x1, x2) ->
        (devariantize ty1 x1, devariantize ty2 x2)
    | _ -> raise VariantMismatch
;;

(* Handling records *)

type 'a ty =
  | Int: int ty
  | String: string ty
  | List: 'a ty -> 'a list ty
  | Pair: ('a ty * 'b ty) -> ('a * 'b) ty
  | Record: 'a record -> 'a ty

and 'a record =
    {
     path: string;
     fields: 'a field_ list;
    }

and 'a field_ =
  | Field: ('a, 'b) field -> 'a field_

and ('a, 'b) field =
    {
     label: string;
     field_type: 'b ty;
     get: ('a -> 'b);
    }
;;

(* Again *)

type variant =
  | VInt of int
  | VString of string
  | VList of variant list
  | VPair of variant * variant
  | VRecord of (string * variant) list

let rec variantize: type t. t ty -> t -> variant =
  fun ty x ->
    (* type t is abstract here *)
    match ty with
    | Int -> VInt x  (* in this branch: t = int *)
    | String -> VString x (* t = string *)
    | List ty1 ->
        VList (List.map (variantize ty1) x)
        (* t = 'a list for some 'a *)
    | Pair (ty1, ty2) ->
        VPair (variantize ty1 (fst x), variantize ty2 (snd x))
        (* t = ('a, 'b) for some 'a and 'b *)
    | Record {fields} ->
        VRecord
          (List.map (fun (Field{field_type; label; get}) ->
                       (label, variantize field_type (get x))) fields)
;;

(* Extraction *)

type 'a ty =
  | Int: int ty
  | String: string ty
  | List: 'a ty -> 'a list ty
  | Pair: ('a ty * 'b ty) -> ('a * 'b) ty
  | Record: ('a, 'builder) record -> 'a ty

and ('a, 'builder) record =
    {
     path: string;
     fields: ('a, 'builder) field list;
     create_builder: (unit -> 'builder);
     of_builder: ('builder -> 'a);
    }

and ('a, 'builder) field =
  | Field: ('a, 'builder, 'b) field_ -> ('a, 'builder) field

and ('a, 'builder, 'b) field_ =
  {
   label: string;
   field_type: 'b ty;
   get: ('a -> 'b);
   set: ('builder -> 'b -> unit);
  }

let rec devariantize: type t. t ty -> variant -> t =
  fun ty v ->
    match ty, v with
    | Int, VInt x -> x
    | String, VString x -> x
    | List ty1, VList vl ->
        List.map (devariantize ty1) vl
    | Pair (ty1, ty2), VPair (x1, x2) ->
        (devariantize ty1 x1, devariantize ty2 x2)
    | Record {fields; create_builder; of_builder}, VRecord fl ->
        if List.length fields <> List.length fl then raise VariantMismatch;
        let builder = create_builder () in
        List.iter2
          (fun (Field {label; field_type; set}) (lab, v) ->
            if label <> lab then raise VariantMismatch;
            set builder (devariantize field_type v)
          )
          fields fl;
        of_builder builder
    | _ -> raise VariantMismatch
;;

type my_record  =
    {
     a: int;
     b: string list;
    }

let my_record =
  let fields =
    [
     Field {label = "a"; field_type = Int;
            get = (fun {a} -> a);
            set = (fun (r, _) x -> r := Some x)};
     Field {label = "b"; field_type = List String;
            get = (fun {b} -> b);
            set = (fun (_, r) x -> r := Some x)};
    ]
  in
  let create_builder () = (ref None, ref None) in
  let of_builder (a, b) =
    match !a, !b with
    | Some a, Some b -> {a; b}
    | _ -> failwith "Some fields are missing in record of type my_record"
  in
  Record {path = "My_module.my_record"; fields; create_builder; of_builder}
;;

(* Extension to recursive types and polymorphic variants *)
(* by Jacques Garrigue *)

type noarg = Noarg

type (_,_) ty =
  | Int: (int,_) ty
  | String: (string,_) ty
  | List: ('a,'e) ty -> ('a list, 'e) ty
  | Option: ('a,'e) ty -> ('a option, 'e) ty
  | Pair: (('a,'e) ty * ('b,'e) ty) -> ('a * 'b,'e) ty
  (* Support for type variables and recursive types *)
  | Var: ('a, 'a -> 'e) ty
  | Rec: ('a, 'a -> 'e) ty -> ('a,'e) ty
  | Pop: ('a, 'e) ty -> ('a, 'b -> 'e) ty
  (* Change the representation of a type *)
  | Conv: string * ('a -> 'b) * ('b -> 'a) * ('b, 'e) ty -> ('a, 'e) ty
  (* Sum types (both normal sums and polymorphic variants) *)
  | Sum: ('a, 'e, 'b) ty_sum -> ('a, 'e) ty

and ('a, 'e, 'b) ty_sum =
    { sum_proj: 'a -> string * 'e ty_dyn option;
      sum_cases: (string * ('e,'b) ty_case) list;
      sum_inj: 'c. ('b,'c) ty_sel * 'c -> 'a; }

and 'e ty_dyn =              (* dynamic type *)
  | Tdyn : ('a,'e) ty * 'a -> 'e ty_dyn

and (_,_) ty_sel =           (* selector from a list of types *)
  | Thd : ('a -> 'b, 'a) ty_sel
  | Ttl : ('b -> 'c, 'd) ty_sel -> ('a -> 'b -> 'c, 'd) ty_sel

and (_,_) ty_case =          (* type a sum case *)
  | TCarg : ('b,'a) ty_sel * ('a,'e) ty -> ('e,'b) ty_case
  | TCnoarg : ('b,noarg) ty_sel -> ('e,'b) ty_case
;;

type _ ty_env =              (* type variable substitution *)
  | Enil : unit ty_env
  | Econs : ('a,'e) ty * 'e ty_env -> ('a -> 'e) ty_env
;;

(* Comparing selectors *)
type (_,_) eq = Eq: ('a,'a) eq

let rec eq_sel : type a b c. (a,b) ty_sel -> (a,c) ty_sel -> (b,c) eq option =
  fun s1 s2 ->
    match s1, s2 with
    | Thd, Thd -> Some Eq
    | Ttl s1, Ttl s2 ->
        (match eq_sel s1 s2 with None -> None | Some Eq -> Some Eq)
    | _ -> None

(* Auxiliary function to get the type of a case from its selector *)
let rec get_case : type a b e.
  (b, a) ty_sel -> (string * (e,b) ty_case) list -> string * (a, e) ty option =
  fun sel cases ->
  match cases with
  | (name, TCnoarg sel') :: rem ->
      begin match eq_sel sel sel' with
      | None -> get_case sel rem
      | Some Eq -> name, None
      end
  | (name, TCarg (sel', ty)) :: rem ->
      begin match eq_sel sel sel' with
      | None -> get_case sel rem
      | Some Eq -> name, Some ty
      end
  | [] -> raise Not_found
;;

(* Untyped representation of values *)
type variant =
  | VInt of int
  | VString of string
  | VList of variant list
  | VOption of variant option
  | VPair of variant * variant
  | VConv of string * variant
  | VSum of string * variant option

let may_map f = function Some x -> Some (f x) | None -> None

let rec variantize : type a e. e ty_env -> (a,e) ty -> a -> variant =
  fun e ty v ->
  match ty with
  | Int -> VInt v
  | String -> VString v
  | List t -> VList (List.map (variantize e t) v)
  | Option t -> VOption (may_map (variantize e t) v)
  | Pair (t1, t2) -> VPair (variantize e t1 (fst v), variantize e t2 (snd v))
  | Rec t -> variantize (Econs (ty, e)) t v
  | Pop t -> (match e with Econs (_, e') -> variantize e' t v)
  | Var -> (match e with Econs (t, e') -> variantize e' t v)
  | Conv (s, proj, inj, t) -> VConv (s, variantize e t (proj v))
  | Sum ops ->
      let tag, arg = ops.sum_proj v in
      VSum (tag, may_map (function Tdyn (ty,arg) -> variantize e ty arg) arg)
;;

let rec devariantize : type t e. e ty_env -> (t, e) ty -> variant -> t =
  fun e ty v ->
  match ty, v with
  | Int, VInt x -> x
  | String, VString x -> x
  | List ty1, VList vl ->
      List.map (devariantize e ty1) vl
  | Pair (ty1, ty2), VPair (x1, x2) ->
      (devariantize e ty1 x1, devariantize e ty2 x2)
  | Rec t, _ -> devariantize (Econs (ty, e)) t v
  | Pop t, _ -> (match e with Econs (_, e') -> devariantize e' t v)
  | Var, _ -> (match e with Econs (t, e') -> devariantize e' t v)
  | Conv (s, proj, inj, t), VConv (s', v) when s = s' ->
      inj (devariantize e t v)
  | Sum ops, VSum (tag, a) ->
      begin try match List.assoc tag ops.sum_cases, a with
      | TCarg (sel, t), Some a -> ops.sum_inj (sel, devariantize e t a)
      | TCnoarg sel, None -> ops.sum_inj (sel, Noarg)
      | _ -> raise VariantMismatch
      with Not_found -> raise VariantMismatch
      end
  | _ -> raise VariantMismatch
;;

(* First attempt: represent 1-constructor variants using Conv *)
let wrap_A t = Conv ("`A", (fun (`A x) -> x), (fun x -> `A x), t);;

let ty a = Rec (wrap_A (Option (Pair (a, Var)))) ;;
let v = variantize Enil (ty Int);;
let x = v (`A (Some (1, `A (Some (2, `A None))))) ;;

(* Can also use it to decompose a tuple *)

let triple t1 t2 t3 =
  Conv ("Triple", (fun (a,b,c) -> (a,(b,c))),
        (fun (a,(b,c)) -> (a,b,c)), Pair (t1, Pair (t2, t3)))

let v = variantize Enil (triple String Int Int) ("A", 2, 3) ;;

(* Second attempt: introduce a real sum construct *)
let ty_abc =
  (* Could also use [get_case] for proj, but direct definition is shorter *)
  let proj = function
      `A n -> "A", Some (Tdyn (Int, n))
    | `B s -> "B", Some (Tdyn (String, s))
    | `C   -> "C", None
  (* Define inj in advance to be able to write the type annotation easily *)
  and inj : type c. (int -> string -> noarg -> unit, c) ty_sel * c ->
    [`A of int | `B of string | `C] = function
        Thd, v -> `A v
      | Ttl Thd, v -> `B v
      | Ttl (Ttl Thd), Noarg -> `C
  in
  (* Coherence of sum_inj and sum_cases is checked by the typing *)
  Sum { sum_proj = proj; sum_inj = inj; sum_cases =
        [ "A", TCarg (Thd, Int); "B", TCarg (Ttl Thd, String);
          "C", TCnoarg (Ttl (Ttl Thd)) ] }
;;

let v = variantize Enil ty_abc (`A 3)
let a = devariantize Enil ty_abc v

(* And an example with recursion... *)
type 'a vlist = [`Nil | `Cons of 'a * 'a vlist]

let ty_list : type a e. (a, e) ty -> (a vlist, e) ty = fun t ->
  let tcons = Pair (Pop t, Var) in
  Rec (Sum {
       sum_proj = (function
           `Nil -> "Nil", None
         | `Cons p -> "Cons", Some (Tdyn (tcons, p)));
       sum_cases = ["Nil", TCnoarg Thd; "Cons", TCarg (Ttl Thd, tcons)];
       sum_inj = fun (type c) ->
         (function
         | Thd, Noarg -> `Nil
         | Ttl Thd, v -> `Cons v
         : (noarg -> a * a vlist -> unit, c) ty_sel * c -> a vlist)
         (* One can also write the type annotation directly *)
     })

let v = variantize Enil (ty_list Int) (`Cons (1, `Cons (2, `Nil))) ;;


(* Simpler but weaker approach *)

type (_,_) ty =
  | Int: (int,_) ty
  | String: (string,_) ty
  | List: ('a,'e) ty -> ('a list, 'e) ty
  | Option: ('a,'e) ty -> ('a option, 'e) ty
  | Pair: (('a,'e) ty * ('b,'e) ty) -> ('a * 'b,'e) ty
  | Var: ('a, 'a -> 'e) ty
  | Rec: ('a, 'a -> 'e) ty -> ('a,'e) ty
  | Pop: ('a, 'e) ty -> ('a, 'b -> 'e) ty
  | Conv: string * ('a -> 'b) * ('b -> 'a) * ('b, 'e) ty -> ('a, 'e) ty
  | Sum: ('a -> string * 'e ty_dyn option) * (string * 'e ty_dyn option -> 'a)
             -> ('a, 'e) ty
and 'e ty_dyn =
  | Tdyn : ('a,'e) ty * 'a -> 'e ty_dyn

let ty_abc : ([`A of int | `B of string | `C],'e) ty =
  (* Could also use [get_case] for proj, but direct definition is shorter *)
  Sum (
  (function
      `A n -> "A", Some (Tdyn (Int, n))
    | `B s -> "B", Some (Tdyn (String, s))
    | `C   -> "C", None),
  (function
      "A", Some (Tdyn (Int, n)) -> `A n
    | "B", Some (Tdyn (String, s)) -> `B s
    | "C", None -> `C
    | _ -> invalid_arg "ty_abc"))
;;

(* Breaks: no way to pattern-match on a full recursive type *)
let ty_list : type a e. (a,e) ty -> (a vlist,e) ty = fun t ->
  let targ = Pair (Pop t, Var) in
  Rec (Sum (
  (function `Nil -> "Nil", None
    | `Cons p -> "Cons", Some (Tdyn (targ, p))),
  (function "Nil", None -> `Nil
    | "Cons", Some (Tdyn (Pair (_, Var), (p : a * a vlist))) -> `Cons p)))
;;

(* Define Sum using object instead of record for first-class polymorphism *)

type (_,_) ty =
  | Int: (int,_) ty
  | String: (string,_) ty
  | List: ('a,'e) ty -> ('a list, 'e) ty
  | Option: ('a,'e) ty -> ('a option, 'e) ty
  | Pair: (('a,'e) ty * ('b,'e) ty) -> ('a * 'b,'e) ty
  | Var: ('a, 'a -> 'e) ty
  | Rec: ('a, 'a -> 'e) ty -> ('a,'e) ty
  | Pop: ('a, 'e) ty -> ('a, 'b -> 'e) ty
  | Conv: string * ('a -> 'b) * ('b -> 'a) * ('b, 'e) ty -> ('a, 'e) ty
  | Sum: < proj: 'a -> string * 'e ty_dyn option;
           cases: (string * ('e,'b) ty_case) list;
           inj: 'c. ('b,'c) ty_sel * 'c -> 'a >
          -> ('a, 'e) ty

and 'e ty_dyn =
  | Tdyn : ('a,'e) ty * 'a -> 'e ty_dyn

and (_,_) ty_sel =
  | Thd : ('a -> 'b, 'a) ty_sel
  | Ttl : ('b -> 'c, 'd) ty_sel -> ('a -> 'b -> 'c, 'd) ty_sel

and (_,_) ty_case =
  | TCarg : ('b,'a) ty_sel * ('a,'e) ty -> ('e,'b) ty_case
  | TCnoarg : ('b,noarg) ty_sel -> ('e,'b) ty_case
;;

let ty_abc : ([`A of int | `B of string | `C] as 'a, 'e) ty =
  Sum (object
    method proj = function
        `A n -> "A", Some (Tdyn (Int, n))
      | `B s -> "B", Some (Tdyn (String, s))
      | `C -> "C", None
    method cases =
      [ "A", TCarg (Thd, Int); "B", TCarg (Ttl Thd, String);
        "C", TCnoarg (Ttl (Ttl Thd)) ];
    method inj : type c.
        (int -> string -> noarg -> unit, c) ty_sel * c ->
          [`A of int | `B of string | `C] =
      function
        Thd, v -> `A v
      | Ttl Thd, v -> `B v
      | Ttl (Ttl Thd), Noarg -> `C
  end)

type 'a vlist = [`Nil | `Cons of 'a * 'a vlist]

let ty_list : type a e. (a, e) ty -> (a vlist, e) ty = fun t ->
  let tcons = Pair (Pop t, Var) in
  Rec (Sum (object
    method proj = function
        `Nil -> "Nil", None
      | `Cons p -> "Cons", Some (Tdyn (tcons, p))
    method cases = ["Nil", TCnoarg Thd; "Cons", TCarg (Ttl Thd, tcons)]
    method inj : type c.(noarg -> a * a vlist -> unit, c) ty_sel * c -> a vlist
    = function
      | Thd, Noarg -> `Nil
      | Ttl Thd, v -> `Cons v
  end))
;;

(*
type (_,_) ty_assoc =
  | Anil : (unit,'e) ty_assoc
  | Acons : string * ('a,'e) ty * ('b,'e) ty_assoc -> ('a -> 'b, 'e) ty_assoc

and (_,_) ty_pvar =
  | Pnil : ('a,'e) ty_pvar
  | Pconst : 't * ('b,'e) ty_pvar -> ('t -> 'b, 'e) ty_pvar
  | Parg : 't * ('a,'e) ty * ('b,'e) ty_pvar -> ('t * 'a -> 'b, 'e) ty_pvar
*)
(*
   An attempt at encoding omega examples from the 2nd Central European
   Functional Programming School:
     Generic Programming in Omega, by Tim Sheard and Nathan Linger
          http://web.cecs.pdx.edu/~sheard/
*)

(* Basic types *)

type ('a,'b) sum = Inl of 'a | Inr of 'b

type zero = Zero
type 'a succ = Succ of 'a
type _ nat =
  | NZ : zero nat
  | NS : 'a nat -> 'a succ nat
;;

(* 2: A simple example *)

type (_,_) seq =
  | Snil  : ('a,zero) seq
  | Scons : 'a * ('a,'n) seq -> ('a, 'n succ) seq
;;

let l1 = Scons (3, Scons (5, Snil)) ;;

(* We do not have type level functions, so we need to use witnesses. *)
(* We copy here the definitions from section 3.9 *)
(* Note the addition of the ['a nat] argument to PlusZ, since we do not
   have kinds *)
type (_,_,_) plus =
  | PlusZ : 'a nat -> (zero, 'a, 'a) plus
  | PlusS : ('a,'b,'c) plus -> ('a succ, 'b, 'c succ) plus
;;

let rec length : type a n. (a,n) seq -> n nat = function
  | Snil -> NZ
  | Scons (_, s) -> NS (length s)
;;

(* app returns the catenated lists with a witness proving that
   the size is the sum of its two inputs *)
type (_,_,_) app = App : ('a,'p) seq * ('n,'m,'p) plus -> ('a,'n,'m) app

let rec app : type a n m. (a,n) seq -> (a,m) seq -> (a,n,m) app =
  fun xs ys ->
  match xs with
  | Snil -> App (ys, PlusZ (length ys))
  | Scons (x, xs') ->
      let App (xs'', pl) = app xs' ys in
      App (Scons (x, xs''), PlusS pl)
;;

(* 3.1 Feature: kinds *)

(* We do not have kinds, but we can encode them as predicates *)

type tp = TP
type nd = ND
type ('a,'b) fk = FK
type _ shape =
  | Tp : tp shape
  | Nd : nd shape
  | Fk : 'a shape * 'b shape -> ('a,'b) fk shape
;;
type tt = TT
type ff = FF
type _ boolean =
  | BT : tt boolean
  | BF : ff boolean
;;

(* 3.3 Feature : GADTs *)

type (_,_) path =
  | Pnone : 'a -> (tp,'a) path
  | Phere : (nd,'a) path
  | Pleft : ('x,'a) path -> (('x,'y) fk, 'a) path
  | Pright : ('y,'a) path -> (('x,'y) fk, 'a) path
;;
type (_,_) tree =
  | Ttip  : (tp,'a) tree
  | Tnode : 'a -> (nd,'a) tree
  | Tfork : ('x,'a) tree * ('y,'a) tree -> (('x,'y)fk, 'a) tree
;;
let tree1 = Tfork (Tfork (Ttip, Tnode 4), Tfork (Tnode 4, Tnode 3))
;;
let rec find : type sh.
    ('a -> 'a -> bool) -> 'a -> (sh,'a) tree -> (sh,'a) path list
  = fun eq n t ->
    match t with
    | Ttip -> []
    | Tnode m ->
        if eq n m then [Phere] else []
    | Tfork (x, y) ->
        List.map (fun x -> Pleft x) (find eq n x) @
        List.map (fun x -> Pright x) (find eq n y)
;;
let rec extract : type sh. (sh,'a) path -> (sh,'a) tree -> 'a = fun p t ->
  match (p, t) with
  | Pnone x, Ttip -> x
  | Phere, Tnode y -> y
  | Pleft p, Tfork(l,_) -> extract p l
  | Pright p, Tfork(_,r) -> extract p r
;;

(* 3.4 Pattern : Witness *)

type (_,_) le =
  | LeZ : 'a nat -> (zero, 'a) le
  | LeS : ('n, 'm) le -> ('n succ, 'm succ) le
;;
type _ even =
  | EvenZ : zero even
  | EvenSS : 'n even -> 'n succ succ even
;;
type one = zero succ
type two = one succ
type three = two succ
type four = three succ
;;
let even0 : zero even = EvenZ
let even2 : two even = EvenSS EvenZ
let even4 : four even = EvenSS (EvenSS EvenZ)
;;
let p1 : (two, one, three) plus = PlusS (PlusS (PlusZ (NS NZ)))
;;
let rec summandLessThanSum : type a b c. (a,b,c) plus -> (a,c) le = fun p ->
  match p with
  | PlusZ n -> LeZ n
  | PlusS p' -> LeS (summandLessThanSum p')
;;

(* 3.8 Pattern: Leibniz Equality *)

type (_,_) equal = Eq : ('a,'a) equal

let convert : type a b. (a,b) equal -> a -> b = fun Eq x -> x

let rec sameNat : type a b. a nat -> b nat -> (a,b) equal option = fun a b ->
  match a, b with
  | NZ, NZ -> Some Eq
  | NS a', NS b' ->
      begin match sameNat a' b' with
      | Some Eq -> Some Eq
      | None -> None
      end
  | _ -> None
;;

(* Extra: associativity of addition *)

let rec plus_func : type a b m n.
  (a,b,m) plus -> (a,b,n) plus -> (m,n) equal =
  fun p1 p2 ->
  match p1, p2 with
  | PlusZ _, PlusZ _ -> Eq
  | PlusS p1', PlusS p2' ->
      let Eq = plus_func p1' p2' in Eq

let rec plus_assoc : type a b c ab bc m n.
  (a,b,ab) plus -> (ab,c,m) plus ->
  (b,c,bc) plus -> (a,bc,n) plus -> (m,n) equal = fun p1 p2 p3 p4 ->
  match p1, p4 with
  | PlusZ b, PlusZ bc ->
      let Eq = plus_func p2 p3 in Eq
  | PlusS p1', PlusS p4' ->
      let PlusS p2' = p2 in
      let Eq = plus_assoc p1' p2' p3 p4' in Eq
;;

(* 3.9 Computing Programs and Properties Simultaneously *)

(* Plus and app1 are moved to section 2 *)

let smaller : type a b. (a succ, b succ) le -> (a,b) le =
  function LeS x -> x ;;

type (_,_) diff = Diff : 'c nat * ('a,'c,'b) plus -> ('a,'b) diff ;;

(*
let rec diff : type a b. (a,b) le -> a nat -> b nat -> (a,b) diff =
  fun le a b ->
  match a, b, le with
  | NZ, m, _ -> Diff (m, PlusZ m)
  | NS x, NZ, _ -> assert false
  | NS x, NS y, q ->
      match diff (smaller q) x y with Diff (m, p) -> Diff (m, PlusS p)
;;
*)

let rec diff : type a b. (a,b) le -> a nat -> b nat -> (a,b) diff =
  fun le a b ->
  match le, a, b with
  | LeZ _, _, m -> Diff (m, PlusZ m)
  | LeS q, NS x, NS y ->
      match diff q x y with Diff (m, p) -> Diff (m, PlusS p)
;;

let rec diff : type a b. (a,b) le -> a nat -> b nat -> (a,b) diff =
  fun le a b ->
  match a, b,le with (* warning *)
  | NZ, m, LeZ _ -> Diff (m, PlusZ m)
  | NS x, NS y, LeS q ->
      (match diff q x y with Diff (m, p) -> Diff (m, PlusS p))
  | _ -> .
;;

let rec diff : type a b. (a,b) le -> b nat -> (a,b) diff =
  fun le b ->
  match b,le with
  | m, LeZ _ -> Diff (m, PlusZ m)
  | NS y, LeS q ->
      match diff q y with Diff (m, p) -> Diff (m, PlusS p)
;;

type (_,_) filter = Filter : ('m,'n) le * ('a,'m) seq -> ('a,'n) filter

let rec leS' : type m n. (m,n) le -> (m,n succ) le = function
  | LeZ n -> LeZ (NS n)
  | LeS le -> LeS (leS' le)
;;

let rec filter : type a n. (a -> bool) -> (a,n) seq -> (a,n) filter =
  fun f s ->
  match s with
  | Snil -> Filter (LeZ NZ, Snil)
  | Scons (a,l) ->
      match filter f l with Filter (le, l') ->
        if f a then Filter (LeS le, Scons (a, l'))
        else Filter (leS' le, l')
;;

(* 4.1 AVL trees *)

type (_,_,_) balance =
  | Less : ('h, 'h succ, 'h succ) balance
  | Same : ('h, 'h, 'h) balance
  | More : ('h succ, 'h, 'h succ) balance

type _ avl =
  | Leaf : zero avl
  | Node :
      ('hL, 'hR, 'hMax) balance * 'hL avl * int * 'hR avl -> 'hMax succ avl

type avl' = Avl : 'h avl -> avl'
;;

let empty = Avl Leaf

let rec elem : type h. int -> h avl -> bool = fun x t ->
  match t with
  | Leaf -> false
  | Node (_, l, y, r) ->
      x = y || if x < y then elem x l else elem x r
;;

let rec rotr : type n. (n succ succ) avl -> int -> n avl ->
  ((n succ succ) avl, (n succ succ succ) avl) sum =
  fun tL y tR ->
  match tL with
  | Node (Same, a, x, b) -> Inr (Node (Less, a, x, Node (More, b, y, tR)))
  | Node (More, a, x, b) -> Inl (Node (Same, a, x, Node (Same, b, y, tR)))
  | Node (Less, a, x, Node (Same, b, z, c)) ->
      Inl (Node (Same, Node (Same, a, x, b), z, Node (Same, c, y, tR)))
  | Node (Less, a, x, Node (Less, b, z, c)) ->
      Inl (Node (Same, Node (More, a, x, b), z, Node (Same, c, y, tR)))
  | Node (Less, a, x, Node (More, b, z, c)) ->
      Inl (Node (Same, Node (Same, a, x, b), z, Node (Less, c, y, tR)))
;;
let rec rotl : type n. n avl -> int -> (n succ succ) avl ->
  ((n succ succ) avl, (n succ succ succ) avl) sum =
  fun tL u tR ->
  match tR with
  | Node (Same, a, x, b) -> Inr (Node (More, Node (Less, tL, u, a), x, b))
  | Node (Less, a, x, b) -> Inl (Node (Same, Node (Same, tL, u, a), x, b))
  | Node (More, Node (Same, a, x, b), y, c) ->
      Inl (Node (Same, Node (Same, tL, u, a), x, Node (Same, b, y, c)))
  | Node (More, Node (Less, a, x, b), y, c) ->
      Inl (Node (Same, Node (More, tL, u, a), x, Node (Same, b, y, c)))
  | Node (More, Node (More, a, x, b), y, c) ->
      Inl (Node (Same, Node (Same, tL, u, a), x, Node (Less, b, y, c)))
;;
let rec ins : type n. int -> n avl -> (n avl, (n succ) avl) sum =
  fun x t ->
  match t with
  | Leaf -> Inr (Node (Same, Leaf, x, Leaf))
  | Node (bal, a, y, b) ->
      if x = y then Inl t else
      if x < y then begin
        match ins x a with
        | Inl a -> Inl (Node (bal, a, y, b))
        | Inr a ->
            match bal with
            | Less -> Inl (Node (Same, a, y, b))
            | Same -> Inr (Node (More, a, y, b))
            | More -> rotr a y b
      end else begin
        match ins x b with
        | Inl b -> Inl (Node (bal, a, y, b) : n avl)
        | Inr b ->
            match bal with
            | More -> Inl (Node (Same, a, y, b) : n avl)
            | Same -> Inr (Node (Less, a, y, b) : n succ avl)
            | Less -> rotl a y b
      end
;;

let insert x (Avl t) =
  match ins x t with
  | Inl t -> Avl t
  | Inr t -> Avl t
;;

let rec del_min : type n. (n succ) avl -> int * (n avl, (n succ) avl) sum =
  function
  | Node (Less, Leaf, x, r) -> (x, Inl r)
  | Node (Same, Leaf, x, r) -> (x, Inl r)
  | Node (bal, (Node _ as l) , x, r) ->
      match del_min l with
      | y, Inr l -> (y, Inr (Node (bal, l, x, r)))
      | y, Inl l ->
          (y, match bal with
          | Same -> Inr (Node (Less, l, x, r))
          | More -> Inl (Node (Same, l, x, r))
          | Less -> rotl l x r)

type _ avl_del =
  | Dsame : 'n avl -> 'n avl_del
  | Ddecr : ('m succ, 'n) equal * 'm avl -> 'n avl_del

let rec del : type n. int -> n avl -> n avl_del = fun y t ->
  match t with
  | Leaf -> Dsame Leaf
  | Node (bal, l, x, r) ->
      if x = y then begin
        match r with
        | Leaf ->
            begin match bal with
            | Same -> Ddecr (Eq, l)
            | More -> Ddecr (Eq, l)
            end
        | Node _ ->
            begin match bal, del_min r with
            | _, (z, Inr r) -> Dsame (Node (bal, l, z, r))
            | Same, (z, Inl r) -> Dsame (Node (More, l, z, r))
            | Less, (z, Inl r) -> Ddecr (Eq, Node (Same, l, z, r))
            | More, (z, Inl r) ->
                match rotr l z r with
                | Inl t -> Ddecr (Eq, t)
                | Inr t -> Dsame t
            end
      end else if y < x then begin
        match del y l with
        | Dsame l -> Dsame (Node (bal, l, x, r))
        | Ddecr(Eq,l) ->
            begin match bal with
            | Same -> Dsame (Node (Less, l, x, r))
            | More -> Ddecr (Eq, Node (Same, l, x, r))
            | Less ->
                match rotl l x r with
                | Inl t -> Ddecr (Eq, t)
                | Inr t -> Dsame t
            end
      end else begin
        match del y r with
        | Dsame r -> Dsame (Node (bal, l, x, r))
        | Ddecr(Eq,r) ->
            begin match bal with
            | Same -> Dsame (Node (More, l, x, r))
            | Less -> Ddecr (Eq, Node (Same, l, x, r))
            | More ->
                match rotr l x r with
                | Inl t -> Ddecr (Eq, t)
                | Inr t -> Dsame t
            end
      end
;;

let delete x (Avl t) =
  match del x t with
  | Dsame t -> Avl t
  | Ddecr (_, t) -> Avl t
;;


(* Exercise 22: Red-black trees *)

type red = RED
type black = BLACK
type (_,_) sub_tree =
  | Bleaf : (black, zero) sub_tree
  | Rnode :
      (black, 'n) sub_tree * int * (black, 'n) sub_tree -> (red, 'n) sub_tree
  | Bnode :
      ('cL, 'n) sub_tree * int * ('cR, 'n) sub_tree -> (black, 'n succ) sub_tree

type rb_tree = Root : (black, 'n) sub_tree -> rb_tree
;;

type dir = LeftD | RightD

type (_,_) ctxt =
  | CNil : (black,'n) ctxt
  | CRed : int * dir * (black,'n) sub_tree * (red,'n) ctxt -> (black,'n) ctxt
  | CBlk : int * dir * ('c1,'n) sub_tree * (black, 'n succ) ctxt -> ('c,'n) ctxt
;;

let blacken = function
    Rnode (l, e, r) -> Bnode (l, e, r)

type _ crep =
  | Red : red crep
  | Black : black crep

let color : type c n. (c,n) sub_tree -> c crep = function
  | Bleaf -> Black
  | Rnode _ -> Red
  | Bnode _ -> Black
;;

let rec fill : type c n. (c,n) ctxt -> (c,n) sub_tree -> rb_tree =
  fun ct t ->
  match ct with
  | CNil -> Root t
  | CRed (e, LeftD, uncle, c) -> fill c (Rnode (uncle, e, t))
  | CRed (e, RightD, uncle, c) -> fill c (Rnode (t, e, uncle))
  | CBlk (e, LeftD, uncle, c) -> fill c (Bnode (uncle, e, t))
  | CBlk (e, RightD, uncle, c) -> fill c (Bnode (t, e, uncle))
;;
let recolor d1 pE sib d2 gE uncle t =
  match d1, d2 with
  | LeftD, RightD -> Rnode (Bnode (sib, pE, t), gE, uncle)
  | RightD, RightD -> Rnode (Bnode (t, pE, sib), gE, uncle)
  | LeftD, LeftD -> Rnode (uncle, gE, Bnode (sib, pE, t))
  | RightD, LeftD -> Rnode (uncle, gE, Bnode (t, pE, sib))
;;
let rotate d1 pE sib d2 gE uncle (Rnode (x, e, y)) =
  match d1, d2 with
  | RightD, RightD -> Bnode (Rnode (x,e,y), pE, Rnode (sib, gE, uncle))
  | LeftD,  RightD -> Bnode (Rnode (sib, pE, x), e, Rnode (y, gE, uncle))
  | LeftD,  LeftD  -> Bnode (Rnode (uncle, gE, sib), pE, Rnode (x,e,y))
  | RightD, LeftD  -> Bnode (Rnode (uncle, gE, x), e, Rnode (y, pE, sib))
;;
let rec repair : type c n. (red,n) sub_tree -> (c,n) ctxt -> rb_tree =
  fun t ct ->
  match ct with
  | CNil -> Root (blacken t)
  | CBlk (e, LeftD, sib, c) -> fill c (Bnode (sib, e, t))
  | CBlk (e, RightD, sib, c) -> fill c (Bnode (t, e, sib))
  | CRed (e, dir, sib, CBlk (e', dir', uncle, ct)) ->
      match color uncle with
      | Red -> repair (recolor dir e sib dir' e' (blacken uncle) t) ct
      | Black -> fill ct (rotate dir e sib dir' e' uncle t)
;;
let rec ins : type c n. int -> (c,n) sub_tree -> (c,n) ctxt -> rb_tree =
  fun e t ct ->
  match t with
  | Rnode (l, e', r) ->
      if e < e' then ins e l (CRed (e', RightD, r, ct))
                else ins e r (CRed (e', LeftD, l, ct))
  | Bnode (l, e', r) ->
      if e < e' then ins e l (CBlk (e', RightD, r, ct))
                else ins e r (CBlk (e', LeftD, l, ct))
  | Bleaf -> repair (Rnode (Bleaf, e, Bleaf)) ct
;;
let insert e (Root t) = ins e t CNil
;;

(* 5.7 typed object languages using GADTs *)

type _ term =
  | Const : int -> int term
  | Add   : (int * int -> int) term
  | LT    : (int * int -> bool) term
  | Ap    : ('a -> 'b) term * 'a term -> 'b term
  | Pair  : 'a term * 'b term -> ('a * 'b) term

let ex1 = Ap (Add, Pair (Const 3, Const 5))
let ex2 = Pair (ex1, Const 1)

let rec eval_term : type a. a term -> a = function
  | Const x -> x
  | Add -> fun (x,y) -> x+y
  | LT  -> fun (x,y) -> x<y
  | Ap(f,x) -> eval_term f (eval_term x)
  | Pair(x,y) -> (eval_term x, eval_term y)

type _ rep =
  | Rint  : int rep
  | Rbool : bool rep
  | Rpair : 'a rep * 'b rep -> ('a * 'b) rep
  | Rfun  : 'a rep * 'b rep -> ('a -> 'b) rep

type (_,_) equal = Eq : ('a,'a) equal

let rec rep_equal : type a b. a rep -> b rep -> (a, b) equal option =
  fun ra rb ->
  match ra, rb with
  | Rint, Rint -> Some Eq
  | Rbool, Rbool -> Some Eq
  | Rpair (a1, a2), Rpair (b1, b2) ->
      begin match rep_equal a1 b1 with
      | None -> None
      | Some Eq -> match rep_equal a2 b2 with
        | None -> None
        | Some Eq -> Some Eq
      end
  | Rfun (a1, a2), Rfun (b1, b2) ->
      begin match rep_equal a1 b1 with
      | None -> None
      | Some Eq -> match rep_equal a2 b2 with
        | None -> None
        | Some Eq -> Some Eq
      end
  | _ -> None
;;

type assoc = Assoc : string * 'a rep * 'a -> assoc

let rec assoc : type a. string -> a rep -> assoc list -> a =
  fun x r -> function
  | [] -> raise Not_found
  | Assoc (x', r', v) :: env ->
      if x = x' then
        match rep_equal r r' with
        | None -> failwith ("Wrong type for " ^ x)
        | Some Eq -> v
      else assoc x r env

type _ term =
  | Var   : string * 'a rep -> 'a term
  | Abs   : string * 'a rep * 'b term -> ('a -> 'b) term
  | Const : int -> int term
  | Add   : (int * int -> int) term
  | LT    : (int * int -> bool) term
  | Ap    : ('a -> 'b) term * 'a term -> 'b term
  | Pair  : 'a term * 'b term -> ('a * 'b) term

let rec eval_term : type a. assoc list -> a term -> a =
  fun env -> function
  | Var (x, r) -> assoc x r env
  | Abs (x, r, e) -> fun v -> eval_term (Assoc (x, r, v) :: env) e
  | Const x -> x
  | Add -> fun (x,y) -> x+y
  | LT  -> fun (x,y) -> x<y
  | Ap(f,x) -> eval_term env f (eval_term env x)
  | Pair(x,y) -> (eval_term env x, eval_term env y)
;;

let ex3 = Abs ("x", Rint, Ap (Add, Pair (Var("x",Rint), Var("x",Rint))))
let ex4 = Ap (ex3, Const 3)

let v4 = eval_term [] ex4
;;

(* 5.9/5.10 Language with binding *)

type rnil = RNIL
type ('a,'b,'c) rcons = RCons of 'a * 'b * 'c

type _ is_row =
  | Rnil  : rnil is_row
  | Rcons : 'c is_row -> ('a,'b,'c) rcons is_row

type (_,_) lam =
  | Const : int -> ('e, int) lam
  | Var : 'a -> (('a,'t,'e) rcons, 't) lam
  | Shift : ('e,'t) lam -> (('a,'q,'e) rcons, 't) lam
  | Abs : 'a * (('a,'s,'e) rcons, 't) lam -> ('e, 's -> 't) lam
  | App : ('e, 's -> 't) lam * ('e, 's) lam -> ('e, 't) lam

type x = X
type y = Y

let ex1 = App (Var X, Shift (Var Y))
let ex2 = Abs (X, Abs (Y, App (Shift (Var X), Var Y)))
;;

type _ env =
  | Enil : rnil env
  | Econs : 'a * 't * 'e env -> ('a, 't, 'e) rcons env

let rec eval_lam : type e t. e env -> (e, t) lam -> t =
  fun env m ->
  match env, m with
  | _, Const n -> n
  | Econs (_, v, r), Var _ -> v
  | Econs (_, _, r), Shift e -> eval_lam r e
  | _, Abs (n, body) -> fun x -> eval_lam (Econs (n, x, env)) body
  | _, App (f, x)    -> eval_lam env f (eval_lam env x)
;;

type add = Add
type suc = Suc

let env0 = Econs (Zero, 0, Econs (Suc, succ, Econs (Add, (+), Enil)))

let _0 : (_, int) lam = Var Zero
let suc x = App (Shift (Var Suc : (_, int -> int) lam), x)
let _1 = suc _0
let _2 = suc _1
let _3 = suc _2
let add = Shift (Shift (Var Add : (_, int -> int -> int) lam))

let double = Abs (X, App (App (Shift add, Var X), Var X))
let ex3 = App (double, _3)
;;

let v3 = eval_lam env0 ex3
;;

(* 5.13: Constructing typing derivations at runtime *)

(* Modified slightly to use the language of 5.10, since this is more fun.
   Of course this works also with the language of 5.12. *)

type _ rep =
  | I : int rep
  | Ar : 'a rep * 'b rep -> ('a -> 'b) rep

let rec compare : type a b. a rep -> b rep -> (string, (a,b) equal) sum =
  fun a b ->
  match a, b with
  | I, I -> Inr Eq
  | Ar(x,y), Ar(s,t) ->
      begin match compare x s with
      | Inl _ as e -> e
      | Inr Eq -> match compare y t with
        | Inl _ as e -> e
        | Inr Eq as e -> e
      end
  | I, Ar _ -> Inl "I <> Ar _"
  | Ar _, I -> Inl "Ar _ <> I"
;;

type term =
  | C of int
  | Ab : string * 'a rep * term -> term
  | Ap of term * term
  | V of string

type _ ctx =
  | Cnil : rnil ctx
  | Ccons : 't * string * 'x rep * 'e ctx -> ('t,'x,'e) rcons ctx
;;

type _ checked =
  | Cerror of string
  | Cok : ('e,'t) lam * 't rep -> 'e checked

let rec lookup : type e. string -> e ctx -> e checked =
  fun name ctx ->
  match ctx with
  | Cnil -> Cerror ("Name not found: " ^ name)
  | Ccons (l,s,t,rs) ->
      if s = name then Cok (Var l,t) else
      match lookup name rs with
      | Cerror m -> Cerror m
      | Cok (v, t) -> Cok (Shift v, t)
;;

let rec tc : type n e. n nat -> e ctx -> term -> e checked =
  fun n ctx t ->
  match t with
  | V s -> lookup s ctx
  | Ap(f,x) ->
      begin match tc n ctx f with
      | Cerror _ as e -> e
      | Cok (f', ft) -> match tc n ctx x with
        | Cerror _ as e -> e
        | Cok (x', xt) ->
            match ft with
            | Ar (a, b) ->
                begin match compare a xt with
                | Inl s -> Cerror s
                | Inr Eq -> Cok (App (f',x'), b)
                end
            | _ -> Cerror "Non fun in Ap"
      end
  | Ab(s,t,body) ->
      begin match tc (NS n) (Ccons (n, s, t, ctx)) body with
      | Cerror _ as e -> e
      | Cok (body', et) -> Cok (Abs (n, body'), Ar (t, et))
      end
  | C m -> Cok (Const m, I)
;;

let ctx0 =
  Ccons (Zero, "0", I,
         Ccons (Suc, "S", Ar(I,I),
                Ccons (Add, "+", Ar(I,Ar(I,I)), Cnil)))

let ex1 = Ab ("x", I, Ap(Ap(V"+",V"x"),V"x"));;
let c1 = tc NZ ctx0 ex1;;
let ex2 = Ap (ex1, C 3);;
let c2 = tc NZ ctx0 ex2;;

let eval_checked env = function
  | Cerror s -> failwith s
  | Cok (e, I) -> (eval_lam env e : int)
  | Cok _ -> failwith "Can only evaluate expressions of type I"
;;

let v2 = eval_checked env0 c2 ;;

(* 5.12 Soundness *)

type pexp = PEXP
type pval = PVAL
type _ mode =
  | Pexp : pexp mode
  | Pval : pval mode

type ('a,'b) tarr = TARR
type tint = TINT

type (_,_) rel =
  | IntR : (tint, int) rel
  | IntTo : ('b, 's) rel -> ((tint, 'b) tarr, int -> 's) rel

type (_,_,_) lam =
  | Const : ('a,'b) rel * 'b -> (pval, 'env, 'a) lam
  | Var : 'a -> (pval, ('a,'t,'e) rcons, 't) lam
  | Shift : ('m,'e,'t) lam -> ('m, ('a,'q,'e) rcons, 't) lam
  | Lam : 'a * ('m, ('a,'s,'e) rcons, 't) lam -> (pval, 'e, ('s,'t) tarr) lam
  | App : ('m1, 'e, ('s,'t) tarr) lam * ('m2, 'e, 's) lam -> (pexp, 'e, 't) lam
;;

let ex1 = App (Lam (X, Var X), Const (IntR, 3))

let rec mode : type m e t. (m,e,t) lam -> m mode = function
  | Lam (v, body) -> Pval
  | Var v -> Pval
  | Const (r, v) -> Pval
  | Shift e -> mode e
  | App _ -> Pexp
;;

type (_,_) sub =
  | Id : ('r,'r) sub
  | Bind : 't * ('m,'r2,'x) lam * ('r,'r2) sub -> (('t,'x,'r) rcons, 'r2) sub
  | Push : ('r1,'r2) sub -> (('a,'b,'r1) rcons, ('a,'b,'r2) rcons) sub

type (_,_) lam' = Ex : ('m, 's, 't) lam -> ('s,'t) lam'
;;

let rec subst : type m1 r t s. (m1,r,t) lam -> (r,s) sub -> (s,t) lam' =
  fun t s ->
  match t, s with
  | _, Id -> Ex t
  | Const(r,c), sub -> Ex (Const (r,c))
  | Var v, Bind (x, e, r) -> Ex e
  | Var v, Push sub -> Ex (Var v)
  | Shift e, Bind (_, _, r) -> subst e r
  | Shift e, Push sub ->
      (match subst e sub with Ex a -> Ex (Shift a))
  | App(f,x), sub ->
      (match subst f sub, subst x sub with Ex g, Ex y -> Ex (App (g,y)))
  | Lam(v,x), sub ->
      (match subst x (Push sub) with Ex body -> Ex (Lam (v, body)))
;;

type closed = rnil

type 'a rlam = ((pexp,closed,'a) lam, (pval,closed,'a) lam) sum ;;

let rec rule : type a b.
  (pval, closed, (a,b) tarr) lam -> (pval, closed, a) lam -> b rlam =
  fun v1 v2 ->
  match v1, v2 with
  | Lam(x,body), v ->
      begin
        match subst body (Bind (x, v, Id)) with Ex term ->
        match mode term with
        | Pexp -> Inl term
        | Pval -> Inr term
      end
  | Const (IntTo b, f), Const (IntR, x) ->
      Inr (Const (b, f x))
;;
let rec onestep : type m t. (m,closed,t) lam -> t rlam = function
  | Lam (v, body) -> Inr (Lam (v, body))
  | Const (r, v)  -> Inr (Const (r, v))
  | App (e1, e2) ->
      match mode e1, mode e2 with
      | Pexp, _->
          begin match onestep e1 with
          | Inl e -> Inl(App(e,e2))
          | Inr v -> Inl(App(v,e2))
          end
      | Pval, Pexp ->
          begin match onestep e2 with
          | Inl e -> Inl(App(e1,e))
          | Inr v -> Inl(App(e1,v))
          end
      | Pval, Pval -> rule e1 e2
;;
type ('env, 'a) var =
 | Zero : ('a * 'env, 'a) var
 | Succ : ('env, 'a) var -> ('b * 'env, 'a) var
;;
type ('env, 'a) typ =
 | Tint : ('env, int) typ
 | Tbool : ('env, bool) typ
 | Tvar : ('env, 'a) var -> ('env, 'a) typ
;;
let f : type env a. (env, a) typ -> (env, a) typ -> int = fun ta tb ->
 match ta, tb with
   | Tint, Tint -> 0
   | Tbool, Tbool -> 1
   | Tvar var, tb -> 2
   | _ -> .   (* error *)
;;
(* let x = f Tint (Tvar Zero) ;; *)
type inkind = [ `Link | `Nonlink ]

type _ inline_t =
   | Text: string -> [< inkind > `Nonlink ] inline_t
   | Bold: 'a inline_t list -> 'a inline_t
   | Link: string -> [< inkind > `Link ] inline_t
   | Mref: string * [ `Nonlink ] inline_t list -> [< inkind > `Link ] inline_t
;;

let uppercase seq =
   let rec process: type a. a inline_t -> a inline_t = function
       | Text txt       -> Text (String.uppercase_ascii txt)
       | Bold xs        -> Bold (List.map process xs)
       | Link lnk       -> Link lnk
       | Mref (lnk, xs) -> Mref (lnk, List.map process xs)
   in List.map process seq
;;

type ast_t =
   | Ast_Text of string
   | Ast_Bold of ast_t list
   | Ast_Link of string
   | Ast_Mref of string * ast_t list
;;

let inlineseq_from_astseq seq =
   let rec process_nonlink = function
       | Ast_Text txt  -> Text txt
       | Ast_Bold xs   -> Bold (List.map process_nonlink xs)
       | _             -> assert false in
   let rec process_any = function
       | Ast_Text txt       -> Text txt
       | Ast_Bold xs        -> Bold (List.map process_any xs)
       | Ast_Link lnk       -> Link lnk
       | Ast_Mref (lnk, xs) -> Mref (lnk, List.map process_nonlink xs)
   in List.map process_any seq
;;

(* OK *)
type _ linkp =
 | Nonlink : [ `Nonlink ] linkp
 | Maylink : inkind linkp
;;
let inlineseq_from_astseq seq =
 let rec process : type a. a linkp -> ast_t -> a inline_t =
   fun allow_link ast ->
     match (allow_link, ast) with
     | (Maylink, Ast_Text txt)    -> Text txt
     | (Nonlink, Ast_Text txt)    -> Text txt
     | (x, Ast_Bold xs)           -> Bold (List.map (process x) xs)
     | (Maylink, Ast_Link lnk)    -> Link lnk
     | (Nonlink, Ast_Link _)      -> assert false
     | (Maylink, Ast_Mref (lnk, xs)) ->
         Mref (lnk, List.map (process Nonlink) xs)
     | (Nonlink, Ast_Mref _)      -> assert false
   in List.map (process Maylink) seq
;;

(* Bad *)
type _ linkp2 = Kind : 'a linkp -> ([< inkind ] as 'a) linkp2
;;
let inlineseq_from_astseq seq =
let rec process : type a. a linkp2 -> ast_t -> a inline_t =
  fun allow_link ast ->
    match (allow_link, ast) with
    | (Kind _, Ast_Text txt)    -> Text txt
    | (x, Ast_Bold xs)           -> Bold (List.map (process x) xs)
    | (Kind Maylink, Ast_Link lnk)    -> Link lnk
    | (Kind Nonlink, Ast_Link _)      -> assert false
    | (Kind Maylink, Ast_Mref (lnk, xs)) ->
        Mref (lnk, List.map (process (Kind Nonlink)) xs)
    | (Kind Nonlink, Ast_Mref _)      -> assert false
  in List.map (process (Kind Maylink)) seq
;;
module Add (T : sig type two end) =
struct
  type _ t =
  | One : [`One] t
  | Two : T.two t

  let add (type a) : a t * a t -> string = function
    | One, One -> "two"
    | Two, Two -> "four"
end;;
module B : sig
 type (_, _) t = Eq: ('a, 'a) t
 val f: 'a -> 'b -> ('a, 'b) t
end
=
struct
 type (_, _) t = Eq: ('a, 'a) t
 let f t1 t2 = Obj.magic Eq
end;;

let of_type: type a. a -> a = fun x ->
  match B.f x 4 with
  | Eq -> 5
;;
type _ constant =
  | Int: int -> int constant
  | Bool: bool -> bool constant

type (_, _, _) binop =
  | Eq: ('a, 'a, bool) binop
  | Leq: ('a, 'a, bool) binop
  | Add: (int, int, int) binop

let eval (type a) (type b) (type c) (bop:(a,b,c) binop) (x:a constant)
         (y:b constant) : c constant =
  match bop, x, y with
  | Eq, Bool x, Bool y -> Bool (if x then y else not y)
  | Leq, Int x, Int y -> Bool (x <= y)
  | Leq, Bool x, Bool y -> Bool (x <= y)
  | Add, Int x, Int y -> Int (x + y)

let _ = eval Eq (Int 2) (Int 3)
type tag = [`TagA | `TagB | `TagC];;

type 'a poly =
    AandBTags : [< `TagA of int | `TagB ] poly
  | ATag : [< `TagA of int] poly
(* constraint 'a = [< `TagA of int | `TagB] *)
;;

let intA = function `TagA i -> i
let intB = function `TagB -> 4
;;

let intAorB = function
    `TagA i -> i
  | `TagB -> 4
;;

type _ wrapPoly =
    WrapPoly : 'a poly -> ([< `TagA of int | `TagB] as 'a) wrapPoly
;;

let example6 : type a. a wrapPoly -> (a -> int) =
  fun w  ->
    match w with
    | WrapPoly ATag -> intA
    | WrapPoly _ -> intA (* This should not be allowed *)
;;

let _ =  example6 (WrapPoly AandBTags) `TagB (* This causes a seg fault *)
;;
module F(S : sig type 'a t end) = struct
  type _ ab =
      A : int S.t ab
    | B : float S.t ab

  let f : int S.t ab -> float S.t ab -> string =
    fun (l : int S.t ab) (r : float S.t ab) -> match l, r with
    | A, B -> "f A B"
end;;

module F(S : sig type 'a t end) = struct
  type a = int * int
  type b = int -> int

  type _ ab =
      A : a S.t ab
    | B : b S.t ab

  let f : a S.t ab -> b S.t ab -> string =
    fun l r -> match l, r with
    | A, B -> "f A B"
end;;
type (_, _) t =
    Any : ('a, 'b) t
  | Eq : ('a, 'a) t
;;

module M :
sig
  type s = private [> `A]
  val eq : (s, [`A | `B]) t
end =
struct
  type s = [`A | `B]
  let eq = Eq
end;;

let f : (M.s, [`A | `B]) t -> string = function
  | Any -> "Any"
;;

let () = print_endline (f M.eq) ;;

module N :
sig
  type s = private < a : int; .. >
  val eq : (s, <a : int; b : bool>) t
end =
struct
  type s = <a : int; b : bool>
  let eq = Eq
end
;;

let f : (N.s, <a : int; b : bool>) t -> string = function
  | Any -> "Any"
;;
type (_, _) comp =
  | Eq : ('a, 'a) comp
  | Diff : ('a, 'b) comp
;;

module U = struct type t = T end;;

module M : sig
  type t = T
  val comp : (U.t, t) comp
end = struct
  include U
  let comp = Eq
end;;

match M.comp with | Diff -> false;;

module U = struct type t = {x : int} end;;

module M : sig
  type t = {x : int}
  val comp : (U.t, t) comp
end = struct
  include U
  let comp = Eq
end;;

match M.comp with | Diff -> false;;
type 'a t = T of 'a
type 'a s = S of 'a

type (_, _) eq = Refl : ('a, 'a) eq;;

let f : (int s, int t) eq -> unit = function Refl -> ();;

module M (S : sig type 'a t = T of 'a type 'a s = T of 'a end) =
struct let f : ('a S.s, 'a S.t) eq -> unit = function Refl -> () end;;
type _ nat =
    Zero : [`Zero] nat
  | Succ : 'a nat -> [`Succ of 'a] nat;;
type 'a pre_nat = [`Zero | `Succ of 'a];;
type aux =
  | Aux : [`Succ of [<[<[<[`Zero] pre_nat] pre_nat] pre_nat]] nat -> aux;;

let f (Aux x) =
  match x with
  | Succ Zero -> "1"
  | Succ (Succ Zero) -> "2"
  | Succ (Succ (Succ Zero)) -> "3"
  | Succ (Succ (Succ (Succ Zero))) -> "4"
  | _ -> .  (* error *)
;;
type _ t = C : ((('a -> 'o) -> 'o) -> ('b -> 'o) -> 'o) t
let f : type a o. ((a -> o) -> o) t -> (a -> o) -> o =
 fun C k -> k (fun x -> x);;
type (_, _) t =
 A : ('a, 'a) t
| B : string -> ('a, 'b) t
;;

module M (A : sig module type T end) (B : sig module type T end) =
struct
 let f : ((module A.T), (module B.T)) t -> string = function
   | B s -> s
end;;

module A = struct module type T = sig end end;;

module N = M(A)(A);;

let x = N.f A;;
type 'a visit_action

type insert

type 'a local_visit_action

type ('a, 'result, 'visit_action) context =
  | Local : ('a, ('a * insert) as 'result, 'a local_visit_action) context
  | Global : ('a, 'a, 'a visit_action) context
;;

let vexpr (type visit_action)
    : (_, _, visit_action) context -> _ -> visit_action =
  function
  | Local -> fun _ -> raise Exit
  | Global -> fun _ -> raise Exit
;;

let vexpr (type visit_action)
    : ('a, 'result, visit_action) context -> 'a -> visit_action =
  function
  | Local -> fun _ -> raise Exit
  | Global -> fun _ -> raise Exit
;;

let vexpr (type result) (type visit_action)
    : (unit, result, visit_action) context -> unit -> visit_action =
  function
  | Local -> fun _ -> raise Exit
  | Global -> fun _ -> raise Exit
;;
module A = struct
    type nil = Cstr
  end
open A
;;

type _ s =
  | Nil : nil s
  | Cons : 't s -> ('h -> 't) s

type ('stack, 'typ) var =
  | Head : (('typ -> _) s, 'typ) var
  | Tail : ('tail s, 'typ) var -> ((_ -> 'tail) s, 'typ) var

type _ lst =
  | CNil : nil lst
  | CCons : 'h * ('t lst) -> ('h -> 't) lst
;;

let rec get_var : type stk ret. (stk s, ret) var -> stk lst -> ret = fun n s ->
  match n, s with
  | Head, CCons (h, _) -> h
  | Tail n', CCons (_, t) -> get_var n' t
;;
type 'a t = [< `Foo | `Bar] as 'a;;
type 'a s = [< `Foo | `Bar | `Baz > `Bar] as 'a;;

type 'a first = First : 'a second -> ('b t as 'a) first
and 'a second = Second : ('b s as 'a) second;;

type aux = Aux : 'a t second * ('a -> int) -> aux;;

let it : 'a. [< `Bar | `Foo > `Bar ] as 'a = `Bar;;

let g (Aux(Second, f)) = f it;;
type (_, _) eqp = Y : ('a, 'a) eqp | N : string -> ('a, 'b) eqp
let f : ('a list, 'a) eqp -> unit = function N s -> print_string s;;

module rec A :  sig type t = B.t list end =
  struct type t = B.t list end
and B : sig  type t val eq : (B.t list, t) eqp end =
  struct
    type t = A.t
    let eq = Y
  end;;

f B.eq;;
type (_, _) t =
  | Nil : ('tl, 'tl) t
  | Cons : 'a * ('b, 'tl) t -> ('a * 'b, 'tl) t;;

let get1 (Cons (x, _) : (_ * 'a, 'a) t) = x ;; (* warn, cf PR#6993 *)

let get1' = function
  | (Cons (x, _) : (_ * 'a, 'a) t) -> x
  | Nil -> assert false ;; (* ok *)
type _ t =
  Int : int -> int t | String : string -> string t | Same : 'l t -> 'l t;;
let rec f = function Int x -> x | Same s -> f s;;
type 'a tt = 'a t =
  Int : int -> int tt | String : string -> string tt | Same : 'l1 t -> 'l2 tt;;
type _ t = I : int t;;

let f (type a) (x : a t) =
  let module M = struct
    let (I : a t) = x     (* fail because of toplevel let *)
    let x = (I : a t)
  end in
  () ;;

(* extra example by Stephen Dolan, using recursive modules *)
(* Should not be allowed! *)
type (_,_) eq = Refl : ('a, 'a) eq;;

let bad (type a) =
 let module N = struct
   module rec M : sig
     val e : (int, a) eq
   end = struct
     let (Refl : (int, a) eq) = M.e  (* must fail for soundness *)
     let e : (int, a) eq = Refl
   end
 end in N.M.e
;;
type +'a n = private int
type nil = private Nil_type
type (_,_) elt =
  | Elt_fine: 'nat n -> ('l,'nat * 'l) elt
  | Elt: 'nat n -> ('l,'nat -> 'l) elt
type _ t = Nil : nil t | Cons : ('x, 'fx) elt * 'x t -> 'fx t;;

let undetected: ('a -> 'b -> nil) t -> 'a n -> 'b n -> unit = fun sh i j ->
  let Cons(Elt dim, _) = sh in ()
;;
type _ t = T : int t;;

(* Should raise Not_found *)
let _ = match (raise Not_found : float t) with _ -> .;;
type (_, _) eq = Eq : ('a, 'a) eq | Neq : int -> ('a, 'b) eq;;
type 'a t;;
let f (type a) (Neq n : (a, a t) eq) = n;;   (* warn! *)

module F (T : sig type _ t end) = struct
 let f (type a) (Neq n : (a, a T.t) eq) = n  (* warn! *)
end;;
(* First-Order Unification by Structural Recursion *)
(* Conor McBride, JFP 13(6) *)
(* http://strictlypositive.org/publications.html *)

(* This is a translation of the code part to ocaml *)
(* Of course, we do not prove other properties, not even termination *)

(* 2.2 Inductive Families *)

type zero = Zero
type _ succ = Succ
type _ nat =
  | NZ : zero nat
  | NS : 'a nat -> 'a succ nat

type _ fin =
  | FZ : 'a succ fin
  | FS : 'a fin -> 'a succ fin

(* We cannot define
     val empty : zero fin -> 'a
   because we cannot write an empty pattern matching.
   This might be useful to have *)

(* In place, prove that the parameter is 'a succ *)
type _ is_succ = IS : 'a succ is_succ

let fin_succ : type n. n fin -> n is_succ = function
  | FZ -> IS
  | FS _ -> IS
;;

(* 3 First-Order Terms, Renaming and Substitution *)

type 'a term =
  | Var of 'a fin
  | Leaf
  | Fork of 'a term * 'a term

let var x = Var x

let lift r : 'm fin -> 'n term = fun x -> Var (r x)

let rec pre_subst f = function
  | Var x -> f x
  | Leaf -> Leaf
  | Fork (t1, t2) -> Fork (pre_subst f t1, pre_subst f t2)

let comp_subst f g (x : 'a fin) = pre_subst f (g x)
(*  val comp_subst :
    ('b fin -> 'c term) -> ('a fin -> 'b term) -> 'a fin -> 'c term *)
;;

(* 4 The Occur-Check, through thick and thin *)

let rec thin : type n. n succ fin -> n fin -> n succ fin =
  fun x y -> match x, y with
  | FZ, y    -> FS y
  | FS x, FZ -> FZ
  | FS x, FS y -> FS (thin x y)

let bind t f =
  match t with
  | None   -> None
  | Some x -> f x
(* val bind : 'a option -> ('a -> 'b option) -> 'b option *)

let rec thick : type n. n succ fin -> n succ fin -> n fin option =
  fun x y -> match x, y with
  | FZ, FZ   -> None
  | FZ, FS y -> Some y
  | FS x, FZ -> let IS = fin_succ x in Some FZ
  | FS x, FS y ->
      let IS = fin_succ x in bind (thick x y) (fun x -> Some (FS x))

let rec check : type n. n succ fin -> n succ term -> n term option =
  fun x t -> match t with
  | Var y -> bind (thick x y) (fun x -> Some (Var x))
  | Leaf  -> Some Leaf
  | Fork (t1, t2) ->
      bind (check x t1) (fun t1 ->
        bind (check x t2) (fun t2 -> Some (Fork (t1, t2))))

let subst_var x t' y =
  match thick x y with
  | None -> t'
  | Some y' -> Var y'
(* val subst_var : 'a succ fin -> 'a term -> 'a succ fin -> 'a term *)

let subst x t' = pre_subst (subst_var x t')
(* val subst : 'a succ fin -> 'a term -> 'a succ term -> 'a term *)
;;

(* 5 A Refinement of Substitution *)

type (_,_) alist =
  | Anil  : ('n,'n) alist
  | Asnoc : ('m,'n) alist * 'm term * 'm succ fin -> ('m succ, 'n) alist

let rec sub : type m n. (m,n) alist -> m fin -> n term = function
  | Anil -> var
  | Asnoc (s, t, x) -> comp_subst (sub s) (subst_var x t)

let rec append : type m n l. (m,n) alist -> (l,m) alist -> (l,n) alist =
  fun r s -> match s with
  | Anil -> r
  | Asnoc (s, t, x) -> Asnoc (append r s, t, x)

type _ ealist = EAlist : ('a,'b) alist -> 'a ealist

let asnoc a t' x = EAlist (Asnoc (a, t', x))

(* Extra work: we need sub to work on ealist too, for examples *)
let rec weaken_fin : type n. n fin -> n succ fin = function
  | FZ -> FZ
  | FS x -> FS (weaken_fin x)

let weaken_term t = pre_subst (fun x -> Var (weaken_fin x)) t

let rec weaken_alist : type m n. (m, n) alist -> (m succ, n succ) alist =
  function
    | Anil -> Anil
    | Asnoc (s, t, x) -> Asnoc (weaken_alist s, weaken_term t, weaken_fin x)

let rec sub' : type m. m ealist -> m fin -> m term = function
  | EAlist Anil -> var
  | EAlist (Asnoc (s, t, x)) ->
      comp_subst (sub' (EAlist (weaken_alist s)))
        (fun t' -> weaken_term (subst_var x t t'))

let subst' d = pre_subst (sub' d)
(*  val subst' : 'a ealist -> 'a term -> 'a term *)
;;

(* 6 First-Order Unification *)

let flex_flex x y =
  match thick x y with
  | Some y' -> asnoc Anil (Var y') x
  | None -> EAlist Anil
(* val flex_flex : 'a succ fin -> 'a succ fin -> 'a succ ealist *)

let flex_rigid x t =
  bind (check x t) (fun t' -> Some (asnoc Anil t' x))
(* val flex_rigid : 'a succ fin -> 'a succ term -> 'a succ ealist option *)

let rec amgu : type m. m term -> m term -> m ealist -> m ealist option =
  fun s t acc -> match s, t, acc with
  | Leaf, Leaf, _   -> Some acc
  | Leaf, Fork _, _ -> None
  | Fork _, Leaf, _ -> None
  | Fork (s1, s2), Fork (t1, t2), _ ->
      bind (amgu s1 t1 acc) (amgu s2 t2)
  | Var x, Var y, EAlist Anil -> let IS = fin_succ x in Some (flex_flex x y)
  | Var x, t,     EAlist Anil -> let IS = fin_succ x in flex_rigid x t
  | t, Var x,     EAlist Anil -> let IS = fin_succ x in flex_rigid x t
  | s, t, EAlist(Asnoc(d,r,z)) ->
      bind (amgu (subst z r s) (subst z r t) (EAlist d))
           (fun (EAlist d) -> Some (asnoc d r z))

let mgu s t = amgu s t (EAlist Anil)
(* val mgu : 'a term -> 'a term -> 'a ealist option *)
;;

let s = Fork (Var FZ, Fork (Var (FS (FS FZ)), Leaf))
let t = Fork (Var (FS FZ), Var (FS FZ))
let d = match mgu s t with Some x -> x | None -> failwith "mgu"
let s' = subst' d s
let t' = subst' d t
;;
(* Injectivity *)

type (_, _) eq = Refl : ('a, 'a) eq

let magic : 'a 'b. 'a -> 'b =
  fun (type a b) (x : a) ->
    let module M =
      (functor (T : sig type 'a t end) ->
       struct
         let f (Refl : (a T.t, b T.t) eq) = (x :> b)
       end)
        (struct type 'a t = unit end)
    in M.f Refl
;;

(* Variance and subtyping *)

type (_, +_) eq = Refl : ('a, 'a) eq

let magic : 'a 'b. 'a -> 'b =
  fun (type a) (type b) (x : a) ->
    let bad_proof (type a) =
      (Refl : (< m : a>, <m : a>) eq :> (<m : a>, < >) eq) in
    let downcast : type a. (a, < >) eq -> < > -> a =
      fun (type a) (Refl : (a, < >) eq) (s : < >) -> (s :> a) in
    (downcast bad_proof ((object method m = x end) :> < >)) # m
;;

(* Record patterns *)

type _ t =
  | IntLit : int t
  | BoolLit : bool t

let check : type s . s t * s -> bool = function
  | BoolLit, false -> false
  | IntLit , 6 -> false
;;

type ('a, 'b) pair = { fst : 'a; snd : 'b }

let check : type s . (s t, s) pair -> bool = function
  | {fst = BoolLit; snd = false} -> false
  | {fst = IntLit ; snd =  6} -> false
;;
module type S = sig type t [@@immediate] end;;
module F (M : S) : S = M;;
[%%expect{|
module type S = sig type t [@@immediate] end
module F : functor (M : S) -> S
|}];;

(* VALID DECLARATIONS *)

module A = struct
  (* Abstract types can be immediate *)
  type t [@@immediate]

  (* [@@immediate] tag here is unnecessary but valid since t has it *)
  type s = t [@@immediate]

  (* Again, valid alias even without tag *)
  type r = s

  (* Mutually recursive declarations work as well *)
  type p = q [@@immediate]
  and q = int
end;;
[%%expect{|
module A :
  sig
    type t [@@immediate]
    type s = t [@@immediate]
    type r = s
    type p = q [@@immediate]
    and q = int
  end
|}];;

(* Valid using with constraints *)
module type X = sig type t end;;
module Y = struct type t = int end;;
module Z = ((Y : X with type t = int) : sig type t [@@immediate] end);;
[%%expect{|
module type X = sig type t end
module Y : sig type t = int end
module Z : sig type t [@@immediate] end
|}];;

(* Valid using an explicit signature *)
module M_valid : S = struct type t = int end;;
module FM_valid = F (struct type t = int end);;
[%%expect{|
module M_valid : S
module FM_valid : S
|}];;

(* Practical usage over modules *)
module Foo : sig type t val x : t ref end = struct
  type t = int
  let x = ref 0
end;;
[%%expect{|
module Foo : sig type t val x : t ref end
|}];;

module Bar : sig type t [@@immediate] val x : t ref end = struct
  type t = int
  let x = ref 0
end;;
[%%expect{|
module Bar : sig type t [@@immediate] val x : t ref end
|}];;

let test f =
  let start = Sys.time() in f ();
  (Sys.time() -. start);;
[%%expect{|
val test : (unit -> 'a) -> float = <fun>
|}];;

let test_foo () =
  for i = 0 to 100_000_000 do
    Foo.x := !Foo.x
  done;;
[%%expect{|
val test_foo : unit -> unit = <fun>
|}];;

let test_bar () =
  for i = 0 to 100_000_000 do
    Bar.x := !Bar.x
  done;;
[%%expect{|
val test_bar : unit -> unit = <fun>
|}];;

(* Uncomment these to test. Should see substantial speedup!
let () = Printf.printf "No @@immediate: %fs\n" (test test_foo)
let () = Printf.printf "With @@immediate: %fs\n" (test test_bar) *)


(* INVALID DECLARATIONS *)

(* Cannot directly declare a non-immediate type as immediate *)
module B = struct
  type t = string [@@immediate]
end;;
[%%expect{|
Line _, characters 2-31:
Error: Types marked with the immediate attribute must be
       non-pointer types like int or bool
|}];;

(* Not guaranteed that t is immediate, so this is an invalid declaration *)
module C = struct
  type t
  type s = t [@@immediate]
end;;
[%%expect{|
Line _, characters 2-26:
Error: Types marked with the immediate attribute must be
       non-pointer types like int or bool
|}];;

(* Can't ascribe to an immediate type signature with a non-immediate type *)
module D : sig type t [@@immediate] end = struct
  type t = string
end;;
[%%expect{|
Line _, characters 42-70:
Error: Signature mismatch:
       Modules do not match:
         sig type t = string end
       is not included in
         sig type t [@@immediate] end
       Type declarations do not match:
         type t = string
       is not included in
         type t [@@immediate]
       the first is not an immediate type.
|}];;

(* Same as above but with explicit signature *)
module M_invalid : S = struct type t = string end;;
module FM_invalid = F (struct type t = string end);;
[%%expect{|
Line _, characters 23-49:
Error: Signature mismatch:
       Modules do not match: sig type t = string end is not included in S
       Type declarations do not match:
         type t = string
       is not included in
         type t [@@immediate]
       the first is not an immediate type.
|}];;

(* Can't use a non-immediate type even if mutually recursive *)
module E = struct
  type t = s [@@immediate]
  and s = string
end;;
[%%expect{|
Line _, characters 2-26:
Error: Types marked with the immediate attribute must be
       non-pointer types like int or bool
|}];;
(*
   Implicit unpack allows to omit the signature in (val ...) expressions.

   It also adds (module M : S) and (module M) patterns, relying on
   implicit (val ...) for the implementation. Such patterns can only
   be used in function definition, match clauses, and let ... in.

   New: implicit pack is also supported, and you only need to be able
   to infer the the module type path from the context.
 *)
(* ocaml -principal *)

(* Use a module pattern *)
let sort (type s) (module Set : Set.S with type elt = s) l =
  Set.elements (List.fold_right Set.add l Set.empty)

(* No real improvement here? *)
let make_set (type s) cmp : (module Set.S with type elt = s) =
  (module Set.Make (struct type t = s let compare = cmp end))

(* No type annotation here *)
let sort_cmp (type s) cmp =
  sort (module Set.Make (struct type t = s let compare = cmp end))

module type S = sig type t val x : t end;;
let f (module M : S with type t = int) = M.x;;
let f (module M : S with type t = 'a) = M.x;; (* Error *)
let f (type a) (module M : S with type t = a) = M.x;;
f (module struct type t = int let x = 1 end);;

type 'a s = {s: (module S with type t = 'a)};;
{s=(module struct type t = int let x = 1 end)};;
let f {s=(module M)} = M.x;; (* Error *)
let f (type a) ({s=(module M)} : a s) = M.x;;

type s = {s: (module S with type t = int)};;
let f {s=(module M)} = M.x;;
let f {s=(module M)} {s=(module N)} = M.x + N.x;;

module type S = sig val x : int end;;
let f (module M : S) y (module N : S) = M.x + y + N.x;;
let m = (module struct let x = 3 end);; (* Error *)
let m = (module struct let x = 3 end : S);;
f m 1 m;;
f m 1 (module struct let x = 2 end);;

let (module M) = m in M.x;;
let (module M) = m;; (* Error: only allowed in [let .. in] *)
class c = let (module M) = m in object end;; (* Error again *)
module M = (val m);;

module type S' = sig val f : int -> int end;;
(* Even works with recursion, but must be fully explicit *)
let rec (module M : S') =
  (module struct let f n = if n <= 0 then 1 else n * M.f (n-1) end : S')
in M.f 3;;

(* Subtyping *)

module type S = sig type t type u val x : t * u end
let f (l : (module S with type t = int and type u = bool) list) =
  (l :> (module S with type u = bool) list)

(* GADTs from the manual *)
(* the only modification is in to_string *)

module TypEq : sig
  type ('a, 'b) t
  val apply: ('a, 'b) t -> 'a -> 'b
  val refl: ('a, 'a) t
  val sym: ('a, 'b) t -> ('b, 'a) t
end = struct
  type ('a, 'b) t = ('a -> 'b) * ('b -> 'a)
  let refl = (fun x -> x), (fun x -> x)
  let apply (f, _) x = f x
  let sym (f, g) = (g, f)
end

module rec Typ : sig
  module type PAIR = sig
    type t and t1 and t2
    val eq: (t, t1 * t2) TypEq.t
    val t1: t1 Typ.typ
    val t2: t2 Typ.typ
  end

  type 'a typ =
    | Int of ('a, int) TypEq.t
    | String of ('a, string) TypEq.t
    | Pair of (module PAIR with type t = 'a)
end = Typ

let int = Typ.Int TypEq.refl

let str = Typ.String TypEq.refl

let pair (type s1) (type s2) t1 t2 =
  let module P = struct
    type t = s1 * s2
    type t1 = s1
    type t2 = s2
    let eq = TypEq.refl
    let t1 = t1
    let t2 = t2
  end in
  Typ.Pair (module P)

open Typ
let rec to_string: 'a. 'a Typ.typ -> 'a -> string =
  fun (type s) t x ->
    match (t : s typ) with
    | Int eq -> string_of_int (TypEq.apply eq x)
    | String eq -> Printf.sprintf "%S" (TypEq.apply eq x)
    | Pair (module P) ->
        let (x1, x2) = TypEq.apply P.eq x in
        Printf.sprintf "(%s,%s)" (to_string P.t1 x1) (to_string P.t2 x2)

(* Wrapping maps *)
module type MapT = sig
  include Map.S
  type data
  type map
  val of_t : data t -> map
  val to_t : map -> data t
end

type ('k,'d,'m) map =
    (module MapT with type key = 'k and type data = 'd and type map = 'm)

let add (type k) (type d) (type m) (m:(k,d,m) map) x y s =
   let module M =
     (val m:MapT with type key = k and type data = d and type map = m) in
   M.of_t (M.add x y (M.to_t s))

module SSMap = struct
  include Map.Make(String)
  type data = string
  type map = data t
  let of_t x = x
  let to_t x = x
end

let ssmap =
  (module SSMap:
   MapT with type key = string and type data = string and type map = SSMap.map)
;;

let ssmap =
  (module struct include SSMap end :
   MapT with type key = string and type data = string and type map = SSMap.map)
;;

let ssmap =
  (let module S = struct include SSMap end in (module S) :
  (module
   MapT with type key = string and type data = string and type map = SSMap.map))
;;

let ssmap =
  (module SSMap: MapT with type key = _ and type data = _ and type map = _)
;;

let ssmap : (_,_,_) map = (module SSMap);;

add ssmap;;
open StdLabels
open MoreLabels

(* Use maps for substitutions and sets for free variables *)

module Subst = Map.Make(struct type t = string let compare = compare end)
module Names = Set.Make(struct type t = string let compare = compare end)


(* Variables are common to lambda and expr *)

type var = [`Var of string]

let subst_var ~subst : var -> _ =
  function `Var s as x ->
    try Subst.find s subst
    with Not_found -> x

let free_var : var -> _ = function `Var s -> Names.singleton s


(* The lambda language: free variables, substitutions, and evaluation *)

type 'a lambda = [`Var of string | `Abs of string * 'a | `App of 'a * 'a]

let free_lambda ~free_rec : _ lambda -> _ = function
    #var as x -> free_var x
  | `Abs (s, t) -> Names.remove s (free_rec t)
  | `App (t1, t2) -> Names.union (free_rec t1) (free_rec t2)

let map_lambda ~map_rec : _ lambda -> _ = function
    #var as x -> x
  | `Abs (s, t) as l ->
      let t' = map_rec t in
      if t == t' then l else `Abs(s, t')
  | `App (t1, t2) as l ->
      let t'1 = map_rec t1 and t'2 = map_rec t2 in
      if t'1 == t1 && t'2 == t2 then l else `App (t'1, t'2)

let next_id =
  let current = ref 3 in
  fun () -> incr current; !current

let subst_lambda ~subst_rec ~free ~subst : _ lambda -> _ = function
    #var as x -> subst_var ~subst x
  | `Abs(s, t) as l ->
      let used = free t in
      let used_expr =
        Subst.fold subst ~init:[]
          ~f:(fun ~key ~data acc ->
                if Names.mem s used then data::acc else acc) in
      if List.exists used_expr ~f:(fun t -> Names.mem s (free t)) then
        let name = s ^ string_of_int (next_id ()) in
        `Abs(name,
             subst_rec ~subst:(Subst.add ~key:s ~data:(`Var name) subst) t)
      else
        map_lambda ~map_rec:(subst_rec ~subst:(Subst.remove s subst)) l
  | `App _ as l ->
      map_lambda ~map_rec:(subst_rec ~subst) l

let eval_lambda ~eval_rec ~subst l =
  match map_lambda ~map_rec:eval_rec l with
    `App(`Abs(s,t1), t2) ->
      eval_rec (subst ~subst:(Subst.add ~key:s ~data:t2 Subst.empty) t1)
  | t -> t

(* Specialized versions to use on lambda *)

let rec free1 x = free_lambda ~free_rec:free1 x
let rec subst1 ~subst = subst_lambda ~subst_rec:subst1 ~free:free1 ~subst
let rec eval1 x = eval_lambda ~eval_rec:eval1 ~subst:subst1 x


(* The expr language of arithmetic expressions *)

type 'a expr =
    [`Var of string | `Num of int | `Add of 'a * 'a
    | `Neg of 'a | `Mult of 'a * 'a]

let free_expr ~free_rec : _ expr -> _ = function
    #var as x -> free_var x
  | `Num _ -> Names.empty
  | `Add(x, y) -> Names.union (free_rec x) (free_rec y)
  | `Neg x -> free_rec x
  | `Mult(x, y) -> Names.union (free_rec x) (free_rec y)

(* Here map_expr helps a lot *)
let map_expr ~map_rec : _ expr -> _ = function
    #var as x -> x
  | `Num _ as x -> x
  | `Add(x, y) as e ->
      let x' = map_rec x and y' = map_rec y in
      if x == x' && y == y' then e
      else `Add(x', y')
  | `Neg x as e ->
      let x' = map_rec x in
      if x == x' then e else `Neg x'
  | `Mult(x, y) as e ->
      let x' = map_rec x and y' = map_rec y in
      if x == x' && y == y' then e
      else `Mult(x', y')

let subst_expr ~subst_rec ~subst : _ expr -> _ = function
    #var as x -> subst_var ~subst x
  | #expr as e -> map_expr ~map_rec:(subst_rec ~subst) e

let eval_expr ~eval_rec e =
  match map_expr ~map_rec:eval_rec e with
    `Add(`Num m, `Num n) -> `Num (m+n)
  | `Neg(`Num n) -> `Num (-n)
  | `Mult(`Num m, `Num n) -> `Num (m*n)
  | #expr as e -> e

(* Specialized versions *)

let rec free2 x = free_expr ~free_rec:free2 x
let rec subst2 ~subst = subst_expr ~subst_rec:subst2 ~subst
let rec eval2 x = eval_expr ~eval_rec:eval2 x


(* The lexpr language, reunion of lambda and expr *)

type lexpr =
  [ `Var of string | `Abs of string * lexpr | `App of lexpr * lexpr
  | `Num of int | `Add of lexpr * lexpr | `Neg of lexpr
  | `Mult of lexpr * lexpr ]

let rec free : lexpr -> _ = function
    #lambda as x -> free_lambda ~free_rec:free x
  | #expr as x -> free_expr ~free_rec:free x

let rec subst ~subst:s : lexpr -> _ = function
    #lambda as x -> subst_lambda ~subst_rec:subst ~subst:s ~free x
  | #expr as x -> subst_expr ~subst_rec:subst ~subst:s x

let rec eval : lexpr -> _ = function
    #lambda as x -> eval_lambda ~eval_rec:eval ~subst x
  | #expr as x -> eval_expr ~eval_rec:eval x

let rec print = function
  | `Var id -> print_string id
  | `Abs (id, l) -> print_string ("\ " ^ id ^ " . "); print l
  | `App (l1, l2) -> print l1; print_string " "; print l2
  | `Num x -> print_int x
  | `Add (e1, e2) -> print e1; print_string " + "; print e2
  | `Neg e -> print_string "-"; print e
  | `Mult (e1, e2) -> print e1; print_string " * "; print e2

let () =
  let e1 = eval1 (`App(`Abs("x",`Var"x"), `Var"y")) in
  let e2 = eval2 (`Add(`Mult(`Num 3,`Neg(`Num 2)), `Var"x")) in
  let e3 = eval (`Add(`App(`Abs("x",`Mult(`Var"x",`Var"x")),`Num 2), `Num 5)) in
  print e1; print_newline ();
  print e2; print_newline ();
  print e3; print_newline ()
(* Full fledge version, using objects to structure code *)

open StdLabels
open MoreLabels

(* Use maps for substitutions and sets for free variables *)

module Subst = Map.Make(struct type t = string let compare = compare end)
module Names = Set.Make(struct type t = string let compare = compare end)

(* To build recursive objects *)

let lazy_fix make =
  let rec obj () = make (lazy (obj ()) : _ Lazy.t) in
  obj ()

let (!!) = Lazy.force

(* The basic operations *)

class type ['a, 'b] ops =
  object
    method free : x:'b -> ?y:'c -> Names.t
    method subst : sub:'a Subst.t -> 'b -> 'a
    method eval : 'b -> 'a
  end

(* Variables are common to lambda and expr *)

type var = [`Var of string]

class ['a] var_ops = object (self : ('a, var) #ops)
  constraint 'a = [> var]
  method subst ~sub (`Var s as x) =
    try Subst.find s sub with Not_found -> x
  method free (`Var s) =
    Names.singleton s
  method eval (#var as v) = v
end

(* The lambda language: free variables, substitutions, and evaluation *)

type 'a lambda = [`Var of string | `Abs of string * 'a | `App of 'a * 'a]

let next_id =
  let current = ref 3 in
  fun () -> incr current; !current

class ['a] lambda_ops (ops : ('a,'a) #ops Lazy.t) =
  let var : 'a var_ops = new var_ops
  and free = lazy !!ops#free
  and subst = lazy !!ops#subst
  and eval = lazy !!ops#eval in
  object (self : ('a, 'a lambda) #ops)
    constraint 'a = [> 'a lambda]
    method free = function
        #var as x -> var#free x
      | `Abs (s, t) -> Names.remove s (!!free t)
      | `App (t1, t2) -> Names.union (!!free t1) (!!free t2)

    method map ~f = function
        #var as x -> x
      | `Abs (s, t) as l ->
          let t' = f t in
          if t == t' then l else `Abs(s, t')
      | `App (t1, t2) as l ->
          let t'1 = f t1 and t'2 = f t2 in
          if t'1 == t1 && t'2 == t2 then l else `App (t'1, t'2)

    method subst ~sub = function
        #var as x -> var#subst ~sub x
      | `Abs(s, t) as l ->
          let used = !!free t in
          let used_expr =
            Subst.fold sub ~init:[]
              ~f:(fun ~key ~data acc ->
                if Names.mem s used then data::acc else acc) in
          if List.exists used_expr ~f:(fun t -> Names.mem s (!!free t)) then
            let name = s ^ string_of_int (next_id ()) in
            `Abs(name,
                 !!subst ~sub:(Subst.add ~key:s ~data:(`Var name) sub) t)
          else
            self#map ~f:(!!subst ~sub:(Subst.remove s sub)) l
      | `App _ as l ->
          self#map ~f:(!!subst ~sub) l

    method eval l =
      match self#map ~f:!!eval l with
        `App(`Abs(s,t1), t2) ->
          !!eval (!!subst ~sub:(Subst.add ~key:s ~data:t2 Subst.empty) t1)
      | t -> t
end

(* Operations specialized to lambda *)

let lambda = lazy_fix (new lambda_ops)

(* The expr language of arithmetic expressions *)

type 'a expr =
    [ `Var of string | `Num of int | `Add of 'a * 'a
    | `Neg of 'a | `Mult of 'a * 'a]

class ['a] expr_ops (ops : ('a,'a) #ops Lazy.t) =
  let var : 'a var_ops = new var_ops
  and free = lazy !!ops#free
  and subst = lazy !!ops#subst
  and eval = lazy !!ops#eval in
  object (self : ('a, 'a expr) #ops)
    constraint 'a = [> 'a expr]
    method free = function
        #var as x -> var#free x
      | `Num _ -> Names.empty
      | `Add(x, y) -> Names.union (!!free x) (!!free y)
      | `Neg x -> !!free x
      | `Mult(x, y) -> Names.union (!!free x) (!!free y)

    method map ~f = function
        #var as x -> x
      | `Num _ as x -> x
      | `Add(x, y) as e ->
          let x' = f x and y' = f y in
          if x == x' && y == y' then e
          else `Add(x', y')
      | `Neg x as e ->
          let x' = f x in
          if x == x' then e else `Neg x'
      | `Mult(x, y) as e ->
          let x' = f x and y' = f y in
          if x == x' && y == y' then e
          else `Mult(x', y')

    method subst ~sub = function
        #var as x -> var#subst ~sub x
      | #expr as e -> self#map ~f:(!!subst ~sub) e

    method eval (#expr as e) =
      match self#map ~f:!!eval e with
        `Add(`Num m, `Num n) -> `Num (m+n)
      | `Neg(`Num n) -> `Num (-n)
      | `Mult(`Num m, `Num n) -> `Num (m*n)
      | e -> e
  end

(* Specialized versions *)

let expr = lazy_fix (new expr_ops)

(* The lexpr language, reunion of lambda and expr *)

type 'a lexpr = [ 'a lambda | 'a expr ]

class ['a] lexpr_ops (ops : ('a,'a) #ops Lazy.t) =
  let lambda = new lambda_ops ops in
  let expr = new expr_ops ops in
  object (self : ('a, 'a lexpr) #ops)
    constraint 'a = [> 'a lexpr]
    method free = function
        #lambda as x -> lambda#free x
      | #expr as x -> expr#free x

    method subst ~sub = function
        #lambda as x -> lambda#subst ~sub x
      | #expr as x -> expr#subst ~sub x

    method eval = function
        #lambda as x -> lambda#eval x
      | #expr as x -> expr#eval x
end

let lexpr = lazy_fix (new lexpr_ops)

let rec print = function
  | `Var id -> print_string id
  | `Abs (id, l) -> print_string ("\ " ^ id ^ " . "); print l
  | `App (l1, l2) -> print l1; print_string " "; print l2
  | `Num x -> print_int x
  | `Add (e1, e2) -> print e1; print_string " + "; print e2
  | `Neg e -> print_string "-"; print e
  | `Mult (e1, e2) -> print e1; print_string " * "; print e2

let () =
  let e1 = lambda#eval (`App(`Abs("x",`Var"x"), `Var"y")) in
  let e2 = expr#eval (`Add(`Mult(`Num 3,`Neg(`Num 2)), `Var"x")) in
  let e3 =
    lexpr#eval (`Add(`App(`Abs("x",`Mult(`Var"x",`Var"x")),`Num 2), `Num 5))
  in
  print e1; print_newline ();
  print e2; print_newline ();
  print e3; print_newline ()
(* Full fledge version, using objects to structure code *)

open StdLabels
open MoreLabels

(* Use maps for substitutions and sets for free variables *)

module Subst = Map.Make(struct type t = string let compare = compare end)
module Names = Set.Make(struct type t = string let compare = compare end)

(* To build recursive objects *)

let lazy_fix make =
  let rec obj () = make (lazy (obj ()) : _ Lazy.t) in
  obj ()

let (!!) = Lazy.force

(* The basic operations *)

class type ['a, 'b] ops =
  object
    method free : 'b -> Names.t
    method subst : sub:'a Subst.t -> 'b -> 'a
    method eval : 'b -> 'a
  end

(* Variables are common to lambda and expr *)

type var = [`Var of string]

let var = object (self : ([>var], var) #ops)
  method subst ~sub (`Var s as x) =
    try Subst.find s sub with Not_found -> x
  method free (`Var s) =
    Names.singleton s
  method eval (#var as v) = v
end

(* The lambda language: free variables, substitutions, and evaluation *)

type 'a lambda = [`Var of string | `Abs of string * 'a | `App of 'a * 'a]

let next_id =
  let current = ref 3 in
  fun () -> incr current; !current

let lambda_ops (ops : ('a,'a) #ops Lazy.t) =
  let free = lazy !!ops#free
  and subst = lazy !!ops#subst
  and eval = lazy !!ops#eval in
  object (self : ([> 'a lambda], 'a lambda) #ops)
    method free = function
        #var as x -> var#free x
      | `Abs (s, t) -> Names.remove s (!!free t)
      | `App (t1, t2) -> Names.union (!!free t1) (!!free t2)

    method private map ~f = function
        #var as x -> x
      | `Abs (s, t) as l ->
          let t' = f t in
          if t == t' then l else `Abs(s, t')
      | `App (t1, t2) as l ->
          let t'1 = f t1 and t'2 = f t2 in
          if t'1 == t1 && t'2 == t2 then l else `App (t'1, t'2)

    method subst ~sub = function
        #var as x -> var#subst ~sub x
      | `Abs(s, t) as l ->
          let used = !!free t in
          let used_expr =
            Subst.fold sub ~init:[]
              ~f:(fun ~key ~data acc ->
                if Names.mem s used then data::acc else acc) in
          if List.exists used_expr ~f:(fun t -> Names.mem s (!!free t)) then
            let name = s ^ string_of_int (next_id ()) in
            `Abs(name,
                 !!subst ~sub:(Subst.add ~key:s ~data:(`Var name) sub) t)
          else
            self#map ~f:(!!subst ~sub:(Subst.remove s sub)) l
      | `App _ as l ->
          self#map ~f:(!!subst ~sub) l

    method eval l =
      match self#map ~f:!!eval l with
        `App(`Abs(s,t1), t2) ->
          !!eval (!!subst ~sub:(Subst.add ~key:s ~data:t2 Subst.empty) t1)
      | t -> t
end

(* Operations specialized to lambda *)

let lambda = lazy_fix lambda_ops

(* The expr language of arithmetic expressions *)

type 'a expr =
    [ `Var of string | `Num of int | `Add of 'a * 'a
    | `Neg of 'a | `Mult of 'a * 'a]

let expr_ops (ops : ('a,'a) #ops Lazy.t) =
  let free = lazy !!ops#free
  and subst = lazy !!ops#subst
  and eval = lazy !!ops#eval in
  object (self : ([> 'a expr], 'a expr) #ops)
    method free = function
        #var as x -> var#free x
      | `Num _ -> Names.empty
      | `Add(x, y) -> Names.union (!!free x) (!!free y)
      | `Neg x -> !!free x
      | `Mult(x, y) -> Names.union (!!free x) (!!free y)

    method private map ~f = function
        #var as x -> x
      | `Num _ as x -> x
      | `Add(x, y) as e ->
          let x' = f x and y' = f y in
          if x == x' && y == y' then e
          else `Add(x', y')
      | `Neg x as e ->
          let x' = f x in
          if x == x' then e else `Neg x'
      | `Mult(x, y) as e ->
          let x' = f x and y' = f y in
          if x == x' && y == y' then e
          else `Mult(x', y')

    method subst ~sub = function
        #var as x -> var#subst ~sub x
      | #expr as e -> self#map ~f:(!!subst ~sub) e

    method eval (#expr as e) =
      match self#map ~f:!!eval e with
        `Add(`Num m, `Num n) -> `Num (m+n)
      | `Neg(`Num n) -> `Num (-n)
      | `Mult(`Num m, `Num n) -> `Num (m*n)
      | e -> e
  end

(* Specialized versions *)

let expr = lazy_fix expr_ops

(* The lexpr language, reunion of lambda and expr *)

type 'a lexpr = [ 'a lambda | 'a expr ]

let lexpr_ops (ops : ('a,'a) #ops Lazy.t) =
  let lambda = lambda_ops ops in
  let expr = expr_ops ops in
  object (self : ([> 'a lexpr], 'a lexpr) #ops)
    method free = function
        #lambda as x -> lambda#free x
      | #expr as x -> expr#free x

    method subst ~sub = function
        #lambda as x -> lambda#subst ~sub x
      | #expr as x -> expr#subst ~sub x

    method eval = function
        #lambda as x -> lambda#eval x
      | #expr as x -> expr#eval x
end

let lexpr = lazy_fix lexpr_ops

let rec print = function
  | `Var id -> print_string id
  | `Abs (id, l) -> print_string ("\ " ^ id ^ " . "); print l
  | `App (l1, l2) -> print l1; print_string " "; print l2
  | `Num x -> print_int x
  | `Add (e1, e2) -> print e1; print_string " + "; print e2
  | `Neg e -> print_string "-"; print e
  | `Mult (e1, e2) -> print e1; print_string " * "; print e2

let () =
  let e1 = lambda#eval (`App(`Abs("x",`Var"x"), `Var"y")) in
  let e2 = expr#eval (`Add(`Mult(`Num 3,`Neg(`Num 2)), `Var"x")) in
  let e3 =
    lexpr#eval (`Add(`App(`Abs("x",`Mult(`Var"x",`Var"x")),`Num 2), `Num 5))
  in
  print e1; print_newline ();
  print e2; print_newline ();
  print e3; print_newline ()
type sexp = A of string | L of sexp list
type 'a t = 'a array
let _ = fun (_ : 'a t)  -> ()

let array_of_sexp _ _ = [| |]
let sexp_of_array _ _ = A "foo"
let sexp_of_int _ = A "42"
let int_of_sexp _ = 42

let t_of_sexp : 'a . (sexp -> 'a) -> sexp -> 'a t=
  let _tp_loc = "core_array.ml.t" in
  fun _of_a  -> fun t  -> (array_of_sexp _of_a) t
let _ = t_of_sexp
let sexp_of_t : 'a . ('a -> sexp) -> 'a t -> sexp=
  fun _of_a  -> fun v  -> (sexp_of_array _of_a) v
let _ = sexp_of_t
module T =
  struct
    module Int =
      struct
        type t_ = int array
        let _ = fun (_ : t_)  -> ()

        let t__of_sexp: sexp -> t_ =
          let _tp_loc = "core_array.ml.T.Int.t_" in
          fun t  -> (array_of_sexp int_of_sexp) t
        let _ = t__of_sexp
        let sexp_of_t_: t_ -> sexp =
          fun v  -> (sexp_of_array sexp_of_int) v
        let _ = sexp_of_t_
      end
  end
module type Permissioned  =
  sig
    type ('a,-'perms) t
  end
module Permissioned :
  sig
    type ('a,-'perms) t
    include
      sig
        val t_of_sexp :
          (sexp -> 'a) ->
            (sexp -> 'perms) -> sexp -> ('a,'perms) t
        val sexp_of_t :
          ('a -> sexp) ->
            ('perms -> sexp) -> ('a,'perms) t -> sexp
      end
    module Int :
    sig
      type nonrec -'perms t = (int,'perms) t
      include
        sig
          val t_of_sexp :
            (sexp -> 'perms) -> sexp -> 'perms t
          val sexp_of_t :
            ('perms -> sexp) -> 'perms t -> sexp
        end
    end
  end =
  struct
    type ('a,-'perms) t = 'a array
    let _ = fun (_ : ('a,'perms) t)  -> ()

    let t_of_sexp :
      'a 'perms .
        (sexp -> 'a) ->
          (sexp -> 'perms) -> sexp -> ('a,'perms) t=
      let _tp_loc = "core_array.ml.Permissioned.t" in
      fun _of_a  -> fun _of_perms  -> fun t  -> (array_of_sexp _of_a) t
    let _ = t_of_sexp
    let sexp_of_t :
      'a 'perms .
        ('a -> sexp) ->
          ('perms -> sexp) -> ('a,'perms) t -> sexp=
      fun _of_a  -> fun _of_perms  -> fun v  -> (sexp_of_array _of_a) v
    let _ = sexp_of_t
    module Int =
      struct
        include T.Int
        type -'perms t = t_
        let _ = fun (_ : 'perms t)  -> ()

        let t_of_sexp :
          'perms . (sexp -> 'perms) -> sexp -> 'perms t=
          let _tp_loc = "core_array.ml.Permissioned.Int.t" in
          fun _of_perms  -> fun t  -> t__of_sexp t
        let _ = t_of_sexp
        let sexp_of_t :
          'perms . ('perms -> sexp) -> 'perms t -> sexp=
          fun _of_perms  -> fun v  -> sexp_of_t_ v
        let _ = sexp_of_t
      end
  end
type 'a  foo = {x: 'a; y: int}
let r = {{x = 0; y = 0} with x = 0}
let r' : string foo = r
external foo : int = "%ignore";;
let _ = foo ();;
type 'a t = [`A of 'a t t] as 'a;; (* fails *)

type 'a t = [`A of 'a t t];; (* fails *)

type 'a t = [`A of 'a t t] constraint 'a = 'a t;;

type 'a t = [`A of 'a t] constraint 'a = 'a t;;

type 'a t = [`A of 'a] as 'a;;

type 'a v = [`A of u v] constraint 'a = t and t = u and u = t;; (* fails *)

type 'a t = 'a;;
let f (x : 'a t as 'a) = ();; (* fails *)

let f (x : 'a t) (y : 'a) = x = y;;

(* PR#6505 *)
module type PR6505 = sig
  type 'o is_an_object = < .. > as 'o
  and 'o abs constraint 'o = 'o is_an_object
  val abs : 'o is_an_object -> 'o abs
  val unabs : 'o abs -> 'o
end;; (* fails *)
(* PR#5835 *)
let f ~x = x + 1;;
f ?x:0;;

(* PR#6352 *)
let foo (f : unit -> unit) = ();;
let g ?x () = ();;
foo ((); g);;

(* PR#5748 *)
foo (fun ?opt () -> ()) ;; (* fails *)
(* PR#5907 *)

type 'a t = 'a;;
let f (g : 'a list -> 'a t -> 'a) s = g s s;;
let f (g : 'a * 'b -> 'a t -> 'a) s = g s s;;
type ab = [ `A | `B ];;
let f (x : [`A]) = match x with #ab -> 1;;
let f x = ignore (match x with #ab -> 1); ignore (x : [`A]);;
let f x = ignore (match x with `A|`B -> 1); ignore (x : [`A]);;

let f (x : [< `A | `B]) = match x with `A | `B | `C -> 0;; (* warn *)
let f (x : [`A | `B]) = match x with `A | `B | `C -> 0;; (* fail *)

(* PR#6787 *)
let revapply x f = f x;;

let f x (g : [< `Foo]) =
  let y = `Bar x, g in
  revapply y (fun ((`Bar i), _) -> i);;
(* f : 'a -> [< `Foo ] -> 'a *)

let rec x = [| x |]; 1.;;

let rec x = let u = [|y|] in 10. and y = 1.;;
type 'a t
type a

let f : < .. > t -> unit = fun _ -> ();;

let g : [< `b] t -> unit = fun _ -> ();;

let h : [> `b] t -> unit = fun _ -> ();;

let _ = fun (x : a t) -> f x;;

let _ = fun (x : a t) -> g x;;

let _ = fun (x : a t) -> h x;;
(* PR#7012 *)

type t = [ 'A_name | `Hi ];;

let f (x:'id_arg) = x;;

let f (x:'Id_arg) = x;;
(* undefined labels *)
type t = {x:int;y:int};;
{x=3;z=2};;
fun {x=3;z=2} -> ();;

(* mixed labels *)
{x=3; contents=2};;

(* private types *)
type u = private {mutable u:int};;
{u=3};;
fun x -> x.u <- 3;;

(* Punning and abbreviations *)
module M = struct
  type t = {x: int; y: int}
end;;

let f {M.x; y} = x+y;;
let r = {M.x=1; y=2};;
let z = f r;;

(* messages *)
type foo = { mutable y:int };;
let f (r: int) = r.y <- 3;;

(* bugs *)
type foo = { y: int; z: int };;
type bar = { x: int };;
let f (r: bar) = ({ r with z = 3 } : foo)

type foo = { x: int };;
let r : foo = { ZZZ.x = 2 };;

(ZZZ.X : int option);;

(* PR#5865 *)
let f (x : Complex.t) = x.Complex.z;;
(* PR#6394 *)

module rec X : sig
 type t = int * bool
end = struct
 type t = A | B
 let f = function A | B -> 0
end;;
(* PR#6768 *)

type _ prod = Prod : ('a * 'y) prod;;

let f : type t. t prod -> _ = function Prod ->
  let module M =
    struct
      type d = d * d
    end
  in ()
;;
let (a : M.a) = 2
let (b : M.b) = 2
let _ = A.a = B.b
module Std = struct module Hash = Hashtbl end;;

open Std;;
module Hash1 : module type of Hash = Hash;;
module Hash2 : sig include (module type of Hash) end = Hash;;
let f1 (x : (_,_) Hash1.t) = (x : (_,_) Hashtbl.t);;
let f2 (x : (_,_) Hash2.t) = (x : (_,_) Hashtbl.t);;

(* Another case, not using include *)

module Std2 = struct module M = struct type t end end;;
module Std' = Std2;;
module M' : module type of Std'.M = Std2.M;;
let f3 (x : M'.t) = (x : Std2.M.t);;

(* original report required Core_kernel:
module type S = sig
open Core_kernel.Std

module Hashtbl1 : module type of Hashtbl
module Hashtbl2 : sig
  include (module type of Hashtbl)
end

module Coverage : Core_kernel.Std.Hashable

type types = unit constraint 'a Coverage.Table.t = (Coverage.t, 'a) Hashtbl1.t
type doesnt_type = unit
  constraint 'a Coverage.Table.t = (Coverage.t, 'a) Hashtbl2.t
end
*)
module type INCLUDING = sig
  include module type of List
  include module type of ListLabels
end

module Including_typed: INCLUDING = struct
  include List
  include ListLabels
end
module X=struct
  module type SIG=sig type t=int val x:t end
  module F(Y:SIG) : SIG = struct type t=Y.t let x=Y.x end
end;;
module DUMMY=struct type t=int let x=2 end;;
let x = (3 : X.F(DUMMY).t);;

module X2=struct
  module type SIG=sig type t=int val x:t end
  module F(Y:SIG)(Z:SIG) = struct
    type t=Y.t
    let x=Y.x
    type t'=Z.t
    let x'=Z.x
  end
end;;
let x = (3 : X2.F(DUMMY)(DUMMY).t);;
let x = (3 : X2.F(DUMMY)(DUMMY).t');;
module F (M : sig
    type 'a t
    type 'a u = string
    val f : unit -> _ u t
  end) = struct
    let t = M.f ()
  end
type 't a = [ `A ]
type 't wrap = 't constraint 't = [> 't wrap a ]
type t = t a wrap

module T = struct
  let foo : 't wrap -> 't wrap -> unit = fun _ _ -> ()
  let bar : ('a a wrap as 'a) = `A
end

module Good : sig
  val bar: t
  val foo: t -> t -> unit
end = T

module Bad : sig
  val foo: t -> t -> unit
  val bar: t
end = T
module M : sig
  module type T
  module F (X : T) : sig end
end = struct
  module type T = sig end
  module F (X : T) = struct end
end

module type T = M.T

module F : functor (X : T) -> sig end = M.F
module type S = sig type t = { a : int; b : int; } end;;
let f (module M : S with type t = int) = { M.a = 0 };;
let flag = ref false
module F(S : sig module type T end) (A : S.T) (B : S.T) =
struct
  module X = (val if !flag then (module A) else (module B) : S.T)
end

(* If the above were accepted, one could break soundness *)
module type S = sig type t val x : t end
module Float = struct type t = float let x = 0.0 end
module Int = struct type t = int let x = 0 end

module M = F(struct module type T = S end)

let () = flag := false
module M1 = M(Float)(Int)

let () = flag := true
module M2 = M(Float)(Int)

let _ = [| M2.X.x; M1.X.x |]
module type PR6513 = sig
module type S = sig type u end

module type T = sig
  type 'a wrap
  type uri
end

module Make: functor (Html5 : T with type 'a wrap = 'a) ->
  S with type u = < foo : Html5.uri >
end

(* Requires -package tyxml
module type PR6513_orig = sig
module type S =
sig
        type t
        type u
end

module Make: functor (Html5: Html5_sigs.T
                             with type 'a Xml.wrap = 'a and
                             type 'a wrap = 'a and
                             type 'a list_wrap = 'a list)
                     -> S with type t = Html5_types.div Html5.elt and
                               type u = < foo: Html5.uri >
end
*)
module type S = sig
  include Set.S
  module E : sig val x : int end
end

module Make(O : Set.OrderedType) : S with type elt = O.t =
  struct
    include Set.Make(O)
    module E = struct let x = 1 end
  end

module rec A : Set.OrderedType = struct
 type t = int
  let compare = Pervasives.compare
end
and B : S = struct
 module C = Make(A)
 include C
end
module type S = sig
  module type T
  module X : T
end

module F (X : S) = X.X

module M = struct
  module type T = sig type t end
  module X = struct type t = int end
end

type t = F(M).t
module Common0 =
 struct
   type msg = Msg

   let handle_msg = ref (function _ -> failwith "Unable to handle message")
   let extend_handle f =
   let old = !handle_msg in
   handle_msg := f old

   let q : _ Queue.t = Queue.create ()
   let add msg = Queue.add msg q
   let handle_queue_messages () = Queue.iter !handle_msg q
 end

let q' : Common0.msg Queue.t = Common0.q

module Common =
 struct
   type msg = ..

   let handle_msg = ref (function _ -> failwith "Unable to handle message")
   let extend_handle f =
   let old = !handle_msg in
   handle_msg := f old

   let q : _ Queue.t = Queue.create ()
   let add msg = Queue.add msg q
   let handle_queue_messages () = Queue.iter !handle_msg q
 end

module M1 =
 struct
   type Common.msg += Reload of string | Alert of string

   let handle fallback = function
     Reload s -> print_endline ("Reload "^s)
   | Alert s -> print_endline ("Alert "^s)
   | x -> fallback x

   let () = Common.extend_handle handle
   let () = Common.add (Reload "config.file")
   let () = Common.add (Alert "Initialisation done")
 end
let should_reject =
  let table = Hashtbl.create 1 in
  fun x y -> Hashtbl.add table x y
type 'a t = 'a option
let is_some = function
  | None -> false
  | Some _ -> true

let should_accept ?x () = is_some x
include struct
  let foo `Test = ()
  let wrap f `Test = f
  let bar = wrap ()
end
let f () =
   let module S = String in
   let module N = Map.Make(S) in
   N.add "sum" 41 N.empty;;
module X = struct module Y = struct module type S = sig type t end end end

(* open X  (* works! *) *)
module Y = X.Y

type 'a arg_t = 'at constraint 'a = (module Y.S with type t = 'at)
type t = (module X.Y.S with type t = unit)

let f (x : t arg_t) = ()

let () = f ()
module type S =
sig
  type a
  type b
end
module Foo
    (Bar : S with type a = private [> `A])
    (Baz : S with type b = private < b : Bar.b ; .. >) =
struct
end
module A = struct
 module type A_S = sig
 end

 type t = (module A_S)
end

module type S = sig type t end

let f (type a) (module X : S with type t = a) = ()

let _ = f (module A) (* ok *)

module A_annotated_alias : S with type t = (module A.A_S) = A

let _ = f (module A_annotated_alias) (* ok *)
let _ = f (module A_annotated_alias : S with type t = (module A.A_S)) (* ok *)

module A_alias = A
module A_alias_expanded = struct include A_alias end

let _ = f (module A_alias_expanded : S with type t = (module A.A_S)) (* ok *)
let _ = f (module A_alias_expanded) (* ok *)

let _ = f (module A_alias : S with type t = (module A.A_S)) (* doesn't type *)
let _ = f (module A_alias) (* doesn't type either *)
module Foo
 (Bar : sig type a = private [> `A ] end)
 (Baz : module type of struct include Bar end) =
struct
end
module Bazoinks = struct type a = [ `A ] end
module Bug = Foo(Bazoinks)(Bazoinks)
(* PR#6992, reported by Stephen Dolan *)

type (_, _) eq = Eq : ('a, 'a) eq
let cast : type a b . (a, b) eq -> a -> b = fun Eq x -> x

module Fix (F : sig type 'a f end) = struct
  type 'a fix = ('a, 'a F.f) eq
  let uniq (type a) (type b) (Eq : a fix) (Eq : b fix) : (a, b) eq = Eq
end

(* This would allow:
module FixId = Fix (struct type 'a f = 'a end)
 let bad : (int, string) eq = FixId.uniq Eq Eq
 let _ = Printf.printf "Oh dear: %s" (cast bad 42)
*)
module M = struct
 module type S = sig type a val v : a end
 type 'a s = (module S with type a = 'a)
end

module B = struct
 class type a = object method a : 'a. 'a M.s -> 'a end
end

module M' = M
module B' = B

class b : B.a = object
 method a : 'a. 'a M.s -> 'a = fun (type a) ((module X) : (module M.S with type
a = a)) -> X.v
end

class b' : B.a = object
 method a : 'a. 'a M'.s -> 'a = fun (type a) ((module X) : (module M'.S with
type a = a)) -> X.v
end
module type FOO = sig type t end
module type BAR =
sig
  (* Works: module rec A : (sig include FOO with type t = < b:B.t > end) *)
  module rec A : (FOO with type t = < b:B.t >)
         and B : FOO
end
module A = struct module type S module S = struct end end
module F (_ : sig end) = struct module type S module S = A.S end
module M = struct end
module N = M
module G (X : F(N).S) : A.S = X
module F (_ : sig end) = struct module type S end
module M = struct end
module N = M
module G (X : F(N).S) : F(M).S = X
module M :  sig
  type make_dec
  val add_dec: make_dec -> unit
end = struct
  type u

  module Fast: sig
    type 'd t
    val create: unit -> 'd t
    module type S = sig
      module Data: sig type t end
      val key: Data.t t
    end
    module Register (D:S): sig end
    val attach: 'd t -> 'd -> unit
  end = struct
    type 'd t = unit
    let create () = ()
    module type S = sig
      module Data: sig type t end
      val key: Data.t t
    end
    module Register (D:S) = struct end
    let attach _ _ = ()
  end

  type make_dec

  module Dem = struct
    module Data = struct
      type t = make_dec
    end
    let key = Fast.create ()
  end

  module EDem = Fast.Register(Dem)

  let add_dec dec =
    Fast.attach Dem.key dec
end

(* simpler version *)

module Simple = struct
  type 'a t
  module type S = sig
    module Data: sig type t end
    val key: Data.t t
  end
  module Register (D:S) = struct let key = D.key end
  module M = struct
    module Data = struct type t = int end
    let key : _ t = Obj.magic ()
  end
end;;
module EM = Simple.Register(Simple.M);;
Simple.M.key;;

module Simple2 = struct
  type 'a t
  module type S = sig
    module Data: sig type t end
    val key: Data.t t
  end
  module M = struct
    module Data = struct type t = int end
    let key : _ t = Obj.magic ()
  end
  module Register (D:S) = struct let key = D.key end
  module EM = Simple.Register(Simple.M)
  let k : M.Data.t t = M.key
end;;
module rec M
    : sig external f : int -> int = "%identity" end
    = struct external f : int -> int = "%identity" end
(* with module *)

module type S = sig type t and s = t end;;
module type S' = S with type t := int;;

module type S = sig module rec M : sig end and N : sig end end;;
module type S' = S with module M := String;;

(* with module type *)
(*
module type S = sig module type T module F(X:T) : T end;;
module type T0 = sig type t end;;
module type S1 = S with module type T = T0;;
module type S2 = S with module type T := T0;;
module type S3 = S with module type T := sig type t = int end;;
module H = struct
  include (Hashtbl : module type of Hashtbl with
           type statistics := Hashtbl.statistics
           and module type S := Hashtbl.S
           and module Make := Hashtbl.Make
           and module MakeSeeded := Hashtbl.MakeSeeded
           and module type SeededS := Hashtbl.SeededS
           and module type HashedType := Hashtbl.HashedType
           and module type SeededHashedType := Hashtbl.SeededHashedType)
end;;
*)

(* A subtle problem appearing with -principal *)
type -'a t
class type c = object method m : [ `A ] t end;;
module M : sig val v : (#c as 'a) -> 'a end =
  struct let v x = ignore (x :> c); x end;;

(* PR#4838 *)

let id = let module M = struct end in fun x -> x;;

(* PR#4511 *)

let ko = let module M = struct end in fun _ -> ();;

(* PR#5993 *)

module M : sig type -'a t = private int end =
  struct type +'a t = private int end
;;

(* PR#6005 *)

module type A = sig type t = X of int end;;
type u = X of bool;;
module type B = A with type t = u;; (* fail *)

(* PR#5815 *)
(* ---> duplicated exception name is now an error *)

module type S = sig exception Foo of int  exception Foo of bool end;;

(* PR#6410 *)

module F(X : sig end) = struct let x = 3 end;;
F.x;; (* fail *)
module C = Char;;
C.chr 66;;

module C' : module type of Char = C;;
C'.chr 66;;

module C3 = struct include Char end;;
C3.chr 66;;

let f x = let module M = struct module L = List end in M.L.length x;;
let g x = let module L = List in L.length (L.map succ x);;

module F(X:sig end) = Char;;
module C4 = F(struct end);;
C4.chr 66;;

module G(X:sig end) = struct module M = X end;; (* does not alias X *)
module M = G(struct end);;

module M' = struct
  module N = struct let x = 1 end
  module N' = N
end;;
M'.N'.x;;

module M'' : sig module N' : sig val x : int end end = M';;
M''.N'.x;;
module M2 = struct include M' end;;
module M3 : sig module N' : sig val x : int end end = struct include M' end;;
M3.N'.x;;
module M3' : sig module N' : sig val x : int end end = M2;;
M3'.N'.x;;

module M4 : sig module N' : sig val x : int end end = struct
  module N = struct let x = 1 end
  module N' = N
end;;
M4.N'.x;;

module F(X:sig end) = struct
  module N = struct let x = 1 end
  module N' = N
end;;
module G : functor(X:sig end) -> sig module N' : sig val x : int end end = F;;
module M5 = G(struct end);;
M5.N'.x;;

module M = struct
  module D = struct let y = 3 end
  module N = struct let x = 1 end
  module N' = N
end;;

module M1 : sig module N : sig val x : int end module N' = N end = M;;
M1.N'.x;;
module M2 : sig module N' : sig val x : int end end =
  (M : sig module N : sig val x : int end module N' = N end);;
M2.N'.x;;

open M;;
N'.x;;

module M = struct
  module C = Char
  module C' = C
end;;
module M1
  : sig module C : sig val escaped : char -> string end module C' = C end
  = M;; (* sound, but should probably fail *)
M1.C'.escaped 'A';;
module M2 : sig module C' : sig val chr : int -> char end end =
  (M : sig module C : sig val chr : int -> char end module C' = C end);;
M2.C'.chr 66;;

StdLabels.List.map;;

module Q = Queue;;
exception QE = Q.Empty;;
try Q.pop (Q.create ()) with QE -> "Ok";;

module type Complex = module type of Complex with type t = Complex.t;;
module M : sig module C : Complex end = struct module C = Complex end;;

module C = Complex;;
C.one.Complex.re;;
include C;;

module F(X:sig module C = Char end) = struct module C = X.C end;;

(* Applicative functors *)
module S = String
module StringSet = Set.Make(String)
module SSet = Set.Make(S);;
let f (x : StringSet.t) = (x : SSet.t);;

(* Also using include (cf. Leo's mail 2013-11-16) *)
module F (M : sig end) : sig type t end = struct type t = int end
module T = struct
  module M = struct end
  include F(M)
end;;
include T;;
let f (x : t) : T.t = x ;;

(* PR#4049 *)
(* This works thanks to abbreviations *)
module A = struct
  module B = struct type t let compare x y = 0 end
  module S = Set.Make(B)
  let empty = S.empty
end
module A1 = A;;
A1.empty = A.empty;;

(* PR#3476 *)
(* Does not work yet *)
module FF(X : sig end) = struct type t end
module M = struct
  module X = struct end
  module Y = FF (X) (* XXX *)
  type t = Y.t
end
module F (Y : sig type t end) (M : sig type t = Y.t end) = struct end;;

module G = F (M.Y);;
(*module N = G (M);;
module N = F (M.Y) (M);;*)

(* PR#6307 *)

module A1 = struct end
module A2 = struct end
module L1 = struct module X = A1 end
module L2 = struct module X = A2 end;;

module F (L : (module type of L1)) = struct end;;

module F1 = F(L1);; (* ok *)
module F2 = F(L2);; (* should succeed too *)

(* Counter example: why we need to be careful with PR#6307 *)
module Int = struct type t = int let compare = compare end
module SInt = Set.Make(Int)
type (_,_) eq = Eq : ('a,'a) eq
type wrap = W of (SInt.t, SInt.t) eq

module M = struct
  module I = Int
  type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(I).t) eq
end;;
module type S = module type of M;; (* keep alias *)

module Int2 = struct type t = int let compare x y = compare y x end;;
module type S' = sig
  module I = Int2
  include S with module I := I
end;; (* fail *)

(* (* if the above succeeded, one could break invariants *)
module rec M2 : S' = M2;; (* should succeed! (but this is bad) *)

let M2.W eq = W Eq;;

let s = List.fold_right SInt.add [1;2;3] SInt.empty;;
module SInt2 = Set.Make(Int2);;
let conv : type a b. (a,b) eq -> a -> b = fun Eq x -> x;;
let s' : SInt2.t = conv eq s;;
SInt2.elements s';;
SInt2.mem 2 s';; (* invariants are broken *)
*)

(* Check behavior with submodules *)
module M = struct
  module N = struct module I = Int end
  module P = struct module I = N.I end
  module Q = struct
    type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(P.I).t) eq
  end
end;;
module type S = module type of M ;;

module M = struct
  module N = struct module I = Int end
  module P = struct module I = N.I end
  module Q = struct
    type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(N.I).t) eq
  end
end;;
module type S = module type of M ;;

(* PR#6365 *)
module type S = sig module M : sig type t val x : t end end;;
module H = struct type t = A let x = A end;;
module H' = H;;
module type S' = S with module M = H';; (* shouldn't introduce an alias *)

(* PR#6376 *)
module type Alias = sig module N : sig end module M = N end;;
module F (X : sig end) = struct type t end;;
module type A = Alias with module N := F(List);;
module rec Bad : A = Bad;;

(* Shinwell 2014-04-23 *)
module B = struct
 module R = struct
   type t = string
 end

 module O = R
end

module K = struct
 module E = B
 module N = E.O
end;;

let x : K.N.t = "foo";;

(* PR#6465 *)

module M = struct type t = A module B = struct type u = B end end;;
module P : sig type t = M.t = A module B = M.B end = M;; (* should be ok *)
module P : sig type t = M.t = A module B = M.B end = struct include M end;;

module type S = sig
  module M : sig module P : sig end end
  module Q = M
end;;
module type S = sig
  module M : sig module N : sig end module P : sig end end
  module Q : sig module N = M.N module P = M.P end
end;;
module R = struct
  module M = struct module N = struct end module P = struct end end
  module Q = M
end;;
module R' : S = R;; (* should be ok *)

(* PR#6578 *)

module M = struct let f x = x end
module rec R : sig module M : sig val f : 'a -> 'a end end =
  struct module M = M end;;
R.M.f 3;;
module rec R : sig module M = M end = struct module M = M end;;
R.M.f 3;;
open A
let f =
  L.map S.capitalize

let () =
  L.iter print_endline (f ["jacques"; "garrigue"])

module C : sig module L : module type of List end = struct include A end

(* The following introduces a (useless) dependency on A:
module C : sig module L : module type of List end = A
*)

include D'
(*
let () =
  print_endline (string_of_int D'.M.y)
*)
open A
let f =
  L.map S.capitalize

let () =
  L.iter print_endline (f ["jacques"; "garrigue"])

module C : sig module L : module type of List end = struct include A end

(* The following introduces a (useless) dependency on A:
module C : sig module L : module type of List end = A
*)

(* No dependency on D *)
let x = 3
module M = struct let y = 5 end
module type S = sig type u type t end;;
module type S' = sig type t = int type u = bool end;;

(* ok to convert between structurally equal signatures, and parameters
   are inferred *)
let f (x : (module S with type t = 'a and type u = 'b)) = (x : (module S'));;
let g x = (x : (module S with type t = 'a and type u = 'b) :> (module S'));;

(* with subtyping it is also ok to forget some types *)
module type S2 = sig type u type t type w end;;
let g2 x = (x : (module S2 with type t = 'a and type u = 'b) :> (module S'));;
let h x = (x : (module S2 with type t = 'a) :> (module S with type t = 'a));;
let f2 (x : (module S2 with type t = 'a and type u = 'b)) =
  (x : (module S'));; (* fail *)
let k (x : (module S2 with type t = 'a)) =
  (x : (module S with type t = 'a));; (* fail *)

(* but you cannot forget values (no physical coercions) *)
module type S3 = sig type u type t val x : int end;;
let g3 x =
  (x : (module S3 with type t = 'a and type u = 'b) :> (module S'));; (* fail *)
(* Using generative functors *)

(* Without type *)
module type S = sig val x : int end;;
let v = (module struct let x = 3 end : S);;
module F() = (val v);; (* ok *)
module G (X : sig end) : S = F ();; (* ok *)
module H (X : sig end) = (val v);; (* ok *)

(* With type *)
module type S = sig type t val x : t end;;
let v = (module struct type t = int let x = 3 end : S);;
module F() = (val v);; (* ok *)
module G (X : sig end) : S = F ();; (* fail *)
module H() = F();; (* ok *)

(* Alias *)
module U = struct end;;
module M = F(struct end);; (* ok *)
module M = F(U);; (* fail *)

(* Cannot coerce between applicative and generative *)
module F1 (X : sig end) = struct end;;
module F2 : functor () -> sig end = F1;; (* fail *)
module F3 () = struct end;;
module F4 : functor (X : sig end) -> sig end = F3;; (* fail *)

(* tests for shortened functor notation () *)
module X (X: sig end) (Y: sig end) = functor (Z: sig end) -> struct end;;
module Y = functor (X: sig end) (Y:sig end) -> functor (Z: sig end) ->
  struct end;;
module Z = functor (_: sig end) (_:sig end) (_: sig end) -> struct end;;
module GZ : functor (X: sig end) () (Z: sig end) -> sig end
          = functor (X: sig end) () (Z: sig end) -> struct end;;
module F (X : sig end) = struct type t = int end;;
type t = F(Does_not_exist).t;;
type expr =
  [ `Abs of string * expr
  | `App of expr * expr
  ]

class type exp =
object
  method eval : (string, exp) Hashtbl.t -> expr
end;;

class app e1 e2 : exp =
object
  val l = e1
  val r = e2
  method eval env =
      match l with
    | `Abs(var,body) ->
        Hashtbl.add env var r;
        body
    | _ -> `App(l,r);
end

class virtual ['subject, 'event] observer =
   object
     method virtual notify : 'subject ->  'event -> unit
   end

class ['event] subject =
   object (self : 'subject)
     val mutable observers = ([]: (('subject, 'event) observer) list)
     method add_observer obs = observers <- (obs :: observers)
     method notify_observers (e : 'event) =
         List.iter (fun x -> x#notify self e) observers
   end

type id = int

class entity (id : id) =
  object
    val ent_destroy_subject = new subject
    method destroy_subject : (id) subject = ent_destroy_subject

    method entity_id = id
  end

class ['entity] entity_container =
  object (self)
    inherit ['entity, id] observer as observer

    method add_entity (e : 'entity) =
      e#destroy_subject#add_observer (self)

    method notify _ id = ()
  end

let f (x : entity entity_container) = ()

(*
class world =
  object
    val entity_container : entity entity_container = new entity_container

    method add_entity (s : entity) =
      entity_container#add_entity (s :> entity)

  end
*)
(* Two v's in the same class *)
class c v = object initializer  print_endline v val v = 42 end;;
new c "42";;

(* Two hidden v's in the same class! *)
class c (v : int) =
  object
    method v0 = v
    inherit ((fun v -> object method v : string = v end) "42")
  end;;
(new c 42)#v0;;
class virtual ['a] c =
object (s : 'a)
  method virtual m : 'b
end

let o =
    object (s :'a)
      inherit ['a] c
      method m = 42
    end
module M :
   sig
     class x : int -> object method m : int end
  end
=
struct
  class x _ = object
    method m = 42
  end
end;;
module M : sig class c : 'a -> object val x : 'b end end =
  struct class c x = object val x = x end end

class c (x : int) = object inherit M.c x method x : bool = x end

let r = (new c 2)#x;;
(* test.ml *)
class alfa = object(_:'self)
  method x: 'a. ('a, out_channel, unit) format -> 'a = Printf.printf
end

class bravo a = object
  val y = (a :> alfa)
  initializer y#x "bravo initialized"
end

class charlie a = object
  inherit bravo a
  initializer y#x "charlie initialized"
end
(* The module begins *)
exception Out_of_range

class type ['a] cursor =
  object
    method get : 'a
    method incr : unit -> unit
    method is_last : bool
  end

class type ['a] storage =
  object ('self)
    method first : 'a cursor
    method len : int
    method nth : int -> 'a cursor
    method copy : 'self
    method sub : int -> int -> 'self
    method concat : 'a storage -> 'self
    method fold : 'b. ('a -> int -> 'b -> 'b) -> 'b -> 'b
    method iter : ('a -> unit) -> unit
  end

class virtual ['a, 'cursor] storage_base =
  object (self : 'self)
    constraint 'cursor = 'a #cursor
    method virtual first : 'cursor
    method virtual len : int
    method virtual copy : 'self
    method virtual sub : int -> int -> 'self
    method virtual concat : 'a storage -> 'self
    method fold : 'b. ('a -> int -> 'b -> 'b) -> 'b -> 'b = fun f a0 ->
      let cur = self#first in
      let rec loop count a =
        if count >= self#len then a else
        let a' = f cur#get count a in
        cur#incr (); loop (count + 1) a'
      in
      loop 0 a0
    method iter proc =
      let p = self#first in
      for i = 0 to self#len - 2 do proc p#get; p#incr () done;
      if self#len > 0 then proc p#get else ()
  end

class type ['a] obj_input_channel =
  object
    method get : unit -> 'a
    method close : unit -> unit
  end

class type ['a] obj_output_channel =
  object
    method put : 'a -> unit
    method flush : unit -> unit
    method close : unit -> unit
  end

module UChar =
struct

  type t = int

  let highest_bit = 1 lsl 30
  let lower_bits = highest_bit - 1

  let char_of c =
    try Char.chr c with Invalid_argument _ ->  raise Out_of_range

  let of_char = Char.code

  let code c =
    if c lsr 30 = 0
    then c
    else raise Out_of_range

  let chr n =
    if n >= 0 && (n lsr 31 = 0) then n else raise Out_of_range

  let uint_code c = c
  let chr_of_uint n = n

end

type uchar = UChar.t

let int_of_uchar u = UChar.uint_code u
let uchar_of_int n = UChar.chr_of_uint n

class type ucursor = [uchar] cursor

class type ustorage = [uchar] storage

class virtual ['ucursor] ustorage_base = [uchar, 'ucursor] storage_base

module UText =
struct

(* the internal representation is UCS4 with big endian*)
(* The most significant digit appears first. *)
let get_buf s i =
  let n = Char.code s.[i] in
  let n = (n lsl 8) lor (Char.code s.[i + 1]) in
  let n = (n lsl 8) lor (Char.code s.[i + 2]) in
  let n = (n lsl 8) lor (Char.code s.[i + 3]) in
  UChar.chr_of_uint n

let set_buf s i u =
  let n = UChar.uint_code u in
  begin
    s.[i] <- Char.chr (n lsr 24);
    s.[i + 1] <- Char.chr (n lsr 16 lor 0xff);
    s.[i + 2] <- Char.chr (n lsr 8 lor 0xff);
    s.[i + 3] <- Char.chr (n lor 0xff);
  end

let init_buf buf pos init =
  if init#len = 0 then () else
  let cur = init#first in
  for i = 0 to init#len - 2 do
    set_buf buf (pos + i lsl 2) (cur#get); cur#incr ()
  done;
  set_buf buf (pos + (init#len - 1) lsl 2) (cur#get)

let make_buf init =
  let s = String.create (init#len lsl 2) in
  init_buf s 0 init; s

class text_raw buf =
  object (self : 'self)
    inherit [cursor] ustorage_base
    val contents = buf
    method first = new cursor (self :> text_raw) 0
    method len = (String.length contents) / 4
    method get i = get_buf contents (4 * i)
    method nth i = new cursor (self :> text_raw) i
    method copy = {< contents = String.copy contents >}
    method sub pos len =
      {< contents = String.sub contents (pos * 4) (len * 4) >}
    method concat (text : ustorage) =
      let buf = String.create (String.length contents + 4 * text#len) in
      String.blit contents 0 buf 0 (String.length contents);
      init_buf buf (String.length contents) text;
      {< contents = buf >}
  end
and cursor text i =
  object
    val contents = text
    val mutable pos = i
    method get = contents#get pos
    method incr () = pos <- pos + 1
    method is_last = (pos + 1 >= contents#len)
  end

class string_raw buf =
  object
    inherit text_raw buf
    method set i u = set_buf contents (4 * i) u
  end

class text init = text_raw (make_buf init)
class string init = string_raw (make_buf init)

let of_string s =
  let buf = String.make (4 * String.length s) '\000' in
  for i = 0 to String.length s - 1 do
    buf.[4 * i] <- s.[i]
  done;
  new text_raw buf

let make len u =
  let s = String.create (4 * len) in
  for i = 0 to len - 1 do set_buf s (4 * i) u done;
  new string_raw s

let create len = make len (UChar.chr 0)

let copy s = s#copy

let sub s start len = s#sub start len

let fill s start len u =
  for i = start to start + len - 1 do s#set i u done

let blit src srcoff dst dstoff len =
  for i = 0 to len - 1 do
    let u = src#get (srcoff + i) in
    dst#set (dstoff + i) u
  done

let concat s1 s2 = s1#concat (s2 (* : #ustorage *) :> uchar storage)

let iter proc s = s#iter proc
end
class type foo_t =
  object
    method foo: string
  end

type 'a name =
    Foo: foo_t name
  | Int: int name
;;

class foo =
  object(self)
    method foo = "foo"
    method cast =
      function
          Foo -> (self :> <foo : string>)
  end
;;

class foo: foo_t =
  object(self)
    method foo = "foo"
    method cast: type a. a name -> a =
      function
          Foo -> (self :> foo_t)
        | _ -> raise Exit
  end
;;
class type c = object end;;
module type S = sig class c: c end;;
class virtual name =
object
end

and func (args_ty, ret_ty) =
object(self)
  inherit name

  val mutable memo_args = None

  method arguments =
    match memo_args with
    | Some xs -> xs
    | None ->
      let args = List.map (fun ty -> new argument(self, ty)) args_ty in
        memo_args <- Some args; args
end

and argument (func, ty) =
object
  inherit name
end
;;
let f (x: #M.foo) = 0;;
class type ['e] t = object('s)
  method update : 'e -> 's
end;;

module type S = sig
  class base : 'e -> ['e] t
end;;
type 'par t = 'par
module M : sig val x : <m : 'a. 'a> end =
  struct let x : <m : 'a. 'a t> = Obj.magic () end

let ident v = v
class alias = object method alias : 'a . 'a t -> 'a = ident end
module Classdef = struct
  class virtual ['a, 'b, 'c] cl0 =
    object
      constraint 'c = < m : 'a -> 'b -> int; .. >
    end

  class virtual ['a, 'b] cl1 =
    object
      method virtual raise_trouble : int -> 'a
      method virtual m : 'a -> 'b -> int
    end

  class virtual ['a, 'b] cl2 =
    object
      method virtual as_cl0 : ('a, 'b, ('a, 'b) cl1) cl0
    end
end

type refer1 = < poly : 'a 'b 'c . (('b, 'c) #Classdef.cl2 as 'a) >
type refer2 = < poly : 'a 'b 'c . (('b, 'c) #Classdef.cl2 as 'a) >

(* Actually this should succeed ... *)
let f (x : refer1) = (x : refer2)
module Classdef = struct
  class virtual ['a, 'b, 'c] cl0 =
    object
      constraint 'c = < m : 'a -> 'b -> int; .. >
    end

  class virtual ['a, 'b] cl1 =
    object
      method virtual raise_trouble : int -> 'a
      method virtual m : 'a -> 'b -> int
    end

  class virtual ['a, 'b] cl2 =
    object
      method virtual as_cl0 : ('a, 'b, ('a, 'b) cl1) cl0
    end
end

module M : sig
  type refer = { poly : 'a 'b 'c . (('b, 'c) #Classdef.cl2 as 'a) }
end = struct
  type refer = { poly : 'a 'b 'c . (('b, 'c) #Classdef.cl2 as 'a) }
end
(*
  ocamlc -c pr3918a.mli pr3918b.mli
  rm -f pr3918a.cmi
  ocamlc -c pr3918c.ml
*)

open Pr3918b

let f x = (x : 'a vlist :> 'b vlist)
let f (x : 'a vlist) = (x : 'b vlist)
module type Poly = sig
  type 'a t = 'a constraint 'a = [> ]
end

module Combine (A : Poly) (B : Poly) = struct
  type ('a, 'b) t = 'a A.t constraint 'a = 'b B.t
end

module C = Combine
  (struct type 'a t = 'a constraint 'a = [> ] end)
  (struct type 'a t = 'a constraint 'a = [> ] end)
module type Priv = sig
  type t = private int
end

module Make (Unit:sig end): Priv = struct type t = int end

module A = Make (struct end)

module type Priv' = sig
  type t = private [> `A]
end

module Make' (Unit:sig end): Priv' = struct type t = [`A] end

module A' = Make' (struct end)
(* PR5057 *)

module TT = struct
  module IntSet = Set.Make(struct type t = int let compare = compare end)
end

let () =
  let f flag =
    let module T = TT in
    let _ = match flag with `A -> 0 | `B r -> r in
    let _ = match flag with `A -> T.IntSet.mem | `B r -> r in
    ()
  in
  f `A
(* This one should fail *)

let f flag =
  let module T = Set.Make(struct type t = int let compare = compare end) in
  let _ = match flag with `A -> 0 | `B r -> r in
  let _ = match flag with `A -> T.mem | `B r -> r in
  ()
module type S = sig
 type +'a t

 val foo : [`A] t -> unit
 val bar : [< `A | `B] t -> unit
end

module Make(T : S) = struct
 let f x =
   T.foo x;
   T.bar x;
   (x :> [`A | `C] T.t)
end
type 'a termpc =
    [`And of 'a * 'a
    |`Or of 'a * 'a
    |`Not of 'a
    |`Atom of string
    ]

type 'a termk =
    [`Dia of 'a
    |`Box of 'a
    |'a termpc
    ]

module type T = sig
  type term
  val map : (term -> term) -> term -> term
  val nnf : term -> term
  val nnf_not : term -> term
end

module Fpc(X : T with type term = private [> 'a termpc] as 'a) =
  struct
    type term = X.term termpc
    let nnf = function
      |`Not(`Atom _) as x -> x
      |`Not x     -> X.nnf_not x
      | x         -> X.map X.nnf x
    let map f : term -> X.term = function
      |`Not x    -> `Not (f x)
      |`And(x,y) -> `And (f x, f y)
      |`Or (x,y) -> `Or  (f x, f y)
      |`Atom _ as x -> x
    let nnf_not : term -> _ = function
      |`Not x    -> X.nnf x
      |`And(x,y) -> `Or  (X.nnf_not x, X.nnf_not y)
      |`Or (x,y) -> `And (X.nnf_not x, X.nnf_not y)
      |`Atom _ as x -> `Not x
  end

module Fk(X : T with type term = private [> 'a termk] as 'a) =
  struct
    type term = X.term termk
    module Pc = Fpc(X)
    let map f : term -> _ = function
      |`Dia x -> `Dia (f x)
      |`Box x -> `Box (f x)
      |#termpc as x -> Pc.map f x
    let nnf = Pc.nnf
    let nnf_not : term -> _ = function
      |`Dia x -> `Box (X.nnf_not x)
      |`Box x -> `Dia (X.nnf_not x)
      |#termpc as x -> Pc.nnf_not x
  end
type untyped;;
type -'a typed = private untyped;;
type -'typing wrapped = private sexp
and +'a t = 'a typed wrapped
and sexp = private untyped wrapped;;
class type ['a] s3 = object
  val underlying : 'a t
end;;
class ['a] s3object r : ['a] s3 = object
  val underlying = r
end;;
module M (T:sig type t end)
 = struct type t = private { t : T.t } end
module P
 = struct
       module T = struct type t end
       module R = M(T)
 end
module Foobar : sig
  type t = private int
end = struct
  type t = int
end;;

module F0 : sig type t = private int end = Foobar;;

let f (x : F0.t) = (x : Foobar.t);; (* fails *)

module F = Foobar;;

let f (x : F.t) = (x : Foobar.t);;

module M = struct type t = <m:int> end;;
module M1 : sig type t = private <m:int; ..> end = M;;
module M2 :  sig type t = private <m:int; ..> end = M1;;
fun (x : M1.t) -> (x : M2.t);; (* fails *)

module M3 : sig type t = private M1.t end = M1;;
fun x -> (x : M3.t :> M1.t);;
fun x -> (x : M3.t :> M.t);;
module M4 : sig type t = private M3.t end = M2;; (* fails *)
module M4 : sig type t = private M3.t end = M;; (* fails *)
module M4 : sig type t = private M3.t end = M1;; (* might be ok *)
module M5 : sig type t = private M1.t end = M3;;
module M6 : sig type t = private < n:int; .. > end = M1;; (* fails *)

module Bar : sig type t = private Foobar.t val f : int -> t end =
  struct type t = int let f (x : int) = (x : t) end;; (* must fail *)

module M : sig
  type t = private T of int
  val mk : int -> t
end = struct
  type t = T of int
  let mk x = T(x)
end;;

module M1 : sig
  type t = M.t
  val mk : int -> t
end = struct
  type t = M.t
  let mk = M.mk
end;;

module M2 : sig
  type t = M.t
  val mk : int -> t
end = struct
  include M
end;;

module M3 : sig
  type t = M.t
  val mk : int -> t
end = M;;

module M4 : sig
    type t = M.t = T of int
    val mk : int -> t
  end = M;;
(* Error: The variant or record definition does not match that of type M.t *)

module M5 : sig
  type t = M.t = private T of int
  val mk : int -> t
end = M;;

module M6 : sig
  type t = private T of int
  val mk : int -> t
end = M;;

module M' : sig
  type t_priv = private T of int
  type t = t_priv
  val mk : int -> t
end = struct
  type t_priv = T of int
  type t = t_priv
  let mk x = T(x)
end;;

module M3' : sig
  type t = M'.t
  val mk : int -> t
end = M';;

module M : sig type 'a t = private T of 'a end =
  struct type 'a t = T of 'a end;;

module M1 : sig type 'a t = 'a M.t = private T of 'a end =
  struct type 'a t = 'a M.t = private T of 'a end;;

(* PR#6090 *)
module Test = struct type t = private A end
module Test2 : module type of Test with type t = Test.t = Test;;
let f (x : Test.t) = (x : Test2.t);;
let f Test2.A = ();;
let a = Test2.A;; (* fail *)
(* The following should fail from a semantical point of view,
   but allow it for backward compatibility *)
module Test2 : module type of Test with type t = private Test.t = Test;;

(* PR#6331 *)
type t = private < x : int; .. > as 'a;;
type t = private (< x : int; .. > as 'a) as 'a;;
type t = private < x : int > as 'a;;
type t = private (< x : int > as 'a) as 'b;;
type 'a t = private < x : int; .. > as 'a;;
type 'a t = private 'a constraint 'a = < x : int; .. >;;
(* Bad (t = t) *)
module rec A : sig type t = A.t end = struct type t = A.t end;;
(* Bad (t = t) *)
module rec A : sig type t = B.t end = struct type t = B.t end
       and B : sig type t = A.t end = struct type t = A.t end;;
(* OK (t = int) *)
module rec A : sig type t = B.t end = struct type t = B.t end
       and B : sig type t = int end = struct type t = int end;;
(* Bad (t = int * t) *)
module rec A : sig type t = int * A.t end = struct type t = int * A.t end;;
(* Bad (t = t -> int) *)
module rec A : sig type t = B.t -> int end = struct type t = B.t -> int end
       and B : sig type t = A.t end = struct type t = A.t end;;
(* OK (t = <m:t>) *)
module rec A : sig type t = <m:B.t> end = struct type t = <m:B.t> end
       and B : sig type t = A.t end = struct type t = A.t end;;
(* Bad (not regular) *)
module rec A : sig type 'a t = <m: 'a list A.t> end
             = struct type 'a t = <m: 'a list A.t> end;;
(* Bad (not regular) *)
module rec A : sig type 'a t = <m: 'a list B.t; n: 'a array B.t> end
             = struct type 'a t = <m: 'a list B.t; n: 'a array B.t> end
       and B : sig type 'a t = 'a A.t end = struct type 'a t = 'a A.t end;;
(* Bad (not regular) *)
module rec A : sig type 'a t = 'a B.t end
             = struct type 'a t = 'a B.t end
       and B : sig type 'a t = <m: 'a list A.t; n: 'a array A.t> end
             = struct type 'a t = <m: 'a list A.t; n: 'a array A.t> end;;
(* OK *)
module rec A : sig type 'a t = 'a array B.t * 'a list B.t end
             = struct type 'a t = 'a array B.t * 'a list B.t end
       and B : sig type 'a t = <m: 'a B.t> end
             = struct type 'a t = <m: 'a B.t> end;;
(* Bad (not regular) *)
module rec A : sig type 'a t = 'a list B.t end
             = struct type 'a t = 'a list B.t end
       and B : sig type 'a t = <m: 'a array B.t> end
             = struct type 'a t = <m: 'a array B.t> end;;
(* Bad (not regular) *)
module rec M :
    sig
      class ['a] c : 'a -> object
        method map : ('a -> 'b) -> 'b M.c
      end
    end
  = struct
      class ['a] c (x : 'a) = object
        method map : 'b. ('a -> 'b) -> 'b M.c
          = fun f -> new M.c (f x)
      end
    end;;
(* OK *)
class type [ 'node ] extension = object method node : 'node end
and [ 'ext ] node = object constraint 'ext = 'ext node #extension [@id] end
class x = object method node : x node = assert false end
type t = x node;;
(* Bad - PR 4261 *)

module PR_4261 = struct
  module type S =
  sig
    type t
  end

  module type T =
  sig
    module D : S
    type t = D.t
  end

  module rec U : T with module D = U' = U
  and U' : S with type t = U'.t = U
end;;
(* Bad - PR 4512 *)
module type S' = sig type t = int end
module rec M : S' with type t = M.t = struct type t = M.t end;;
(* PR#4450 *)

module PR_4450_1 = struct
  module type MyT = sig type 'a t = Succ of 'a t end
  module MyMap(X : MyT) = X
  module rec MyList : MyT = MyMap(MyList)
end;;

module PR_4450_2 = struct
  module type MyT = sig
    type 'a wrap = My of 'a t
    and 'a t = private < map : 'b. ('a -> 'b) ->'b wrap; .. >
    val create : 'a list -> 'a t
  end
  module MyMap(X : MyT) = struct
    include X
    class ['a] c l = object (self)
      method map : 'b. ('a -> 'b) -> 'b wrap =
        fun f -> My (create (List.map f l))
    end
  end
  module rec MyList : sig
    type 'a wrap = My of 'a t
    and 'a t = < map : 'b. ('a -> 'b) ->'b wrap >
    val create : 'a list -> 'a t
  end = struct
    include MyMap(MyList)
    let create l = new c l
  end
end;;
(* A synthetic example of bootstrapped data structure
   (suggested by J-C Filliatre) *)

module type ORD = sig
  type t
  val compare : t -> t -> int
end

module type SET = sig
  type elt
  type t
  val iter : (elt -> unit) -> t -> unit
end

type 'a tree = E | N of 'a tree * 'a * 'a tree

module Bootstrap2
  (MakeDiet : functor (X: ORD) -> SET with type t = X.t tree and type elt = X.t)
  : SET with type elt = int =
struct

  type elt = int

  module rec Elt : sig
    type t = I of int * int | D of int * Diet.t * int
    val compare : t -> t -> int
    val iter : (int -> unit) -> t -> unit
  end =
  struct
    type t = I of int * int | D of int * Diet.t * int
    let compare x1 x2 = 0
    let rec iter f = function
      | I (l, r) -> for i = l to r do f i done
      | D (_, d, _) -> Diet.iter (iter f) d
  end

  and Diet : SET with type t = Elt.t tree and type elt = Elt.t = MakeDiet(Elt)

  type t = Diet.t
  let iter f = Diet.iter (Elt.iter f)
end
(* PR 4470: simplified from OMake's sources *)

module rec DirElt
  : sig
      type t = DirRoot | DirSub of DirHash.t
    end
  = struct
      type t = DirRoot | DirSub of DirHash.t
    end

and DirCompare
  : sig
      type t = DirElt.t
    end
  = struct
      type t = DirElt.t
    end

and DirHash
  : sig
      type t = DirElt.t list
    end
  = struct
      type t = DirCompare.t list
    end
(* PR 4758, PR 4266 *)

module PR_4758 = struct
  module type S = sig end
  module type Mod = sig
    module Other : S
  end
  module rec A : S = struct end
  and C : sig include Mod with module Other = A end = struct
    module Other = A
  end
  module C' = C  (* check that we can take an alias *)
  module F(X:sig end) = struct type t end
  let f (x : F(C).t) = (x : F(C').t)
end
(* PR 4557 *)
module PR_4557 = struct
  module F ( X : Set.OrderedType ) = struct
    module rec Mod : sig
      module XSet :
        sig
          type elt = X.t
          type t = Set.Make( X ).t
        end
      module XMap :
        sig
          type key = X.t
          type 'a t = 'a Map.Make(X).t
        end
      type elt = X.t
      type t = XSet.t XMap.t
      val compare: t -> t -> int
    end
       =
    struct
      module XSet = Set.Make( X )
      module XMap = Map.Make( X )

      type elt = X.t
      type t = XSet.t XMap.t
      let compare = (fun x y -> 0)
    end
    and ModSet : Set.S with type elt = Mod.t = Set.Make( Mod )
  end
end
module F ( X : Set.OrderedType ) = struct
  module rec Mod : sig
    module XSet :
      sig
        type elt = X.t
        type t = Set.Make( X ).t
      end
    module XMap :
      sig
        type key = X.t
        type 'a t = 'a Map.Make(X).t
      end
    type elt = X.t
    type t = XSet.t XMap.t
    val compare: t -> t -> int
  end
     =
  struct
    module XSet = Set.Make( X )
    module XMap = Map.Make( X )

    type elt = X.t
    type t = XSet.t XMap.t
    let compare = (fun x y -> 0)
  end
  and ModSet : Set.S with type elt = Mod.t = Set.Make( Mod )
end
(* Tests for recursive modules *)

let test number result expected =
  if result = expected
  then Printf.printf "Test %d passed.\n" number
  else Printf.printf "Test %d FAILED.\n" number;
  flush stdout

(* Tree of sets *)

module rec A
 : sig
     type t = Leaf of int | Node of ASet.t
     val compare: t -> t -> int
   end
 = struct
     type t = Leaf of int | Node of ASet.t
     let compare x y =
       match (x,y) with
         (Leaf i, Leaf j) -> Pervasives.compare i j
       | (Leaf i, Node t) -> -1
       | (Node s, Leaf j) -> 1
       | (Node s, Node t) -> ASet.compare s t
   end

and ASet : Set.S with type elt = A.t = Set.Make(A)
;;

let _ =
  let x = A.Node (ASet.add (A.Leaf 3) (ASet.singleton (A.Leaf 2))) in
  let y = A.Node (ASet.add (A.Leaf 1) (ASet.singleton x)) in
  test 10 (A.compare x x) 0;
  test 11 (A.compare x (A.Leaf 3)) 1;
  test 12 (A.compare (A.Leaf 0) x) (-1);
  test 13 (A.compare y y) 0;
  test 14 (A.compare x y) 1
;;

(* Simple value recursion *)

module rec Fib
  : sig val f : int -> int end
  = struct let f x = if x < 2 then 1 else Fib.f(x-1) + Fib.f(x-2) end
;;

let _ =
  test 20 (Fib.f 10) 89
;;

(* Update function by infix *)

module rec Fib2
  : sig val f : int -> int end
  = struct let rec g x = Fib2.f(x-1) + Fib2.f(x-2)
               and f x = if x < 2 then 1 else g x
    end
;;

let _ =
  test 21 (Fib2.f 10) 89
;;

(* Early application *)

let _ =
  let res =
    try
      let module A =
        struct
          module rec Bad
            : sig val f : int -> int end
            = struct let f = let y = Bad.f 5 in fun x -> x+y end
          end in
      false
    with Undefined_recursive_module _ ->
      true in
  test 30 res true
;;

(* Early strict evaluation *)

(*
module rec Cyclic
  : sig val x : int end
  = struct let x = Cyclic.x + 1 end
;;
*)

(* Reordering of evaluation based on dependencies *)

module rec After
  : sig val x : int end
  = struct let x = Before.x + 1 end
and Before
  : sig val x : int end
  = struct let x = 3 end
;;

let _ =
  test 40 After.x 4
;;

(* Type identity between A.t and t within A's definition *)

module rec Strengthen
  : sig type t val f : t -> t end
  = struct
      type t = A | B
      let _ = (A : Strengthen.t)
      let f x = if true then A else Strengthen.f B
    end
;;

module rec Strengthen2
  : sig type t
        val f : t -> t
        module M : sig type u end
        module R : sig type v end
    end
  = struct
      type t = A | B
      let _ = (A : Strengthen2.t)
      let f x = if true then A else Strengthen2.f B
      module M =
        struct
          type u = C
          let _ = (C: Strengthen2.M.u)
        end
      module rec R : sig type v  = Strengthen2.R.v end =
        struct
          type v = D
          let _ = (D : R.v)
          let _ = (D : Strengthen2.R.v)
        end
    end
;;

(* Polymorphic recursion *)

module rec PolyRec
  : sig
      type 'a t = Leaf of 'a | Node of 'a list t * 'a list t
      val depth: 'a t -> int
    end
  = struct
      type 'a t = Leaf of 'a | Node of 'a list t * 'a list t
      let x = (PolyRec.Leaf 1 : int t)
      let depth = function
        Leaf x -> 0
      | Node(l,r) -> 1 + max (PolyRec.depth l) (PolyRec.depth r)
    end
;;

(* Wrong LHS signatures (PR#4336) *)

(*
module type ASig = sig type a val a:a val print:a -> unit end
module type BSig = sig type b val b:b val print:b -> unit end

module A = struct type a = int let a = 0 let print = print_int end
module B = struct type b = float let b = 0.0 let print = print_float end

module MakeA (Empty:sig end) : ASig = A
module MakeB (Empty:sig end) : BSig = B

module
   rec NewA : ASig = MakeA (struct end)
   and NewB : BSig with type b = NewA.a = MakeB (struct end);;

*)

(* Expressions and bindings *)

module StringSet = Set.Make(String);;

module rec Expr
  : sig
      type t =
        Var of string
      | Const of int
      | Add of t * t
      | Binding of Binding.t * t
      val make_let: string -> t -> t -> t
      val fv: t -> StringSet.t
      val simpl: t -> t
    end
  = struct
      type t =
        Var of string
      | Const of int
      | Add of t * t
      | Binding of Binding.t * t
      let make_let id e1 e2 = Binding([id, e1], e2)
      let rec fv = function
        Var s -> StringSet.singleton s
      | Const n -> StringSet.empty
      | Add(t1,t2) -> StringSet.union (fv t1) (fv t2)
      | Binding(b,t) ->
          StringSet.union (Binding.fv b)
            (StringSet.diff (fv t) (Binding.bv b))
      let rec simpl = function
        Var s -> Var s
      | Const n -> Const n
      | Add(Const i, Const j) -> Const (i+j)
      | Add(Const 0, t) -> simpl t
      | Add(t, Const 0) -> simpl t
      | Add(t1,t2) -> Add(simpl t1, simpl t2)
      | Binding(b, t) -> Binding(Binding.simpl b, simpl t)
    end

and Binding
  : sig
      type t = (string * Expr.t) list
      val fv: t -> StringSet.t
      val bv: t -> StringSet.t
      val simpl: t -> t
    end
  = struct
      type t = (string * Expr.t) list
      let fv b =
        List.fold_left (fun v (id,e) -> StringSet.union v (Expr.fv e))
                       StringSet.empty b
      let bv b =
        List.fold_left (fun v (id,e) -> StringSet.add id v)
                       StringSet.empty b
      let simpl b =
        List.map (fun (id,e) -> (id, Expr.simpl e)) b
    end
;;

let _ =
  let e = Expr.make_let "x" (Expr.Add (Expr.Var "y", Expr.Const 0))
                            (Expr.Var "x") in
  let e' = Expr.make_let "x" (Expr.Var "y") (Expr.Var "x") in
  test 50 (StringSet.elements (Expr.fv e)) ["y"];
  test 51 (Expr.simpl e) e'
;;

(* Okasaki's bootstrapping *)

module type ORDERED =
  sig
    type t
    val eq: t -> t -> bool
    val lt: t -> t -> bool
    val leq: t -> t -> bool
  end

module type HEAP =
  sig
    module Elem: ORDERED
    type heap
    val empty: heap
    val isEmpty: heap -> bool
    val insert: Elem.t -> heap -> heap
    val merge: heap -> heap -> heap
    val findMin: heap -> Elem.t
    val deleteMin: heap -> heap
  end

module Bootstrap (MakeH: functor (Element:ORDERED) ->
                                    HEAP with module Elem = Element)
                 (Element: ORDERED) : HEAP with module Elem = Element =
  struct
    module Elem = Element
    module rec BE
    : sig type t = E | H of Elem.t * PrimH.heap
          val eq: t -> t -> bool
          val lt: t -> t -> bool
          val leq: t -> t -> bool
      end
    = struct
        type t = E | H of Elem.t * PrimH.heap
        let leq t1 t2 =
          match t1, t2 with
          | (H(x, _)), (H(y, _)) -> Elem.leq x y
          | H _, E -> false
          | E, H _ -> true
          | E, E -> true
        let eq t1 t2 =
          match t1, t2 with
          | (H(x, _)), (H(y, _)) -> Elem.eq x y
          | H _, E -> false
          | E, H _ -> false
          | E, E -> true
        let lt t1 t2 =
          match t1, t2 with
          | (H(x, _)), (H(y, _)) -> Elem.lt x y
          | H _, E -> false
          | E, H _ -> true
          | E, E -> false
      end
    and PrimH
    : HEAP with type Elem.t = BE.t
    = MakeH(BE)
    type heap = BE.t
    let empty = BE.E
    let isEmpty = function BE.E -> true | _ -> false
    let rec merge x y =
      match (x,y) with
        (BE.E, _) -> y
      | (_, BE.E) -> x
      | (BE.H(e1,p1) as h1), (BE.H(e2,p2) as h2) ->
          if Elem.leq e1 e2
          then BE.H(e1, PrimH.insert h2 p1)
          else BE.H(e2, PrimH.insert h1 p2)
    let insert x h =
      merge (BE.H(x, PrimH.empty)) h
    let findMin = function
        BE.E -> raise Not_found
      | BE.H(x, _) -> x
    let deleteMin = function
        BE.E -> raise Not_found
      | BE.H(x, p) ->
          if PrimH.isEmpty p then BE.E else begin
            match PrimH.findMin p with
            | (BE.H(y, p1)) ->
              let p2 = PrimH.deleteMin p in
              BE.H(y, PrimH.merge p1 p2)
            | BE.E -> assert false
          end
  end
;;

module LeftistHeap(Element: ORDERED): HEAP with module Elem = Element =
  struct
    module Elem = Element
    type heap = E | T of int * Elem.t * heap * heap
    let rank = function E -> 0 | T(r,_,_,_) -> r
    let make x a b =
      if rank a >= rank b
      then T(rank b + 1, x, a, b)
      else T(rank a + 1, x, b, a)
    let empty = E
    let isEmpty = function E -> true | _ -> false
    let rec merge h1 h2 =
      match (h1, h2) with
        (_, E) -> h1
      | (E, _) -> h2
      | (T(_, x1, a1, b1), T(_, x2, a2, b2)) ->
          if Elem.leq x1 x2
          then make x1 a1 (merge b1 h2)
          else make x2 a2 (merge h1 b2)
    let insert x h = merge (T(1, x, E, E)) h
    let findMin = function
      E -> raise Not_found
    | T(_, x, _, _) -> x
    let deleteMin = function
      E -> raise Not_found
    | T(_, x, a, b) -> merge a b
  end
;;

module Ints =
  struct
    type t = int
    let eq = (=)
    let lt = (<)
    let leq = (<=)
  end
;;

module C = Bootstrap(LeftistHeap)(Ints);;

let _ =
  let h = List.fold_right C.insert [6;4;8;7;3;1] C.empty in
  test 60 (C.findMin h) 1;
  test 61 (C.findMin (C.deleteMin h)) 3;
  test 62 (C.findMin (C.deleteMin (C.deleteMin h))) 4
;;

(* Classes *)

module rec Class1
  : sig
      class c : object method m : int -> int end
    end
  = struct
      class c =
        object
          method m x = if x <= 0 then x else (new Class2.d)#m x
        end
    end
and Class2
  : sig
      class d : object method m : int -> int end
    end
  = struct
      class d =
        object(self)
          inherit Class1.c as super
          method m (x:int) = super#m 0
        end
    end
;;

let _ =
  test 70 ((new Class1.c)#m 7) 0
;;

let _ =
  try
    let module A = struct
       module rec BadClass1
         : sig
             class c : object method m : int end
           end
         = struct
             class c = object method m = 123 end
           end
       and BadClass2
         : sig
             val x: int
           end
         = struct
             let x = (new BadClass1.c)#m
           end
    end in
      test 71 true false
  with Undefined_recursive_module _ ->
    test 71 true true
;;

(* Coercions *)

module rec Coerce1
  : sig
      val g: int -> int
      val f: int -> int
    end
  = struct
      module A = (Coerce1: sig val f: int -> int end)
      let g x = x
      let f x = if x <= 0 then 1 else A.f (x-1) * x
    end
;;

let _ =
  test 80 (Coerce1.f 10) 3628800
;;

module CoerceF(S: sig end) = struct
  let f1 () = 1
  let f2 () = 2
  let f3 () = 3
  let f4 () = 4
  let f5 () = 5
end

module rec Coerce2: sig val f1: unit -> int end = CoerceF(Coerce3)
       and Coerce3: sig end = struct end
;;

let _ =
  test 81 (Coerce2.f1 ()) 1
;;

module Coerce4(A : sig val f : int -> int end) = struct
  let x = 0
  let at a = A.f a
end

module rec Coerce5
  : sig val blabla: int -> int val f: int -> int end
  = struct let blabla x = 0 let f x = 5 end
and Coerce6
  : sig val at: int -> int end
  = Coerce4(Coerce5)

let _ =
  test 82 (Coerce6.at 100) 5
;;

(* Miscellaneous bug reports *)

module rec F
  : sig type t = X of int | Y of int
        val f: t -> bool
    end
  = struct
      type t = X of int | Y of int
      let f = function
        | X _ -> false
        | _ -> true
    end;;

let _ =
  test 100 (F.f (F.X 1)) false;
  test 101 (F.f (F.Y 2)) true

(* PR#4316 *)
module G(S : sig val x : int Lazy.t end) = struct include S end

module M1 = struct let x = lazy 3 end

let _ = Lazy.force M1.x

module rec M2 : sig val x : int Lazy.t end = G(M1)

let _ =
  test 102 (Lazy.force M2.x) 3

let _ = Gc.full_major()   (* will shortcut forwarding in M1.x *)

module rec M3 : sig val x : int Lazy.t end = G(M1)

let _ =
  test 103 (Lazy.force M3.x) 3


(** Pure type-checking tests: see recmod/*.ml  *)
type t = A of {x:int; mutable y:int};;
let f (A r) = r;;  (* -> escape *)
let f (A r) = r.x;; (* ok *)
let f x = A {x; y = x};; (* ok *)
let f (A r) = A {r with y = r.x + 1};; (* ok *)
let f () = A {a = 1};; (* customized error message *)
let f () = A {x = 1; y = 3};; (* ok *)

type _ t = A: {x : 'a; y : 'b} -> 'a t;;
let f (A {x; y}) = A {x; y = ()};;  (* ok *)
let f (A ({x; y} as r)) = A {x = r.x; y = r.y};; (* ok *)

module M = struct
  type 'a t =
    | A of {x : 'a}
    | B: {u : 'b} -> unit t;;

  exception Foo of {x : int};;
end;;

module N : sig
  type 'b t = 'b M.t =
    | A of {x : 'b}
    | B: {u : 'bla} -> unit t

  exception Foo of {x : int}
end = struct
  type 'b t = 'b M.t =
    | A of {x : 'b}
    | B: {u : 'z} -> unit t

  exception Foo = M.Foo
end;;


module type S = sig exception A of {x:int}  end;;

module F (X : sig val x : (module S) end) = struct
  module A = (val X.x)
end;;  (* -> this expression creates fresh types (not really!) *)


module type S = sig
  exception A of {x : int}
  exception A of {x : string}
end;;

module M = struct
  exception A of {x : int}
  exception A of {x : string}
end;;


module M1 = struct
  exception A of {x : int}
end;;

module M = struct
  include M1
  include M1
end;;


module type S1 = sig
  exception A of {x : int}
end;;

module type S = sig
  include S1
  include S1
end;;

module M = struct
  exception A = M1.A
end;;

module X1 = struct
  type t = ..
end;;
module X2 = struct
  type t = ..
end;;
module Z = struct
  type X1.t += A of {x: int}
  type X2.t += A of {x: int}
end;;

(* PR#6716 *)

type _ c = C : [`A] c
type t = T : {x:[<`A] c} -> t;;
let f (T { x = C }) = ();;
module M : sig
  type 'a t
  type u = u t and v = v t
  val f : int -> u
  val g : v -> bool
end = struct
  type 'a t = 'a
  type u = int and v = bool
  let f x = x
  let g x = x
end;;

let h (x : int) : bool = M.g (M.f x);;
type _ t = C : ((('a -> 'o) -> 'o) -> ('b -> 'o) -> 'o) t
let f : type a o. ((a -> o) -> o) t -> (a -> o) -> o =
 fun C k -> k (fun x -> x);;
module type T = sig type 'a t end
module Fix (T : T) = struct type r = ('r T.t as 'r) end
 type _ t =
     X of string
   | Y : bytes t

let y : string t = Y
let f : string A.t -> unit = function
    A.X s -> print_endline s

let () = f A.y
module rec A : sig
 type t
end = struct
 type t = { a : unit; b : unit }
 let _ = { a = () }
end
;;
type t = [`A | `B];;
type 'a u = t;;
let a : [< int u] = `A;;

type 'a s = 'a;;
let b : [< t s] = `B;;
module Core = struct
  module Int = struct
    module T = struct
      type t = int
      let compare = compare
      let (+) x y = x + y
    end
    include T
    module Map = Map.Make(T)
  end

  module Std = struct
    module Int = Int
  end
end
;;

open Core.Std
;;

let x = Int.Map.empty ;;
let y = x + x ;;

(* Avoid ambiguity *)

module M = struct type t = A type u = C end
module N = struct type t = B end
open M open N;;
A;;
B;;
C;;

include M open M;;
C;;

module L = struct type v = V end
open L;;
V;;
module L = struct type v = V end
open L;;
V;;


type t1 = A;;
module M1 = struct type u = v and v = t1 end;;
module N1 = struct type u = v and v = M1.v end;;
type t1 = B;;
module N2 = struct type u = v and v = M1.v end;;


(* PR#6566 *)
module type PR6566 = sig type t = string end;;
module PR6566 = struct type t = int end;;
module PR6566' : PR6566 = PR6566;;

module A = struct module B = struct type t = T end end;;
module M2 = struct type u = A.B.t type foo = int type v = A.B.t end;;
(* Adapted from: An Expressive Language of Signatures
   by Norman Ramsey, Kathleen Fisher and Paul Govereau *)

module type VALUE = sig
  type value (* a Lua value *)
  type state (* the state of a Lua interpreter *)
  type usert (* a user-defined value *)
end;;

module type CORE0 = sig
  module V : VALUE
  val setglobal : V.state -> string -> V.value -> unit
  (* five more functions common to core and evaluator *)
end;;

module type CORE = sig
  include CORE0
  val apply : V.value -> V.state -> V.value list -> V.value
  (* apply function f in state s to list of args *)
end;;

module type AST = sig
  module Value : VALUE
  type chunk
  type program
  val get_value : chunk -> Value.value
end;;

module type EVALUATOR = sig
  module Value : VALUE
  module Ast : (AST with module Value := Value)
  type state = Value.state
  type value = Value.value
  exception Error of string
  val compile : Ast.program -> string
  include CORE0 with module V := Value
end;;

module type PARSER = sig
  type chunk
  val parse : string -> chunk
end;;

module type INTERP = sig
  include EVALUATOR
  module Parser : PARSER with type chunk = Ast.chunk
  val dostring : state -> string -> value list
  val mk : unit -> state
end;;

module type USERTYPE = sig
  type t
  val eq : t -> t -> bool
  val to_string : t -> string
end;;

module type TYPEVIEW = sig
  type combined
  type t
  val map : (combined -> t) * (t -> combined)
end;;

module type COMBINED_COMMON = sig
  module T : sig type t end
  module TV1 : TYPEVIEW with type combined := T.t
  module TV2 : TYPEVIEW with type combined := T.t
end;;

module type COMBINED_TYPE = sig
  module T : USERTYPE
  include COMBINED_COMMON with module T := T
end;;

module type BARECODE = sig
  type state
  val init : state -> unit
end;;

module USERCODE(X : TYPEVIEW) = struct
  module type F =
      functor (C : CORE with type V.usert = X.combined) ->
        BARECODE with type state := C.V.state
end;;

module Weapon = struct type t end;;

module type WEAPON_LIB = sig
  type t = Weapon.t
  module T : USERTYPE with type t = t
  module Make :
    functor (TV : TYPEVIEW with type t = t) -> USERCODE(TV).F
end;;

module type X = functor (X: CORE) -> BARECODE;;
module type X = functor (_: CORE) -> BARECODE;;
module M = struct
  type t = int * (< m : 'a > as 'a)
end;;

module type S =
    sig module M : sig type t end end with module M = M
;;
module type Printable = sig
  type t
  val print : Format.formatter -> t -> unit
end;;
module type Comparable = sig
  type t
  val compare : t -> t -> int
end;;
module type PrintableComparable = sig
  include Printable
  include Comparable with type t = t
end;; (* Fails *)
module type PrintableComparable = sig
  type t
  include Printable with type t := t
  include Comparable with type t := t
end;;
module type PrintableComparable = sig
  include Printable
  include Comparable with type t := t
end;;
module type ComparableInt = Comparable with type t := int;;
module type S = sig type t val f : t -> t end;;
module type S' = S with type t := int;;

module type S = sig type 'a t val map : ('a -> 'b) -> 'a t -> 'b t end;;
module type S1 = S with type 'a t := 'a list;;
module type S2 = sig
  type 'a dict = (string * 'a) list
  include S with type 'a t := 'a dict
end;;


module type S =
  sig module T : sig type exp type arg end val f : T.exp -> T.arg end;;
module M = struct type exp = string type arg = int end;;
module type S' = S with module T := M;;


module type S = sig type 'a t end with type 'a t := unit;; (* Fails *)
let property (type t) () =
  let module M = struct exception E of t end in
  (fun x -> M.E x), (function M.E x -> Some x | _ -> None)
;;

let () =
  let (int_inj, int_proj) = property () in
  let (string_inj, string_proj) = property () in

  let i = int_inj 3 in
  let s = string_inj "abc" in

  Printf.printf "%B\n%!" (int_proj i = None);
  Printf.printf "%B\n%!" (int_proj s = None);
  Printf.printf "%B\n%!" (string_proj i = None);
  Printf.printf "%B\n%!" (string_proj s = None)
;;

let sort_uniq (type s) cmp l =
  let module S = Set.Make(struct type t = s let compare = cmp end) in
  S.elements (List.fold_right S.add l S.empty)
;;

let () =
  print_endline (String.concat "," (sort_uniq compare [ "abc"; "xyz"; "abc" ]))
;;

let f x (type a) (y : a) = (x = y);; (* Fails *)
class ['a] c = object (self)
  method m : 'a -> 'a = fun x -> x
  method n : 'a -> 'a = fun (type g) (x:g) -> self#m x
end;; (* Fails *)

external a : (int [@untagged]) -> unit = "a" "a_nat"
external b : (int32 [@unboxed]) -> unit = "b" "b_nat"
external c : (int64 [@unboxed]) -> unit = "c" "c_nat"
external d : (nativeint [@unboxed]) -> unit = "d" "d_nat"
external e : (float [@unboxed]) -> unit = "e" "e_nat"

type t = private int

external f : (t [@untagged]) -> unit = "f" "f_nat"

module M : sig
  external a : int -> (int [@untagged]) = "a" "a_nat"
  external b : (int [@untagged]) -> int = "b" "b_nat"
end = struct
  external a : int -> (int [@untagged]) = "a" "a_nat"
  external b : (int [@untagged]) -> int = "b" "b_nat"
end;;

module Global_attributes = struct
  [@@@ocaml.warning "-3"]

  external a : float -> float = "a" "noalloc" "a_nat" "float"
  external b : float -> float = "b" "noalloc" "b_nat"
  external c : float -> float = "c" "c_nat" "float"
  external d : float -> float = "d" "noalloc"
  external e : float -> float = "e"

  (* Should output a warning: no native implementation provided *)
  external f : (int32 [@unboxed]) -> (int32 [@unboxed]) = "f" "noalloc"
  external g : int32 -> int32 = "g" "g_nat" [@@unboxed] [@@noalloc]

  external h : (int [@untagged]) -> (int [@untagged]) = "h" "h_nat" "noalloc"
  external i : int -> int = "i" "i_nat" [@@untagged] [@@noalloc]
end;;

module Old_style_warning = struct
  [@@@ocaml.warning "+3"]
  external a : float -> float = "a" "noalloc" "a_nat" "float"
  external b : float -> float = "b" "noalloc" "b_nat"
  external c : float -> float = "c" "c_nat" "float"
  external d : float -> float = "d" "noalloc"
  external e : float -> float = "c" "float"
end

(* Bad: attributes not reported in the interface *)

module Bad1 : sig
  external f : int -> int = "f" "f_nat"
end = struct
  external f : int -> (int [@untagged]) = "f" "f_nat"
end;;

module Bad2 : sig
  external f : int -> int = "a" "a_nat"
end = struct
  external f : (int [@untagged]) -> int = "f" "f_nat"
end;;

module Bad3 : sig
  external f : float -> float = "f" "f_nat"
end = struct
  external f : float -> (float [@unboxed]) = "f" "f_nat"
end;;

module Bad4 : sig
  external f : float -> float = "a" "a_nat"
end = struct
  external f : (float [@unboxed]) -> float = "f" "f_nat"
end;;

(* Bad: attributes in the interface but not in the implementation *)

module Bad5 : sig
  external f : int -> (int [@untagged]) = "f" "f_nat"
end = struct
  external f : int -> int = "f" "f_nat"
end;;

module Bad6 : sig
  external f : (int [@untagged]) -> int = "f" "f_nat"
end = struct
  external f : int -> int = "a" "a_nat"
end;;

module Bad7 : sig
  external f : float -> (float [@unboxed]) = "f" "f_nat"
end = struct
  external f : float -> float = "f" "f_nat"
end;;

module Bad8 : sig
  external f : (float [@unboxed]) -> float = "f" "f_nat"
end = struct
  external f : float -> float = "a" "a_nat"
end;;

(* Bad: unboxed or untagged with the wrong type *)

external g : (float [@untagged]) -> float = "g" "g_nat";;
external h : (int [@unboxed]) -> float = "h" "h_nat";;

(* Bad: unboxing the function type *)
external i : int -> float [@unboxed] = "i" "i_nat";;

(* Bad: unboxing a "deep" sub-type. *)
external j : int -> (float [@unboxed]) * float = "j" "j_nat";;

(* This should be rejected, but it is quite complicated to do
   in the current state of things *)

external k : int -> (float [@unboxd]) = "k" "k_nat";;

(* Bad: old style annotations + new style attributes *)

external l : float -> float = "l" "l_nat" "float" [@@unboxed];;
external m : (float [@unboxed]) -> float = "m" "m_nat" "float";;
external n : float -> float = "n" "noalloc" [@@noalloc];;

(* Warnings: unboxed / untagged without any native implementation *)
external o : (float[@unboxed]) -> float = "o";;
external p : float -> (float[@unboxed]) = "p";;
external q : (int[@untagged]) -> float = "q";;
external r : int -> (int[@untagged]) = "r";;
external s : int -> int = "s" [@@untagged];;
external t : float -> float = "t" [@@unboxed];;
let _ = ignore (+);;
let _ = raise Exit 3;;
(* comment 9644 of PR#6000 *)

fun b -> if b then format_of_string "x" else "y";;
fun b -> if b then "x" else format_of_string "y";;
fun b : (_,_,_) format -> if b then "x" else "y";;

(* PR#7135 *)

module PR7135 = struct
  module M : sig type t = private int end =  struct type t = int end
  include M

  let lift2 (f : int -> int -> int) (x : t) (y : t) =
    f (x :> int) (y :> int)
end;;

(* exemple of non-ground coercion *)

module Test1 = struct
  type t = private int
  let f x = let y = if true then x else (x:t) in (y :> int)
end;;
(* Warn about all relevant cases when possible *)
let f = function
    None, None -> 1
  | Some _, Some _ -> 2;;

(* Exhaustiveness check is very slow *)
type _ t =
  A : int t | B : bool t | C : char t | D : float t
type (_,_,_,_) u = U : (int, int, int, int) u
type v = E | F | G
;;

let f : type a b c d e f g.
      a t * b t * c t * d t * e t * f t * g t * v
       * (a,b,c,d) u * (e,f,g,g) u -> int =
 function A, A, A, A, A, A, A, _, U, U -> 1
   | _, _, _, _, _, _, _, G, _, _ -> 1
   (*| _ -> _ *)
;;

(* Unused cases *)
let f (x : int t) = match x with A -> 1 | _ -> 2;; (* warn *)
let f (x : unit t option) = match x with None -> 1 | _ -> 2 ;; (* warn? *)
let f (x : unit t option) = match x with None -> 1 | Some _ -> 2 ;; (* warn *)
let f (x : int t option) = match x with None -> 1 | _ -> 2;;
let f (x : int t option) = match x with None -> 1;; (* warn *)

(* Example with record, type, single case *)

type 'a box = Box of 'a
type 'a pair = {left: 'a; right: 'a};;

let f : (int t box pair * bool) option -> unit = function None -> ();;
let f : (string t box pair * bool) option -> unit = function None -> ();;


(* Examples from ML2015 paper *)

type _ t =
  | Int : int t
  | Bool : bool t
;;

let f : type a. a t -> a = function
  | Int -> 1
  | Bool -> true
;;
let g : int t -> int = function
  | Int -> 1
;;
let h : type a. a t -> a t -> bool =
  fun x y -> match x, y with
  | Int, Int -> true
  | Bool, Bool -> true
;;
type (_, _) cmp =
 | Eq : ('a, 'a) cmp
 | Any: ('a, 'b) cmp
module A : sig type a type b val eq : (a, b) cmp end
  = struct type a type b = a let eq = Eq end
;;
let f : (A.a, A.b) cmp -> unit = function Any -> ()
;;
let deep : char t option -> char =
  function None -> 'c'
;;
type zero = Zero
type _ succ = Succ
;;
type (_,_,_) plus =
  | Plus0 : (zero, 'a, 'a) plus
  | PlusS : ('a, 'b, 'c) plus ->
       ('a succ, 'b, 'c succ) plus
;;
let trivial : (zero succ, zero, zero) plus option -> bool =
  function None -> false
;;
let easy : (zero, zero succ, zero) plus option -> bool =
  function None -> false
;;
let harder : (zero succ, zero succ, zero succ) plus option -> bool =
  function None -> false
;;
let harder : (zero succ, zero succ, zero succ) plus option  -> bool =
  function None -> false | Some (PlusS _) -> .
;;
let inv_zero : type a b c d. (a,b,c) plus -> (c,d,zero) plus -> bool =
  fun p1 p2 ->
    match p1, p2 with
    | Plus0, Plus0 -> true
;;


(* Empty match *)

type _ t = Int : int t;;
let f (x : bool t) = match x with _ -> . ;; (* ok *)


(* trefis in PR#6437 *)

let f () = match None with _ -> .;; (* error *)
let g () = match None with _ -> () | exception _ -> .;; (* error *)
let h () = match None with _ -> .  | exception _ -> .;; (* error *)
let f x = match x with _ -> () | None -> .;; (* do not warn *)

(* #7059, all clauses guarded *)

let f x y = match 1 with 1 when x = y -> 1;;
open CamlinternalOO;;
type _ choice = Left : label choice | Right : tag choice;;
let f : label choice -> bool = function Left -> true;; (* warn *)
exception A;;
type a = A;;

A;;
raise A;;
fun (A : a) -> ();;
function Not_found -> 1 | A -> 2 | _ -> 3;;
try raise A with A -> 2;;
module TypEq = struct
 type (_, _) t = Eq : ('a, 'a) t
end

module type T = sig
 type _ is_t = Is : ('a, 'b) TypEq.t -> 'a is_t
 val is_t : unit -> unit is_t option
end

module Make (M : T) =
 struct
   let _ =
     match M.is_t () with
     | None -> 0
     | Some _ -> 0
   let f () =
     match M.is_t () with None -> 0
end;;

module Make2 (M : T) = struct
  type t = T of unit M.is_t
  let g : t -> int = function _ -> .
end;;
type t = A : t;;

module X1 : sig end = struct
  let _f ~x (* x unused argument *) = function
    | A -> let x = () in x
end;;

module X2 : sig end = struct
  let x = 42 (* unused value *)
  let _f = function
    | A -> let x = () in x
end;;

module X3 : sig end = struct
  module O = struct let x = 42 (* unused *) end
  open O (* unused open *)

  let _f = function
    | A -> let x = () in x
end;;
(* Use type information *)
module M1 = struct
  type t = {x: int; y: int}
  type u = {x: bool; y: bool}
end;;

module OK = struct
  open M1
  let f1 (r:t) = r.x (* ok *)
  let f2 r = ignore (r:t); r.x (* non principal *)

  let f3 (r: t) =
    match r with {x; y} -> y + y (* ok *)
end;;

module F1 = struct
  open M1
  let f r = match r with {x; y} -> y + y
end;; (* fails *)

module F2 = struct
  open M1
  let f r =
    ignore (r: t);
    match r with
       {x; y} -> y + y
end;; (* fails for -principal *)

(* Use type information with modules*)
module M = struct
  type t = {x:int}
  type u = {x:bool}
end;;
let f (r:M.t) = r.M.x;; (* ok *)
let f (r:M.t) = r.x;; (* warning *)
let f ({x}:M.t) = x;; (* warning *)

module M = struct
  type t = {x: int; y: int}
end;;
module N = struct
  type u = {x: bool; y: bool}
end;;
module OK = struct
  open M
  open N
  let f (r:M.t) = r.x
end;;

module M = struct
  type t = {x:int}
  module N = struct type s = t = {x:int} end
  type u = {x:bool}
end;;
module OK = struct
  open M.N
  let f (r:M.t) = r.x
end;;

(* Use field information *)
module M = struct
  type u = {x:bool;y:int;z:char}
  type t = {x:int;y:bool}
end;;
module OK = struct
  open M
  let f {x;z} = x,z
end;; (* ok *)
module F3 = struct
  open M
  let r = {x=true;z='z'}
end;; (* fail for missing label *)

module OK = struct
  type u = {x:int;y:bool}
  type t = {x:bool;y:int;z:char}
  let r = {x=3; y=true}
end;; (* ok *)

(* Corner cases *)

module F4 = struct
  type foo = {x:int; y:int}
  type bar = {x:int}
  let b : bar = {x=3; y=4}
end;; (* fail but don't warn *)

module M = struct type foo = {x:int;y:int} end;;
module N = struct type bar = {x:int;y:int} end;;
let r = { M.x = 3; N.y = 4; };; (* error: different definitions *)

module MN = struct include M include N end
module NM = struct include N include M end;;
let r = {MN.x = 3; NM.y = 4};; (* error: type would change with order *)

(* Lpw25 *)

module M = struct
  type foo = { x: int; y: int }
  type bar = { x:int; y: int; z: int}
end;;
module F5 = struct
  open M
  let f r = ignore (r: foo); {r with x = 2; z = 3}
end;;
module M = struct
  include M
  type other = { a: int; b: int }
end;;
module F6 = struct
  open M
  let f r = ignore (r: foo); { r with x = 3; a = 4 }
end;;
module F7 = struct
  open M
  let r = {x=1; y=2}
  let r: other = {x=1; y=2}
end;;

module A = struct type t = {x: int} end
module B = struct type t = {x: int} end;;
let f (r : B.t) = r.A.x;; (* fail *)

(* Spellchecking *)

module F8 = struct
  type t = {x:int; yyy:int}
  let a : t = {x=1;yyz=2}
end;;

(* PR#6004 *)

type t = A
type s = A

class f (_ : t) = object end;;
class g = f A;; (* ok *)

class f (_ : 'a) (_ : 'a) = object end;;
class g = f (A : t) A;; (* warn with -principal *)


(* PR#5980 *)

module Shadow1 = struct
  type t = {x: int}
  module M = struct
    type s = {x: string}
  end
  open M  (* this open is unused, it isn't reported as shadowing 'x' *)
  let y : t = {x = 0}
end;;
module Shadow2 = struct
  type t = {x: int}
  module M = struct
    type s = {x: string}
  end
  open M  (* this open shadows label 'x' *)
  let y = {x = ""}
end;;

(* PR#6235 *)

module P6235 = struct
  type t = { loc : string; }
  type v = { loc : string; x : int; }
  type u = [ `Key of t ]
  let f (u : u) = match u with `Key {loc} -> loc
end;;

(* Remove interaction between branches *)

module P6235' = struct
  type t = { loc : string; }
  type v = { loc : string; x : int; }
  type u = [ `Key of t ]
  let f = function
    | (_ : u) when false -> ""
    |`Key {loc} -> loc
end;;
module Unused : sig
end = struct
  type unused = int
end
;;

module Unused_nonrec : sig
end = struct
  type nonrec used = int
  type nonrec unused = used
end
;;

module Unused_rec : sig
end = struct
  type unused = A of unused
end
;;

module Unused_exception : sig
end = struct
  exception Nobody_uses_me
end
;;

module Unused_extension_constructor : sig
  type t = ..
end = struct
  type t = ..
  type t += Nobody_uses_me
end
;;

module Unused_exception_outside_patterns : sig
  val falsity : exn -> bool
end = struct
  exception Nobody_constructs_me
  let falsity = function
    | Nobody_constructs_me -> true
    | _ -> false
end
;;

module Unused_extension_outside_patterns : sig
  type t = ..
  val falsity : t -> bool
end = struct
  type t = ..
  type t += Nobody_constructs_me
  let falsity = function
    | Nobody_constructs_me -> true
    | _ -> false
end
;;

module Unused_private_exception : sig
  type exn += private Private_exn
end = struct
  exception Private_exn
end
;;

module Unused_private_extension : sig
  type t = ..
  type t += private Private_ext
end = struct
  type t = ..
  type t += Private_ext
end
;;

for i = 10 downto 0 do () done

type t = < foo: int [@foo] >

let _ = [%foo: < foo : t > ]

type foo += private A of int

let f : 'a 'b 'c. < .. > = assert false

let () =
  let module M = (functor (T : sig end) -> struct end)(struct end) in ()

class c = object inherit ((fun () -> object end [@wee]: object end) ()) end


let f = function x[@wee] -> ()
let f = function
  | '1'..'9' | '1' .. '8'-> ()
  | 'a'..'z' -> ()

let f = function
  | [| x1; x2 |] -> ()
  | [| |] -> ()
  | [|x|][@foo] -> ()
  | _ -> ()

let g = function
  | {l=x} -> ()
  | {l1=x; l2=y}[@foo] -> ()
  | {l1=x; l2=y; _} -> ()

let h = fun ?l:(p=1) ?y:u ?x:(x=3) -> 2

let _ = function
  | a, s, ba1, ba2, ba3, bg -> begin
      ignore (Array.get x 1 + Array.get [| |] 0 +
              Array.get [| 1 |] 1 + Array.get [|1; 2|] 2);
      ignore ([String.get s 1; String.get "" 2; String.get "123" 3]);
      ignore (ba1.{0} + ba2.{1, 2} + ba3.{3, 4, 5})
      ignore (bg.{1, 2, 3, 4})
    end
  | b, s, ba1, ba2, ba3, bg -> begin
      y.(0) <- 1; s.[1] <- 'c';
      ba1.{1} <- 2; ba2.{1, 2} <- 3; ba3.{1, 2, 3} <- 4;
      bg.{1, 2, 3, 4, 5} <- 0
    end

let f (type t) () =
  let exception F of t in ();
  let exception G of t in ();
  let exception E of t in
  (fun x -> E x), (function E _ -> print_endline "OK" | _ -> print_endline "KO")

let inj1, proj1 = f ()
let inj2, proj2 = f ()

let () = proj1 (inj1 42)
let () = proj1 (inj2 42)

let _ = ~-1

class id = [%exp]
(* checkpoint *)

(* Subtyping is "syntactic" *)
let _ = fun (x : < x : int >) y z -> (y :> 'a), (x :> 'a), (z :> 'a);;
(* - : (< x : int > as 'a) -> 'a -> 'a * 'a = <fun> *)

class ['a] c () = object
  method f = (new c (): int c)
end and ['a] d () = object
  inherit ['a] c ()
end;;

(* PR#7329 Pattern open *)
let _ =
  let module M = struct type t = { x : int } end in
  let f M.(x) = () in
  let g M.{x} = () in
  let h = function M.[] | M.[a] | M.(a::q) -> () in
  let i = function M.[||] | M.[|x|]  -> true | _ -> false in
  ()

class ['a] c () = object
  constraint 'a = < .. > -> unit
  method m  = (fun x -> () : 'a)
end

let f: type a'.a' = assert false
let foo : type a' b'. a' -> b' = fun a -> assert false
let foo : type t' . t' = fun (type t') -> (assert false : t')
let foo : 't . 't = fun (type t) -> (assert false : t)
let foo : type a' b' c' t. a' -> b' -> c' -> t = fun a b c -> assert false

let f x =
  x.contents <- (print_string "coucou" ; x.contents)

let ( ~$ ) x = Some x
let g x =
  ~$ (x.contents)

let ( ~$ ) x y = (x, y)
let g x y =
  ~$ (x.contents) (y.contents)



(* PR#7506: attributes on list tail *)

let tail1 = ([1; 2])[@hello]
let tail2 = 0::(([1; 2])[@hello])
let tail3 = 0::(([])[@hello])

let f ~l:(l[@foo]) = l;;
let test x y = ((+)[@foo]) x y;;
let test x = ((~-)[@foo]) x;;
let test contents = { contents = contents[@foo] };;
class type t = object(_[@foo]) end;;
let test f x = f ~x:(x[@foo]);;
let f = function ((`A|`B)[@bar]) | `C -> ();;
let f = function _::(_::_ [@foo]) -> () | _ -> ();;
function {contents=contents[@foo]} -> ();;
fun contents -> {contents=contents[@foo]};;
((); (((); ())[@foo]));;

(* https://github.com/LexiFi/gen_js_api/issues/61 *)

let () = foo##.bar := ();;

(* "let open" in classes and class types *)

class c =
  let open M in
  object
    method f : t = x
  end
;;
class type ct =
  let open M in
  object
    method f : t
  end
;;

(* M.(::) notation *)
module Exotic_list = struct
  module Inner = struct
    type ('a,'b) t = [] | (::) of 'a * 'b *  ('a,'b) t
  end

  let Inner.(::)(x,y, Inner.[]) = Inner.(::)(1,"one",Inner.[])
end

(** Extended index operators *)
module Indexop = struct
  module Def = struct
    let ( .%[] ) = Hashtbl.find
    let ( .%[] <- ) = Hashtbl.add
    let ( .%() ) = Hashtbl.find
    let ( .%() <- ) = Hashtbl.add
    let ( .%{} ) = Hashtbl.find
    let ( .%{} <- ) = Hashtbl.add
  end
  ;;
  let h = Hashtbl.create 17 in
  h.Def.%["one"] <- 1;
  h.Def.%("two") <- 2;
  h.Def.%{"three"} <- 3
  let x,y,z = Def.(h.%["one"], h.%("two"), h.%{"three"})
end
