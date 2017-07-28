
(* By using two types we can have a recursive constraint *)
type 'a class_name = .. constraint 'a = < cast: 'a. 'a name -> 'a; ..>
and 'a name = Class : 'a class_name -> (< cast: 'a. 'a name -> 'a; ..> as 'a) name
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
