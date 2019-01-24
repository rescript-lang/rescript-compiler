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
