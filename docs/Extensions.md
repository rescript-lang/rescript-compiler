


- Embedding raw js code in expression and structure level

```ocaml
let x = [%bs.raw .. ]
[%%bs.raw]
```
- Debugger in expression level

```ocaml
[%bs.debugger]
```

- Create JS object in expression and core-type level

[%bs.obj ({ x = 3 } : < x : int > )]


Curry  Uncurry, and methods


- types

```ocaml
val f0 : unit -> string [@fn] (* arity of 0 *)
val f1 : int -> int -> string [@fn] (* arity of 1 *)
val f2 : int -> string -> int [@fn] (* arity of 2 *)
```
- application 

```ocaml
f0 () [@fn]
f1 a [@fn]
f2 a b [@fn]
```
- object application

```ocaml
f##meth0 ()
f##meth1 a
f##meth2 a b 
```

- object application chain

```ocaml
f##(meth0 ())
f##(meth1 a)
f##(meth2 a b)
```

- object method application

```ocaml
f#.meth0 ()
f#.meth1 a
f#.meth2 a b
```
```ocaml
val meth0 : [%meth: 'obj * (unit -> int)]
val meth1 : [%meth1: 'obj * ('a -> int)]
val meth2 : [%meth2: 'obj *('a -> 'b -> int')]
```

- object method chaining

```ocaml
f#.(meth0 ())
f#.(meth1 a)
f#.(meth2 a b)
```

- function create

```ocaml
fun [@fn] () -> x
fun [@fn] _ -> x (* arity of 0*)
fun [@fn] x -> x (* arity of 1*)
fun [@fn] x y -> x + y (* arity of 2*)
```

- method creat

```ocaml
fun[@meth] o x y -> o##length + x + y :
[%obj: < length : int >] -> int -> int -> int [@meth] 
```

- object

```ocaml
[%bs.obj {
  x = 3 ;
  y  = {z : 3 } ;
  u = fun[@fn] x y -> x + y;
  uu = fun%meth o x y -> o##x + x y 
  }
] : [%bs.obj: <
x : int ;
y : < z : int > ;
u : int -> int -> int  [@fn];
uu : int -> int -> int [@meth]
> ]
```

```ocaml
[%bs.obj: <
x : int ;
y : 'a -> int 
>  as 'a]
```
- class type

```ocaml
class type x = object [@fn]
  method hey : unit -> int
  method hi : int -> (int -> int) -> int
  method ui : int -> (x -> int -> unit [@meth])  -> int
end
```

```ocaml
[%%fn
  class type x = object
    method u : int -> [%meth: ]
  end
];;
```
