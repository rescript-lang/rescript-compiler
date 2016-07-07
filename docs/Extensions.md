


# Embedding raw js code in expression and structure level

```ocaml
let x = [%bs.raw .. ]
[%%bs.raw]
```
# Debugger in expression level

```ocaml
[%bs.debugger]
```

# Create Simple JS object in expression and core-type level

  - Types and expressions nested 
 
  ```ocaml
  [%bs.obj ({ x = 3 } : < x : int > )]
  ```
  - Types and expressions separate 

  ```ocaml
  [%bs.obj {x = 3 } ] : [%bs.obj: < x : int > ]
  ```
  - Object collections 

  ```
  [%bs.obj
  ([ {x = {y = 3}}]
  : < x : <y : int > > list )
  ]
  ```

  ```ocaml
  [%bs.obj {
  x = 3 ;
  y  = {z : 3 } ;
  u = fun[@bs] x y -> x + y;
  }
  ] : [%bs.obj: <
  x : int ;
  y : < z : int > ;
  u : int -> int -> int  [@bs];
  > ]
  ```

  ```ocaml
  [%bs.obj: <
  x : int ;
  y : 'a -> int 
  >  as 'a]
  ```



# Uncurry function, JS methods and JS method callback

## Uncurried function

Note that OCaml's calling convention is curried by default, while JS
does not have native support. Curried and uncurried functions can be
told from type signatures.

For example

```ocaml
val f0 : unit -> string [@bs] (* arity of 0 *)
val f1 : int -> int -> string [@bs] (* arity of 1 *)
val f2 : int -> string -> int [@bs] (* arity of 2 *)

val f : int -> string -> int
val f_uncurry : int -> string -> int [@bs]
```



- How BuckleScript compiles function application

  To apply a function, you can do this

```ocaml
f 3 "x" (* curried calling convention *)
f_uncurry 3 "x" [@bs]
```

Note if you missed arguments or supply more arguments to `f_uncurry`
the type checker would complain.

For uncurried function applicaton, BuckleScript is guaranteed to
compile it in the same way as JS code

```js
f_uncurry(3,"x")
```

However, for curried function application, it depends on how compiler
optimizations goes, for most cases, when the compiler see the
definition of `f`, it will compile it in the most efficient way, i.e,
JS full aplication, if the compiler can not see the definition of `f`,
it will do a runtime dispath, so there are two possible outputs:

```js
Curry._2(f, 3, "x") // compiler fails to optimize
f(3, "x") // compiler optimized correctly
```
Both are correct code, but the second one is more efficient.

- How BuckleScript handles function definition

```ocaml
let f = fun a b -> a + string_of_int b
let f_uncurry = fun [@bs] a b -> a + string_of_int b 
```

- When is uncurried function recommended

  - For FFI to JS functions, all object methods are *strongly recommended*
     to type it as uncurried function.

  - When function is passed as a callback

    This is mostly for performance issues, it is hard to optimize in
    such scenario.

Caveat: when return function you have to write separately

```ocaml
type 'a return = int -> 'a [@bs]
type 'a u0 = int -> string -> 'a return  [@bs]
type 'a u1 = int -> string -> int -> 'a [@bs]
type 'a u2 = int -> string -> (int -> 'a [@bs]) [@bs]
```
`u0` is the same as `u2` while `u1` is different, it expects 3 arguments

- application 

```ocaml
f0 () [@bs] (* arguments of 0 *)
f1 a [@bs] (* arguments of 1 *)
f2 a b [@bs] (* arguments of 2 *)
```

- function create

```ocaml
fun [@bs] () -> x
fun [@bs] _ -> x (* arity of 0*)
fun [@bs] x -> x (* arity of 1*)
fun [@bs] x y -> x + y (* arity of 2*)
```

# method

## method type declaration

- class type

BuckleScript overrides OCaml `class type` syntax for FFI, to trigger this, 
you have to put a file level configuration in the beginning.

```ocaml
[@@@bs.config {bs_class_type } ]
```

```ocaml
class type _x = object
  method height : int [@@bs.set]
  method bark : unit -> int
  method hi : int -> int
  method hey : int -> int   -> int
end 
type x = _x Js.t
```
- getter setter attributs

Such combinations:

```ocaml
[@@@bs.config{bs_class_type}]

class type _y = object 
  method height : int [@@bs.set {no_get}]
end
type y = _y Js.t 
class type _y0 = object 
  method height : int [@@bs.set] [@@bs.get {null}]
end
type y0 = _y0 Js.t 

class type _y1 = object 
  method height : int [@@bs.set] [@@bs.get {undefined}]
end
type y1 = _y1 Js.t 

class type _y2 = object 
  method height : int [@@bs.set] [@@bs.get {undefined; null}]
end
type y2 = _y2 Js.t 

class type _y3 = object 
  method height : int  [@@bs.get {undefined ; null}]
end
type y3 = _y3 Js.t
```

Which is equivalent to type definitions as below:
(Note that user can not write such type definitions, but its semantics-equivalent )

```ocaml
class type _y =
  object method height#= : ([ `Arity_1 of int ], unit) Js.meth end
type y = _y Js.t
class type _y0 =
  object
    method height : int Js.null
    method height#= : ([ `Arity_1 of int ], unit) Js.meth
  end
type y0 = _y0 Js.t
class type _y1 =
  object
    method height : int Js.undefined
    method height#= : ([ `Arity_1 of int ], unit) Js.meth
  end
type y1 = _y1 Js.t
class type _y2 =
  object
    method height : int Js.null_undefined
    method height#= : ([ `Arity_1 of int ], unit) Js.meth
  end
type y2 = _y2 Js.t
class type _y3 = object method height : int Js.null_undefined end
type y3 = _y3 Js.t
```


- object application

```ocaml
let h (f : x ) : int =  
   f##bark (); 
   f##hi a;
   f##hey a b ;
   x##height#= 32 ;
   x##height
```

- object application chain

```ocaml
f
##(meth0 ())
##(meth1 a)
##(meth2 a b)
```

Note that the only way to consume is `method` is via `##` syntax.


## method call back

This is useful for event-handler which will have a implicit this

- create 

```ocaml
let uu :[%bs.obj: < length : int > ] -> int -> int -> int [@bs.this] 
  =
  fun[@bs.this] o x y -> o##length + x + y
```

and it will be compiled as

```js
function uu(x,y){
  var o = this;
  return o.length + x + y
}

```

