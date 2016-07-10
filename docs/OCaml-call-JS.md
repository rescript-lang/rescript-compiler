To make OCaml work smoothly with Javascript, we introduced several
extensions to the OCaml language. These BuckleScript extensions
facilitate the integration of native JavaScript code as well as
improve the generated code.

Like TypeScript, when building typesafe bindings from JS to OCaml, the user has to write type declarations.
In OCaml, unlike TypeScript, user does not need to create a separate `.d.ts` file, 
since the type declaration langauge is the same langauge in OCaml.

The FFI is divided into several components:

- Binding to JS functions
- Binding to JS object literals
- Binding to JS classes

## FFI to first-order JS functions

This part is similar to [traditional FFI](http://caml.inria.fr/pub/docs/manual-ocaml-4.02/intfc.html), 
with syntax as described below:

```
external value-name :  typexpr =  external-declaration  attributes
external-declaration :=	 string-literal  
```

Users need to declare types of the foreign function (JS function here) and 
give it a type and customized attributes.

### Attributes

* `bs.call`

  Example 

  ```ocaml
  external imul : int -> int -> int = "Math.imul" [@@bs.call]
  ```
> Note that if you want to make a single FFI for both C functions and JavaScript functions, you can 
 give the JavaScript foreign function different name:

  ```ocaml
  external imul : int -> int -> int = "c_imul" [@@bs.call "Math.imul"]
  ```

* `bs.new`

  This attribute is to help the user create a JavaScript object.
  Example:

  ```ocaml
  external create_date : unit -> t = "Date" [@@bs.new]
  ```

* `bs.val` 

   Bind to a JavaScript value

   ```OCaml
   type dom 
      (* Abstract type for the DOM *)
   
   external dom : dom = "document" [@@bs.val]
  ```

* `bs.send`
  
  This attribute is to help the user send a message to JS object

  ```OCaml
  type id 
    (** Abstract type for id object *)
  external get_by_id : dom -> string -> id = "getElementById" [@@bs.send]
  ```
  
  The object is always the first argument and actual arguments follow.

  ```OCaml
  getElementById dom "xx"
  ```
  will be compiled as 
  ```js
  dom.getElementById("xx")
  ```

* `bs.get`, `bs.set`
  This attribute helps get and set the property of a JavaScript object.

  ```OCaml
  type textarea
  external set_name : textarea -> string -> unit = "name" [@@bs.set]
  external get_name : textarea -> string = "name" [@@bs.get]
  ```

* `bs.set_index` `bs.get_index`

  This attribute helps dynamic access to a JavaScript property

  ```OCaml
  module Int32Array = struct
    type t
    external create : int -> t = "Int32Array" [@@bs.new]
    external get : t -> int -> int = "" [@@bs.get_index]
    external set : t -> int -> int -> unit = "" [@@bs.set_index]
  end
  ```


* `bs.module`

   Qualify the JavaScript value by a module name

   ```OCaml
   external add : int -> int -> int = "add" [@@bs.call] [@@bs.module "x"]
   let f = add 3 4
   ```
   will be compiled as 

   ```js
   var X = require("x")
   var f = X.add(3,4)
   ```

   ```OCaml
   external add : int -> int -> int = "add" [@@bs.call] [@@bs.module "x" "U"]
   let f = add 3 4
   ```
   Will be compiled as

   ```js
   var U = require("x")
   var f = U.add(3,4)
   ```

## FFI to high-order JS functions

High order function means callback can be another function, for example, suppose
JS has a map function as below:

```js
function map (a, b, f){
  var i = Math.min(a.length, b.length);
  var c = new Array(i);
  for(var j = 0; j < i; ++j){
    c[j] = f(a[i],b[i])
  }
  return c ;
}
```

A naive external type declaration would be as below:

```ocaml
external map : 'a array -> 'b array -> ('a -> 'b -> 'c) -> 'c array = "map" [@@bs.call]
```

Unfortunately, this is not completely faithful. The issue is by 
reading the type `'a -> 'b -> 'c`, it can be in several cases:

```ocaml
let f x y = x + y
```

```ocaml
let g x  = let z  = x + 1 in fun y -> x + z 
```

In OCaml, they all have the same type, however, 
`f` and `g` may be compiled into functions with
different arities.

A naive compilation, compile `f` as below:

```ocaml
let f  = fun x -> fun y -> x + y
```

```js
function f(x){
  return function (y){
    return x + y;
  }
}
function g(x){
  var z = x + 1 ;
  return function (y){
    return x + z ; 
  }
}
```

Its arity will be *consistent* but is *1* (returning another function), however, 
we expect *its arity to be 2*. 


A more complex compilation strategy used in BuckleScript would compile `f` as

```js
function f(x,y){
  return x + y ; 
}
```

**No matter which startegy we use, by just using existing typing rules, we can not
guarantee a function of type `'a -> 'b -> 'c` will have arity 2.**

To solve the problem introduced by OCaml's curried calling convention, we 
introduce a special attribute `[@bs]` in the type level.

```ocaml
external map : 'a array -> 'b array -> ('a -> 'b -> 'c [@bs]) -> 'c array
= "map" [@@bs.call]
```

Here `('a -> 'b -> 'c [@bs])` will be *always of arity 2*, in general 
`'a0 -> 'a1 ... 'aN -> 'b0 [@bs]` is the same as `'a0 -> 'a1 ... 'aN -> 'b0`
except the former's arity is guaranteed to be `N` while the latter is unknown.

To produce a function of type `'a0 -> .. 'aN -> 'b0 [@bs]`, as follows:

```ocaml
let f : 'a0 -> 'a1 -> .. 'b0 [@bs] = fun [@bs] a0 a1 .. aN -> b0 
let b : 'b0 = f a0 a1 a2 .. aN [@bs] 
``` 
A special case for arity of 0:

```ocaml
let f : unit -> 'b0 [@bs] = fun [@bs] () -> b0 
let b : 'b0 = f () [@bs]
```


Note that this extension to the OCaml language is *sound*, if you add 
an attribute in one place and miss it in other place, the type checker
will complain.

Another more complex example:

```ocaml
type 'a return = int -> 'a [@bs]
type 'a u0 = int -> string -> 'a return  [@bs]
(* [u0] has arity of 2, return a function 
   with arity 1
*)
type 'a u1 = int -> string -> int -> 'a [@bs]
(* [u1] has arity of 3 *)
type 'a u2 = int -> string -> (int -> 'a [@bs]) [@bs]
(* [u2] has arity of 2, reutrn a function 
   with arity 1
*)
```


### Uncurried calling convention as an optimization 

#### Background
  
As we discussed before we can compile any OCaml function as arity 1 to 
support OCaml's curried calling convention. 

This model is simple and easy to implement, but
the native compilation is very slow and expensive for all functions.


```ocaml
let f x y z = x + y + z
let a = f 1 2 3 
let b = f 1 2 
```

would be compiled as  

```js
function f(x){
  return function (y){
    return function (z){
      return x + y + z
    }
  }
}
var a = f (1) (2) (3)
var b = f (1) (2)
```

But as you can all see, this is *highly inefficient*, since the compiler already *saw the source definition* of `f`.
It can be optimized as below:

```js
function f(x,y,z) {return x + y + z}
var a = f(1,2,3)
var b = function(z){return f(1,2,z)}
```

We do this optimization in the cross module level and try to infer the arity as much as we can.

### Callback optimization

However, such optimization will not work with *high-order* functions, 
i.e, callbacks.

For example,

```ocaml
let app f x = f x
```
Since `f`'s arity is unknown, the compiler can not do any optimization (unless `app` gets inlined), so we 
have to generate code as below:

```js
function app(f,x){
  return Curry._1(f,x);
}
```
`Curry._1` is a function to dynamically support the curried calling convention. 

Since we add uncurried calling convention support, you can write `app`
as below

```ocaml
let app f x = f x [@bs]
```

Now the type system will infer `app` as type 
`('a ->'b [@bs]) -> 'a` and compile `app` as 

```js
function app(f,x){
  return f(x)
}
```


> Note in OCaml, the compiler internally uncurried every function declared as `external`, 
in that case, the compiler guaranteed that it is always fully applied, so 
for `external` first-order FFI, its outermost function does not need `[@bs]` 
annotation.

### Bindings to callback which relies on `this`

It's quite common that in JS library, a callback relies on `this` (the source), for example:

```js
x.onload = function(v){
  console.log(this.response + v )
}
```
Here, `this` would be the same as `x` (actually depends on how `onload` is called), it is clear that
it is not exact to declare `x.onload` of type `unit -> unit [@bs]`, instead we introduced a special attribute
`bs.this`, in that case we an type `x` as below:

```ocaml
type x 
external onload : x -> (x -> int -> unit [@bs.this]) -> unit = "onload" [@@bs.set]
external resp : x -> int = "response" [@@bs.get]
onload x begin fun [@bs.this] o v -> 
  Js.log(resp o + v )
end
```

The generated code would be as blow:

```js
x.onload = function(v){
  var o = this ; 
  console.log(o.response + v);
}
```

`bs.obj` is the same as `bs`: except that its first parameter is reserved for `this`, and for arity of 0, there
is no need for a redundant `unit` type:

```ocaml
let f : 'obj -> unit [@bs.this] = fun [@bs.this] obj -> ....
let f1 : 'obj -> 'a0 -> 'b [@bs.this] = fun [@bs.this] obj a -> ...
```

> Note there is no way to consume function of type `'obj -> 'a0 .. -> 'aN -> 'b0 [@bs.this]` on OCaml side,
we don't encourage people to write code in this style, it was introduced mainly for consumed by existing JS library
you can also type `x` a an JS class too (see later)


## FFI to JS plain objects


### Js object convention

All JS object of type `'a` are lifted to type `'a Js.t` to avoid
conflict with OCaml's own object system(We support both OCaml's own object system and FFI to JS's objects). 

`##` is used in JS's object method dispatch and field access, 
while `#` is used in OCaml's object method dispatch.

### Create simple JS object literal and its typing


BuckleScript introduces `bs.obj` extension, for example:

```ocaml
let u = [%bs.obj { x = { y = { z = 3}}} ]
```

Would be compiled as 

```js
var u = { x : { y : { z : 3 }}}}
```

The compiler would infer `u` has type

```ocaml
val u : < x :  < y : < z : int > Js.t >  Js.t > Js.t
```

To make it more symmetric, we also apply the extension `bs.obj` 
into the type level, so you can write

```ocaml
val u : [%bs.obj: < x : < y < z : int > > > ]
```

User can also write expression and types togehter as below:

```ocaml
let u = [%bs.obj ( { x = { y = { z = 3 }}} : < x : < y : < z : int > > > ]
```

Even better, user can also write Objects in a collection:

```ocaml
var xs = [%bs.obj [| { x = 3 } ; {x = 3 } |] : < x : int  > array  ]
var ys = [%bs.obj [| { x = 3} : { x = 4 } |] ]
```

which will be compiled as below:

```js
var xs = [ { x : 3 } , { x : 3 }]
var ys = [ { x : 3 },  {x : 4 } ]
```



`bs.obj` can also be used as an attribute in external declarations, like as below:

```OCaml
external make_config : hi:int -> lo:int -> unit -> t [@@bs.obj]
let v = make_config ~hi:2 ~lo:3
```
will be compiled as 

```js
  let v = { hi:2, lo:3}
```

You can use optional as well

```ocaml
external make_config : hi:int -> ?lo:int -> unit -> t = "" [@@bs.obj]
let u = make_config ~hi:3 ()
let v = make_config ~hi:3 ~lo:2 ()
```
Will generate
```js
let u = {hi : 3}
let v = {hi : 3 , lo: 2}
```

#### Field access

As we said `##` is used in both object method dispatch and field access.

```ocaml
f##field (* field access should not come with any argument *)
f##method args0 args1 args2 (* method with arities of 3 *)
```

JS's **method is not a function**, a classis example is as below:

```js
console.log('fine')
var log = console.log;
log('fine') // May cause exception, implementation dependent, `console.log` may depend on `this` 
```

So to make it clearly type safe, for `field` access, it should not come with any argument.

```ocaml
let fn = f##field in
let a = fn a b 
(* f##field a b would think `field` as a method *)
```

> Note that if user make such a mistake, the type checker would complain it expect `Js.method` but have a
function instead, so it is still sound and type safe.

Currently `bs.obj` only supports plain JS object literal with no support of JS method, `class type` (discussed later) supports JS style method.

So, 
```ocaml
let u = [%bs.obj {x = { y = { z = 3 }}; fn = fun [@bs] u v -> u + v } ]
let h = u##x##y##z
let a = h##fn
let b = a 1 2
```

will be compiled as below:

```js
var u = { x : { y : {z : 3}}, fn : function (u,v) {return u + v}}
var h = u.x.y.z
var a = h.fn
var b = a(1,2)
```

When the field is an uncurried function, there is a short cut syntax as below:

```ocaml
let b x y h = h#@fn x y
```
It will be compiled as 

```js
function b (x,y,h){
  return h.fn(x,y)
}
```
And compiler infer the type of `b` as

```ocaml
val b : 'a -> 'b -> [%bs.obj: < fn :  'a -> 'b -> 'c [@bs] ] -> 'c
```

As we said before, currently `[%bs.obj]` is only used for object literals with no `this` semantics.

## FFI to JS classes

### Class type declarations

Below is an example:

```ocaml
class type _rect = object
  method height : int [@@bs.set]
  method width : int [@@bs.set]
  method draw : unit -> unit
end [@bs]
type rect = _rect Js.t
```
As the example, `class type` annotated with `[@bs]` are treated as JS class type.
For JS classes, methods with arrow types are treated as real methods while methods with non-arrow types
are treated as properties, since OCaml's object system does not have getter/setter, we introduce two
attributes `bs.get` and `bs.set` to help inform BuckleScript to compile them as property getter/setter.


#### Annotation to JS properties

There are varous getter/setter decorations as below:

```ocaml
class type _y = object 
  method height : int [@@bs.set {no_get}]
  (* [height] is setter only *)
end [@bs]
type y = _y Js.t 
class type _y0 = object 
  method height : int [@@bs.set] [@@bs.get {null}] 
  (* getter reutrn [int Js.null]*)
end [@bs]
type y0 = _y0 Js.t 
class type _y1 = object 
  method height : int [@@bs.set] [@@bs.get {undefined}]
  (* getter return [int Js.undefined]*)
end [@bs]
type y1 = _y1 Js.t 
class type _y2 = object 
  method height : int [@@bs.set] [@@bs.get {undefined; null}] 
  (* getter return [int Js.null_undefined] *)
end [@bs]
type y2 = _y2 Js.t 
class type _y3 = object 
  method height : int  [@@bs.get {undefined ; null}] 
  (* getter only, return [int Js.null_undefined] *)
end [@bs]
type y3 = _y3 Js.t
```

#### Consume JS class API

For example, 

```ocaml
let f (u : rect) =   
  (* the type annotation is un-necessary,
     but it gives better error message
  *) 
   Js.log u##height ; 
   Js.log u##width ;
   u##width #= 30;
   u##height #= 30;
   u##draw ()
```
Would be compiled as below:

```js
function f(u){
  console.log(u.height);
  console.log(u.width);
  u.width = 30;
  u.height = 30;
  return u.draw()
}
```

Note the type system would guarteen that user can not write such code:

```ocaml
let v = u##draw 
(* use v later *)
```

This is more type safe, since in JS, method is not function.


## Embedding raw Javascript code

- extension `bs.raw`
  
   It can be either `[%bs.raw{|  this_is_arbitrary_js_expression |}]` or `[%%bs.raw{| this is arbitrary_js_statement |}`
   
   Use cases:
   for example if you want to use a JavaScript string, you can write code like this:
   
   ```OCaml
   let x  : string = [%bs.raw{|"\x01\x02"|}]
   ```

   which will be compiled into:

   ```js
   var x = "\x01\x02"
   ``` 

   ```OCaml
   [%%bs.raw{|
   // Math.imul polyfill
   if (!Math.imul){
       Math.imul = function (..) {..}
    }
   |}]
   ```
   In the expression level, i.e, `[%bs.raw ...]` user can add a type
   annotation, for example:

   ```ocaml
   let f : float -> float -> float [@bs] = [%bs.raw "Math.max" ]
   in f 3.0 2.0 [@bs]
   ```
   will be translated into:

   ```js
   var f = Math.max ;
   f(3.0,2.0)
   ```
   Caveat:
   1. So far we don't do any sanity check in the quoted text (syntax check is a long-term goal)
   2. You should not refer to symbols in OCaml code. It is not guaranteed that the order is correct.
      You should avoid introducing new symbols in the raw code, but if needed use the `$$` prefix (ie `$$your_func_name`) 

## Debugger support

- extension `bs.debugger`

   It can be `[%bs.debugger]`

   use case

   ```ocaml
   let f x y = 
      [%bs.debugger];
      x + y
   ```

   which will be compiled into:

   ```js
   function f (x,y) {
     debugger; // JavaScript developer tools will set an breakpoint and stop here
     x + y;
   }
   ```

## Examples:
### A simple example: binding to mocha unit test library

   If we want to provide bindings to the [mochajs](https://mochajs.org/) unit test framework, 
   below is an example:

   ```OCaml
   external describe : string -> (unit -> unit [@bs]) -> unit = "describe" [@@bs.call]
   external it : string -> (unit -> unit [@bs]) -> unit = "it" [@@bs.call "it"]
   ```

   Since, `mochajs` is a test framework, we also need some assertion
   tests. We can also describe the bindings to `assert.deepEqual` from
   nodejs `assert` library:

   ```ocaml
   external eq : 'a -> 'a -> unit = "deepEqual"  [@@bs.call] [@@bs.module "assert"]
   ```

On top of this we can write normal OCaml functions, for example:

   ```OCaml
   let assert_equal = eq
   let from_suites name suite  = 
       describe name (fun [@bs] () -> 
         List.iter (fun (name, code) -> it name code) suite)
   ```

   The compiler would generate code as below:

   ```js
   var Assert = require("assert");
   var List = require("bs-platform/lib/js/list");

   function assert_equal(prim, prim$1) {
     return Assert.deepEqual(prim, prim$1);
   }

   function from_suites(name, suite) {
      return describe(name, function () {
              return List.iter(function (param) {
                          return it(param[0], param[1]);
                        }, suite);
            });
    }
   ```


