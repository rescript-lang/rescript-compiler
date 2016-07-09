To make OCaml work smoothly with Javascript, we introduced several
extensions to OCaml language. Those BuckleScript extensions
facilitates the integration of native JavaScript code as well as
improve the generated code.

Like typescript, when build typesafe bindings from JS to OCaml, user has to write type declarations
in OCaml, unlike typescript, user does not need to create a separate `.d.ts` file, 
the type declaration langauge is the same langauge in OCaml.

The FFI is divided into components, one is binding to JS function, the other is binding to JS object.

## FFI to first order JS functions

This part is similar to [traditional FFI](http://caml.inria.fr/pub/docs/manual-ocaml-4.02/intfc.html), 
syntax is described as below:

```OCaml
external value-name :  typexpr =  external-declaration  attributes
external-declaration :=	 string-literal  
```

Users need declare types of the foreign function (JS function here) and 
gives it a type and customized attributes

### attributes

* `bs.call`

  Example 

  ```ocaml
  external imul : int -> int -> int = "Math.imul" [@@bs.call]
  ```
>Note that if you want to make a single FFI for both c functions and JavaScript functions, you can 
 give JavaScript foreign function different name

  ```ocaml
  external imul : int -> int -> int = "c_imul" [@@bs.call "Math.imul"]
  ```

* `bs.new`

  This attribute is to help user create a JavaScript object
  example:

  ```ocaml
  external create_date : unit -> t = "Date" [@@bs.new]
  ```

* `bs.val` 

   Bind to a JavaScript value

   ```OCaml
   type dom 
      (* Abstract type for the DOM *)
   
   external dom : dome = "document" [@@bs.val]
  ```

* `bs.send`
  
  This attribute is to help user send a message to js object

  ```OCaml
  type id 
    (** Abstract type for id object *)
  external get_by_id : dom -> string -> id = "getElementById" [@@bs.send]
  ```
  
  The object is always the first argument and arguments follow.

  ```OCaml
  getElementById dom "xx"
  ```
  will be compiled as 
  ```js
  dom.getElementById("xx")
  ```

* `bs.get`, `bs.set`
  This attribute help get and set the property of a JavaScript object.

  ```OCaml
  type textarea
  external set_name : textarea -> string -> unit = "name" [@@bs.set]
  external get_name : textarea -> string = "name" [@@bs.get]
  ```

* `bs.set_index` `bs.get_index`

  This attribute help dynamic access to JavaScript property

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

High oder function means callback can be another function, for example, suppose
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
they will be compiled into functions which have 
different arities.

Note even we do a naive compilation, compile `f` as below:

```ocaml
let f  = fun x -> fun y -> x + y
```

Its arity will be *consistent* but is *1* (return another function), however, 
we expect its arity to be 2. 

The conclusion is that we can not guarantee its arity to be 2 just 
by having declaring its type to be `'a -> 'b -> 'c` due to OCaml's 
curried calling convention.

To solve the problem, we introduce a special attribute.

```ocaml
external map : 'a array -> 'b array -> ('a -> 'b -> 'c [@bs]) -> 'c array
= "map" [@@bs.call]
```

Here `('a -> 'b -> 'c [@bs])` will be always of arity 2, in general 
`'a0 -> 'a1 ... 'aN -> 'b0 [@bs]` is the same as `'a0 -> 'a1 ... 'aN -> 'b0`
except the former's arity is guaranteed to be `N` while the latter is unknown.

To produce a function of type `'a0 -> .. 'aN -> 'b0`, as follows:

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


### Uncurried calling convention as an optimization 

As we discussed before we can compile any OCaml function as arity 1 to 
support OCaml's curried calling convention. 

This model is simple and easy to implement, however, 
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

But as you can all see, this is *highly inefficient*, since the compiler already *saw the source definition* of `f`
it can be optimized as below:

```js
function f(x,y,z) {return x + y + z}
var a = f(1,2,3)
var b = function(z){return f(1,2,z)}
```
We do this optimization in the cross module level, however, such optimization will not work with *high order* function, 
i.e, callback

For example,

```ocaml
let app f x = f x
```
Since `f`'s arity is unknown, the compiler can not do any optimization (unless `app` get inlined), so we 
have to generate code as below:

```js
function app(f,x){
  return Curry._1(f,x);
}
```
`Curry._1` is a function to dynamic support curried calling convention. 

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


Note in OCaml, the compiler internally uncurried every function `marked` with `external`, 
in that case, the compiler guaranteed that it is always fully applied, so 
for `external` FFI, its outermost function does not need `[@bs]` 
annotation.


## A simple example: binding to mocha unit test library

   If we want to provide bindings to the [mochajs](https://mochajs.org/) unit test framework, 
   below is an example

   ```OCaml
   external describe : string -> (unit -> unit [@bs]) -> unit = "describe" [@@bs.call]
   external it : string -> (unit -> unit [@bs]) -> unit = "it" [@@bs.call "it"]
   ```

   Since, `mochajs` is a test framework, we also need some assertion
   test, we can also describe the bindings to `assert.deepEqual` from
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
   var List = require("../stdlib/list");

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



## FFI to object


- Js object calling 
All JS object of type `a` are lifted to type `a Js.t` to avoid
conflict with OCaml's own object system. `##` is used in JS's object
method dispatch, while `#` is used in OCaml's object method dispatch.


For example

```ocaml
let f x a b = x ## hi (a,b)
```

is inferred as type

```ocaml
val f : < hi : ('a * 'b -> 'c [@bs] ;  .. > Js.t  -> 'a -> 'b -> 'c
```

- `bs.obj`

  This attribute helps create JavaScript object literal

```ocaml
  let a = f [%bs.obj { hi = fun [@bs] (x,y) -> x + y} ] 1 2 
  let b = f [%bs.obj { hi = fun [@bs] (x,y) -> x +. y} ] 1. 2.
  ```

   Generated code is like below 


   ```js
   function f(x, a, b) {
      return x.hi(a, b);
   }

   var a = f({
      "hi": function (x, y) {
       return x + y | 0;
      }
    }, 1, 2);

   var b = f({
     "hi": function (x, y) {
     return x + y;
    }
    }, 1, 2);
   ```

  `bs.obj` can also be used in external declarations, like as below:

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

## Embedding raw Javascript code

- extension `bs.raw`
  
   It can be either `[%bs.raw{|  this_is_arbitrary_js_expression |}]` or `[%%bs.raw{| this is arbitrary_js_statement |}`
   
   Use cases:
   for example if you want to use a JavaScript string, you can write code like this
   
   ```OCaml
   let x  : string = [%bs.raw{|"\x01\x02"|}]
   ```

   which will be compiled into 

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
   will be translated into 

   ```js
   var f = Math.max ;
   f(3.0,2.0)
   ```
   Caveat:
   1. So far we don't do any sanity check in the quoted text (syntax check is a long-term goal)
   2. You should not refer symbols in OCaml code, it is not guaranteed that the order is correct.
      You should avoid introducing new symbols in the raw code, if needed, use the `$$` prefix (ie `$$your_func_name`) 

## Debugger support

- extension `bs.debugger`

   It can be `[%bs.debugger]`

   use case

   ```ocaml
   let f x y = 
      [%bs.debugger];
      x + y
   ```

   which will be compiled into 

   ```js
   function f (x,y) {
     debugger; // JavaScript developer tools will set an breakpoint and stop here
     x + y;
   }
   ```


