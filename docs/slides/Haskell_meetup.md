
class: center, middle

#  OCaml <3 Javascript

<img src="images/Slide3.PNG" title="OCamlscript" alt="Drawing" style="width: 550px;"/>

Hongbo Zhang@Bloomberg

July 27, 2016

---

# What's [BuckleScript](https://github.com/bucklescript/bucklescript)

- Readable JavaScript backend for OCaml
- *module to module* compiler
- *Seamless integration* with existing JS libraries
- Works with multiple front-end: vanilla OCaml and [Reason](https://facebook.github.io/reason/)


---

# Why target JS platform

- JS is the only language of the browser
- JS is everywhere (Electron for Desktop App, NodeJS on server
  side,  IoT etc)
- JS is where people are (npm: [largest package manager 2 years ago](http://www.modulecounts.com/))
- WebAssembly will make it more capable

---


# Major Benefits of BuckleScript

- It's OCaml, not a new language (30 years distilled research in PL and amazing effort of compiler engineering)
  - Native backends: AMD64, IA32, PowerPC, ARM, SPARC
  - Language based OS: Mirage Unikernel (acquired by Docker)
  - Battle tested: Used for trading in volumes of billions of
    dollars per day
- BuckleScript backend :
  - Great performance with regard to both compile-time and runtime
  - *Expressive* and *efficient* FFI
  - Small size (small runtime: *linked only when needed like int64 support*)
  - Module to module separate compilation, support various module systems: Google module/ AMDJS and CommonJS


---

# R & B ( Reason & BuckleScript)

- Growing support to make OCaml more JavaScript friendly

- Reason is a new interface to OCaml created by the same people who
  created ReactJS, ReactNative

- It provides a Javascript like syntax and toolchain for editing, building, and sharing code

- [It works with BuckleScript nicely](http://bucklescript.github.io/bucklescript/reason-demo/)

---

# A small example of BuckleScript

```ocaml
let test () =
  let m = ref IntMap.empty in
  let count = 1000000 in
  for i = 0 to count do
    m := IntMap.add i i !m
  done;
  for j = 0 to count  do
    ignore (IntMap.find j !m )
  done;;
test()
```
`Generated code`

```js

"use strict";
var Int_map=require("./int_map.js");

function test() {
  var m = /* Empty */0;
  for(var i = 0; i <= 1000000; ++i){
    m = add(i, i, m);
  }
  for(var j = 0; j <= 1000000; ++j){
    find(j, m);
  }
  return /* () */0;
}

test(/* () */0);
```

---

# Comparison with hand-written JS

```js
// Immutable map from  immutablejs library

'use strict';
var Immutable = require('immutable');
var Map = Immutable.Map;
var m = new Map();
function test(){
    var count  = 1000000
    for(var i = 0; i < count; ++i){
        m = m.set(i, i );
    }
    for(var j = 0; j < count ; ++j){
        m.get(j)
    } }
test ()
```

Runtime performance of identical functionality:

| Technology   |  Time(ms) | Code Size  |
|--------------|----------| -----------|
| OCaml with Javascript Backend   |1186ms (Google Closure bundler: simple mode) |   1 KB |
| Handwritten  Javascript  |3415ms |  55.3 KBytes|

---

# Finishes before others warm up

- A truly fast compiler (cold startup)

  <img src="images/compile-time.PNG" title="OCamlscript" alt="Drawing" style="width: 450px;"/>
- A micro benchmark vs TypeScript
   - one file defines 500 fib functions
   - one file calls those 500 fib functions
   ```
             BS: 0m0.063s
             TS: 0m1.427s
   ```
- Even worse for whole program compilation transpilers

---

# Beyond performance:

>  Expressive and efficient FFI is the major design goal

---

# FFI part one (Call OCaml from Javascript for free)

- OCaml signatures are respected (no extra work to do)
- Basic data types are closely matched (Array -> Array, Tuple -> Array, etc)
- BuckleScript can also emit `.d.ts` files for TypeScript compiler (*experimental*)

- [Publish and consume npm packages out of box](https://www.npmjs.com/package/bs-platform)
- [OCaml standard library consumed by Javascript developers](http://caml.inria.fr/pub/docs/manual-ocaml/stdlib.html)
- [Demo](https://tonicdev.com/npm/bs-platform)
```ocaml
var Array = require("bs-platform/lib/js/array")
var String = require("bs-platform/lib/js/string")
String.concat(",",Array.to_list(["hello","bucklescript"]))
```

---
# FFI part two : (Calling Javascript from OCaml)

Like typescript, users must write *type declarations* for existing
JavaScript Libraries.

- *extensible* language by extension points and attributes.
  - Not re-inventing a new language, still OCaml
-  *expressive* type system to model different  paradigms in Javascript
  - Structural typing (model JavaScript Objects)
  - Polymorphic variants (model Event handler)
  - Label and optional arguments (model JSON configuration)

```
external val_name : types_to_js_object_or_function
```
- *external* function type declarations
- *external* object signature

---
A dummy example:

```ocaml
external exp : float -> float = "Math.exp" [@@bs.val]
let v = exp 3.
```
---

# FFI highlights: native uncurried calling convention support

```ocaml
let f = fun [@bs] x y -> x + y

let u = f 1 2 [@bs]
let u = f 1  (* compile error *)
let u = f 1 2 3 (* compile error *)

val f : int -> int -> int [@bs]
```
`Generated code`

```js
function f(x,y){
  return x + y;
}
f (1,2)
```
---

# FFI hightlights: built-in *this* callback support

```ocaml
let f = fun [@bs.this] o x y -> body
val f : 'o -> 'x -> 'y -> 'body [@bs.this]
```

```ocaml
external array_map_this :
  'a array -> ('obj -> 'a -> int -> 'b [@bs.this]) -> 'obj -> 'b array
    = "map"  [@@bs.send]
let v =
  let arr = [|1;2;3|] in
  array_map_this arr (fun [@bs.this] o v i -> (o,v,i)) arr
```

---

# FFI highlights: String and int literal type


```ocaml
external readFileSync :
  name:string ->
  ([`utf8 | `ascii] [@bs.string]) ->
  string = ""
  [@@bs.module "fs"]

let content = readFileSync `utf8  ~name:"file.txt"
```
---

# FFI highlights: type-safe event handlers

Typescript binding:
```ts
interface readline {
  on : (event:string, callback: Function)
}
```

```ocaml
type readline
external on : readline ->
  ([ `line of string -> unit  (* can be customized [@bs.as "another_name"]*)
   | `close of unit -> unit ]
     [@bs.string]) ->  unit = "" [@@bs.send]

let register readline =
  on readline (`line begin fun s -> prerr_endline s end);
  on readline (`close begin fun () -> prerr_endline "finished" end);
  print_endline "done"
```

---

# FFI highlights: structural typing and type safe JSON literals

- In BuckleScript, `##` is used as method dispatch

```ocaml
let f obj = obj##height + obj##width
val f : [%bs.obj: < height : int ; width : int ; .. > ] -> int
let a = f [%bs.obj { height = 3; width = 32}] (* compiles *)
let b = f [%bs.obj {height = 3 ; width  = 32; unused = 3 }] (* compiles *)
```
- class type, subtyping, and inhertiance are also supported in FFI

```ocaml
class type title = object
  method title : string
  end [@bs]

class type widget = object
   inherit title
   end [@bs]

let f (x : widget )  = (x :> title)
```

---

# Demo: A stand alone HTTP server

https://github.com/bucklescript/bucklescript-addons/tree/master/examples/node-http-server


---

# Optimizations

- Code motion, Purity analysis, Cross module inliner, Constant folding/propogation, Strength reduction, escape analysis etc
- Examples: optimized curry calling convention

  ```ocaml
  let f x y z = x + y + z
  let a = f 1 2 3
  let b = f 1 2
  ```
  `Compilation used in PureScript`

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
  `Optimized  in BuckleScript(cross modules arities infererence)`
  ```js
  function f(x,y,z) {return x + y + z}
  var a = f(1,2,3)
  var b = function(z){return f(1,2,z)}
  ```


---

# Comparison with  Elm, PureScript and GHCJS


- Elm/PureScript:
  - Both generate readable code and support structural types
  - PuresScript is pure, while OCaml allows both imperative style and
    OO style and also OO FFI
  - BuckleScript have *different backends* and frontends

- GHCJS
   - Compile vanilla Haskell into JS
     - Whole program compiler
     - Monolithic Javascript
     - Semantics mismatch
   - All great features of Haskell
   - Large size, unreadable code, slow compile

---

# Future work

- Reaches 1.0 soon!
- Testing
- Bindings to existing JS libraries (using typescript compiler API or
   Flow type checker  (also written in OCaml))
- Better integration with Reason and its tool chain
- Documentation and tutorials
- More Optimizations

Follow me for the latest development on BuckleScript
  twitter @bobzhang1988















