
class: center, middle

#  OCaml <3 Javascript

<img src="images/Slide3.PNG" title="OCamlscript" alt="Drawing" style="width: 550px;"/>

Hongbo Zhang@Bloomberg

May 19, 2016

---

# Why OCaml

- You are here probably you already know [the great parts of OCaml](https://realworldocaml.org/v1/en/html/prologue.html)

# Why JS

- JS is the only language of the browser

- JS is everywhere (Electron for Desktop App, NodeJS on server
  side, huge potential on [IoT](https://blogs.windows.com/buildingapps/2015/05/12/bringing-node-js-to-windows-10-iot-core/))

- JS is where people are (npm: [largest package manager 2 years ago](http://www.modulecounts.com/))

---


# Benefits of BuckleScript

- For JS developers :

  - Type safety
  - Higher (both compile-time and runtime) performance
  - Smaller


- For OCaml developers (Today's topic)

  - Seamless integration with existing JS ecosystem

---

# What BuckleScript looks like

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
// Immutable map from Facebook immutable library

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

| Technology   |  Time(s) | Code Size  |
|--------------|----------| -----------|
| OCaml with Javascript Backend   |1186ms (Google Closure bundler: simple mode) |   1 KB |
| Handwritten Facebook Javascript  |3415ms |  55.3 KBytes|

---


# BuckleScript, Javascript/Typescript interaction

1. Call BuckleScript from Javascript code (no extra work to do)

2. Call BuckleScript from Typescript: BuckleScript emits `.d.ts` files
   for TypeScript compiler (*experimental*)

3. Call Javascript from BuckleScript (type declarations like TypeScript)


---

# BuckleScript Function FFI

```OCaml
external array_map : 'a array -> ('a -> 'b [@uncurry]) -> 'b array =
  "Array.prototype.map.call" [@@bs.call ]


let v = array_map [|1,2,3|] (fun%uncurry x -> x+ 3 )
```

```js

var v = Array.prototype.map.call(
          [1,2,3],
          (function(x){return x + 3}))
```

---

# BuckleScript Object FFI - I

```OCaml
(** [Http_lib.ml] *)
let port = 3000
let hostname = "127.0.0.1"
let create_server  http =
  let server = http##createServer (fun %uncurry  (req,  resp)  ->
      resp##statusCode__set 200; (* setter always ends with [__end] *)
      resp##setHeader("Content-Type", "text/plain");
      resp##end__ "Hello world\n" (* end is a key word in OCaml *)
    )
  in
  server##listen(port, hostname,  fun %uncurry () ->
      Js.log ("Server running at http://"^ hostname ^ ":" ^ string_of_int port ^ "/")
    )

```

```js
var hostname = "127.0.0.1";

function create_server(http) {
  var server = http.createServer(function (_, resp) {
        resp.statusCode = 200;
        resp.setHeader("Content-Type", "text/plain");
        return resp.end("Hello world\n");
      });
  return server.listen(3000, hostname, function () {
              console.log("Server running at http://" + (hostname + (":" + (3000 + "/"))));
              return /* () */0;
            });
}
```
---

# BuckleScript Object FFI - II

```ocaml
(** Bindings for NodeJS [Http_binding.ml] *)

type req

type resp = <
   statusCode__set : int -> unit [@uncurry] ;
   setHeader : string * string -> unit [@uncurry] ;
   end__ : string ->  unit [@uncurry]
> Js.t

type server =  <
   listen : int * string *  (unit -> unit [@uncurry]) -> unit [@uncurry];
> Js.t



type http = <
   createServer : (req  * resp  -> unit [@uncurry] ) ->  server [@uncurry]
> Js.t


external http : http  = "http"  [@@bs.val_of_module ]

```

*Pure types, no Code generated*, like *tsd*, but it is just plain
 OCaml program.

---

# BuckleScript Object FFI - II

```OCaml
(** [Http_start.ml]*)

let () =
  Http_lib.create_server Http_binding.http
```

```js
var Http_lib = require('./http_lib');
var http = require('http');
Http_lib.create_server(http);
```

---

# How can it be possible? (What's the magic?)

1. OCaml is like a *formal* Javascript

   Both OCaml and Javascript have similar concepts which make compiling OCaml to Javascript posible:

    ![](OCaml_Javascript_features.png)

2. OCaml has a similar **module system** to Javascript 2015, it supports both Javascript like structural typing
   and ML style type inference.
3. ML is used in some of the foundational work of Javascript:
   - ES4 (abandoned standard) reference implementation
   - The first prototype of Facebook's ReactJS implementation
   - Official reference implementation of WebAssembly


---

# Internals of BuckleScript - an optimizing JS backend for OCaml

Leverage the (high-level, strongly typed) OCaml language tool-chain to generate optimized JS


- Dev mode:
  - *No name mangling*, easy to debug
  - Separate and **blazing fast** compilation
  - Easy integration with existing JS libraries
  - Dead code elimination and Purity analysis
  - Local and cross module optimization
- Production mode:
  - Link time optimization (combination with Google Closure Compiler)
  - Remove unused functions further in the library level
- Result:
  - **Faster, Smaller, and Safer!**


---

# The OCaml compiler workflow

```
 Source code
        |
        | parsing and preprocessing
        |
        v
    Parsetree (untyped AST)
        |
        | type inference and checking
        v
    Typedtree (type-annotated AST)
        |
        | pattern-matching compilation
        | elimination of modules and classes
        v
     Lambda ------------------------(our work)----------+
      /   \                                              \
     /     \ closure conversion, inlining, uncurrying,    \
    v       \  data representation strategy                \
 Bytecode    \                                              |
    |         +-----+                                       |
    |              Cmm                                      IR
    |ocamlrun       |                                       |
    |               | code generation                       | code generation
    |               | assembly & linking                    |
    v               v                                       v
 Interpreted    Compiled                                 Javacript(and meta data for optimizations)
```

---

# Comparison with  Typescript

* Readable output code, great editor

* Compile time slow

  A micro benchmark:

  Two files: one file define 500 fib functions,
  one file call those 500 fib functions

   ```
             BS: 0m0.063s
             TS: 0m1.427s
   ```
  BuckleScript will be even faster in the future with flambda enabled.

* Unsound type system, limited type inference (all arguments have to be annotated)
* Only Javascript backend
* No code optimizations


# [Comparison with  Js_of_ocaml](https://bucklescript.github.io/bucklescript/Differences-from-js_of_ocaml.html)

---

# Demo

* [Code Repo](https://github.com/bobzhang/bucklescript-demo)

---

# Future work

* More tests (currently around 1400 tests)

* Bindings to existing JS library (using typescript compiler API)

* Toolings and help get people started

* Catching up with the latest compiler (currently work with 4.02.3)

* Optimizations

* Our own bundler for purely OCaml part










