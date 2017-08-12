

# BuckleScript

Bringing OCaml to Javascript developers

Hongbo Zhang, Bloomberg

OCaml Meetup in Paris

December 6, 2016

===


## Why BuckleScript?

### Why OCaml

- Save this slide for OCaml audience ^_^

---

### Why JS

- Arguably the most used programming language
- Everywhere and cross platform (Browser: the only language; Desktop
  App: Electron; Server: NodeJS, huge potential on [IoT](https://blogs.windows.com/buildingapps/2015/05/12/bringing-node-js-to-windows-10-iot-core/))
- NPM: largest package manager since 2014
- WebAssembly will make JS platform more attractive, for the first
  time cross platform FFI
- Bloomberg is a big industry user of Javascript

===

## Current status

- BuckleScript is still a young project (started late 2015)
- It covers the whole language, except libraries relying on c stubs
- It grows fast (the most starred open source project by Bloomberg),
  already used by external users in production (Collaborations with
  Facebook Reason team)

<blockquote style= 'background: #f9f9f9; ';>
    I'm on the Facebook Reason team, and we're using BuckleScript to compile OCaml into the best compiler output I've ever seen. People didn't recognize that my React components were generated, not hand-written.
</blockquote>


===

## Design goals (differences from Js_of_ocaml)

Highlights of BuckleScript:

- Easy FFI (OCaml -> JS, JS -> OCaml), incremental migration from JS
  to OCaml (One OCaml module <-> One JS module)
- Human debuggable output (not relying on source-map)
- Fast build (Save -> Compile -> Run:  100ms)
- Native Windows support (same build experience)

Advantages of Js_of_ocaml:

- No changes to your existing OCaml build system
- Less maintenance effort


===

## A brief overview of compiler pipeline


[Compiler pipelines](http://bucklescript.github.io/bucklescript/Manual.html#_high_level_compiler_workflow)

===

## What does it look like?

[Example:  balanced tree](http://bucklescript.github.io/bucklescript-playground/#Balanced_tree)


---

## Balanced tree with 2 million keys insertion and deletion

Execution Time (node v7.2.0, BuckleScript + Google Closure, jsoo minify):

- OCAMLOPT (-g): 0.837s
- BuckleScript: 2.219s
- JSOO: 3.035s
- OCAMLC (-g): 6.545s
- Using Facebook ImmutableJS lib: 13.520s

JS output size:
- BuckleScript: 542 bytes
- JSOO: 3836 bytes



===

## A brief look at FFI (Call OCaml from JS)

- `.mli` is respected
- Basic data types are closely matched (Array -> Array, Tuple -> Array, etc)
- BuckleScript can also emit `.d.ts` files for TypeScript compiler (*experimental*)
- [Publish and consume npm packages out of box](https://www.npmjs.com/package/bs-platform)
- [Call OCaml library from JS side](http://bucklescript.github.io/bucklescript-playground/#Use_OCaml_Standard_Library)

===

## FFI:  (Calling Javascript from OCaml)

Users must write *type declarations* for existing
JavaScript libraries

- Introducing built-in extension points and attributes
- Structural typing (model JavaScript Objects)
- Polymorphic variants (model Event handler)
- Label and optional arguments (model JSON configuration)


```ocaml
external exp : float -> float = "Math.exp" [@@bs.val]
let v = exp 3.
```

```js
var v = Math.exp(3.)
```

---

## FFI examples

Typescript binding:

```ocaml
type readline
type line_callback = string -> unit [@bs]
(*  [bs] annotation to mark it as uncurried callback  *)
type close_callback = unit -> unit [@bs]
external on : readline ->
   ([ `line of line_callback
    | `close of close_callback]
     [@bs.string]) ->  unit = "" [@@bs.send]

let register readline =
  on readline (`line begin fun s -> prerr_endline s end);
  on readline (`close begin fun () -> prerr_endline "finished" end);
  print_endline "done"
```

---

## FFI examples

- In BuckleScript, `##` is used as method dispatch

```ocaml
let f obj = obj##height + obj##width
val f : [%obj: < height : int ; width : int ; .. > ] -> int
let a = f [%obj { height = 3; width = 32}] (* compiles *)
let b = f [%obj {height = 3 ; width  = 32; unused = 3 }] (* compiles *)
```

---

## FFI examples (http server using Node.js)

- `Test_http_server`

```ocaml
let port = 3000
let hostname = "127.0.0.1"
let create_server http  =
  let server = http##createServer begin fun [@bs] req  resp  ->
      resp##statusCode #= 200;
      resp##setHeader "Content-Type" "text/plain";
      resp##end_ "Hello world\n"
    end
  in
  server##listen port hostname  begin fun [@bs] () ->
    Js.log ("Server running at http://"^ hostname ^ ":" ^ Pervasives.string_of_int port ^ "/")
  end

let () =
  create_server Http_types.http
```
---

## FFI examples (FFI bindings to NODEJS http module)

- `http_types`

```ocaml
type req
class type _resp = object
  method statusCode : int [@@bs.set]
  method setHeader : string -> string -> unit
  method end_ : string -> unit
end [@bs]
class type _server = object
  method listen : int -> string -> (unit -> unit [@bs]) -> unit
end [@bs]
type server = _server Js.t
class type _http  = object
  method createServer : (req  ->  resp  -> unit [@bs] ) ->  server
end [@bs]
type http = _http Js.t
external http : http  = ""  [@@bs.module]
```

---

### FFI (HTTP server)

- Output for `http.ml` is empty (pure type declarations)
- `Test_http_server`

```js
var Pervasives = require("bs-platform/lib/js/pervasives");
var Http       = require("http");

var hostname = "127.0.0.1";

function create_server(http) {
  var server = http.createServer(function (_, resp) {
        resp.statusCode = 200;
        resp.setHeader("Content-Type", "text/plain");
        return resp.end("Hello world\n");
      });
  return server.listen(3000, hostname, function () {
              console.log("Server running at http://" + (hostname + (":" + (Pervasives.string_of_int(3000) + "/"))));
              return /* () */0;
            });
}

create_server(Http);
```
===

## Easy to set up

Installation
```
npm install bs-platform
```

Create a JSON file to describe the build spec:

```js
{
    "name": "test",
    "sources": [
        {
            "dir": "src"
        }
    ]
}

```

Build and run

```
bsb -w
```


===

## Different Semantics  from other back-ends



[Semantics diverge](http://bucklescript.github.io/bucklescript/Manual.html#_semantics_difference_from_other_backends)


===

## Data representation

[OCaml data representation in JS](http://bucklescript.github.io/bucklescript/Manual.html#_runtime_representation)

===

## Wishes from OCaml compiler upstream

- Tell the difference between block and array in lambda layer
  - OCaml Array has to be JS array for better FFI
  - More efficient data layout, useful for jsoo too

---

## Wishes from OCaml compiler upstream
- Native uncurried calling convention support
  - Essential for FFI in callbacks
  - BuckleScript attributes is leaky in error message, can not be Polymorpphic
  - Useful for jsoo too and external C bindings
  - Helps improve perf of  native backend partial application
  - Built-in uncurying support can be smarter than ppx
    `'a -> int` === `'a ->  int [@bs]`

---

## Wishes from OCaml compiler upstream

- More flexible lexical convention for method name, structual typing
  is useful but limited by syntax, name mangling is not cool

```js
{ "open" : true }
{ "Content-Type" : "text"}
```
---

## Wishes from OCaml compiler upstream

- Polymorpphic variants as string (with immutable string optimization
  coming)
  - Useful in webprogramming, but generated unreadable code
- Lift the char range limitation

===

## Questions

@bobzhang1988
