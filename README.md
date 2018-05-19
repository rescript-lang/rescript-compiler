[BuckleScript](http://bucklescript.github.io/bucklescript/): A JavaScript backend for [OCaml](https://ocaml.org/) focused on smooth integration and clean generated code.

[![NPM](https://nodei.co/npm/bs-platform.png?compact=true)](https://nodei.co/npm/bs-platform/) [![Build Status](https://travis-ci.org/BuckleScript/bucklescript.svg?branch=master)](https://travis-ci.org/bucklescript/bucklescript) [![Coverage Status](https://coveralls.io/repos/github/BuckleScript/bucklescript/badge.svg?branch=master)](https://coveralls.io/github/BuckleScript/bucklescript?branch=master)

## Documentation

Please see the [documentation site](https://bucklescript.github.io).

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## How BuckleScript Works

BuckleScript leverages the existing OCaml compiler and toolchain to produce JavaScript that closely
resembles the original OCaml. This has several immediate advantages:

* name mangling is avoided
* stack traces are preserved
* OCaml modules are mapped to JavaScript modules
* OCaml optimizations (e.g., constant folding, DCE, TCO) are reusable
* OCaml attributes allow fine control over generated JavaScript

These points make the integration of JavaScript with OCaml very clean and smooth. In this way,
BuckleScript provides all the benefits of OCaml's excellent compiler and sophisticated type system
alongside the rich cross-platform JavaScript ecosystem.

## BuckleScript Examples

Basic examples of using BuckleScript are provided below. More extensive examples are available at
https://github.com/bucklescript/bucklescript-addons.

#### An HTTP Server

This example creates a simple http server. The complete code is available
[here](https://github.com/BuckleTypes/bs-examples/tree/master/node-http-server).

The attribute `[@bs]` used in the example below is one of the OCaml attributes mentioned earlier.
When BuckleScript generates code, it may use either a curried (OCaml) or uncurried (JavaScript)
calling convention depending on how the code gets optimized. The `[@bs]` attribute can be used to
decorate functions and call-sites so that generated code is guaranteed to use the uncurried style.
This guarantee eases integration with existing JavaScript code and avoids unnecessary overhead.

##### Input:

```ocaml
let port = 3000
let hostname = "127.0.0.1"
let create_server http =
  let server = http##createServer begin fun [@bs] req resp ->
      resp##statusCode #= 200;
      resp##setHeader "Content-Type" "text/plain";
      resp##_end "Hello world\n"
    end
  in
  server##listen port hostname begin fun [@bs] () ->
    Js.log ("Server running at http://"^ hostname ^ ":" ^ Pervasives.string_of_int port ^ "/")
  end

let () = create_server Http_types.http
```

##### Output:

```js
'use strict';
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

#### Immutable Data Structures

This example demonstrates the use of immutable data structures. The OCaml code uses the BuckleScript
compiled OCaml standard library. The JavaScript code, given as a point of comparison, uses the
Facebook `immutable` library.

This comparison is somewhat contrived but nevertheless the BuckleScript compiled version has several
nice characteristics:

Execution Time:

- BuckleScript: 1186ms
- JavaScript: 3415ms

Compiled Size:

- BuckleScript (production): 899 Bytes
- JavaScript: 55.3K Bytes

##### BuckleScript (OCaml stdlib)

```Ocaml
module IntMap = Map.Make(struct
  type t = int
  let compare (x : int) y = compare x y
end)

let test () =
  let m = ref IntMap.empty in
  let count = 1000000 in
  for i = 0 to count do
    m := IntMap.add i i !m
  done;
  for i = 0 to count do
    ignore (IntMap.find i !m)
  done

let () = test()
```

##### Javascript (facebook `immutable`)

``` js
'use strict';

var Immutable = require('immutable');
var Map = Immutable.Map;
var m = new Map();

function test() {
  var count = 1000000;
  for(var i = 0; i < count; ++i) {
    m = m.set(i, i);
  }
  for(var j = 0; j < count; ++j) {
    m.get(j);
  }
}

test();
```

## Acknowledgements

* Thanks to the  [OCaml](https://ocaml.org) team, obviously, without such a beautiful yet practical language, this backend would not exist
* Thanks to [ninja-build](https://ninja-build.org), BuckleScript also comes with a blazing fast build tool on top of it, `ninja` is a truly [well engineered](http://aosabook.org/en/posa/ninja.html) scalable build tool
* Thanks to [Bloomberg](https://www.techatbloomberg.com)! This project began at Bloomberg and was published in 2016; without the support of Bloomberg, it would not have happened. Now that the project has grown and developed its own community, it has moved to its own GitHub organization.

## Licensing

See [COPYING](./COPYING) and [COPYING.LESSER](./COPYING.LESSER)

The [`ocaml`](vendor/ocaml) directory contains the official [OCaml](https://ocaml.org) compiler (version 4.02.3).
Refer to its copyright and license notices for information about its licensing.

The [`ninja-build`](ninja-build) directory contains the official [ninja-build](https://github.com/ninja-build/ninja) (version 1.7.2).
Refer to its copyright and license notices for information about its licensing.

BuckleScript builds on parts of [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml):

* [`jscomp/core/js_dump.ml`](jscomp/core/js_dump.ml) (pretty printer)
* [`jscomp/runtime`](jscomp/runtime)

BuckleScript builds on parts of OCaml:

* [`jscomp/core/lam_pass_exits.ml`](jscomp/core/lam_pass_exits.ml)
* [`jscomp/core/lam_pass_lets_dce.ml`](jscomp/core/lam_pass_lets_dce.ml)

These modules were adapted from [`ocaml/bytecomp/simplif.ml`](vendor/ocaml/bytecomp/simplif.ml) for
JavaScript specific optimization purposes.

* [`jscomp/core/js_main.ml`](jscomp/core/js_main.ml)

`jscomp/core/js_main.ml` is adapted from [`ocaml/driver/main.ml`](vendor/ocaml/driver/main.ml). It is not
actively used but demonstrates that it is easy to assemble a whole compiler using the OCaml compiler
libraries. It also shows how to add more compilation flags to a JS backend.

* [`jscomp/stdlib`](jscomp/stdlib)

`jscomp/stdlib` is copied from [`ocaml/stdlib`](vendor/ocaml/stdlib). It is compiled to JavaScript and
included with BuckleScript.

* [`jscomp/test`](jscomp/test)

`jscomp/test` is based on [`ocaml/testsuite`](vendor/ocaml/testsuite).

BuckleScript unittest builds on parts of [OUnit](http://ounit.forge.ocamlcore.org/)

* [`jscomp/ounit`](jscomp/ounit) is adapted from ounit, the unit test
  utilities are only used for dev purpose, they are not required for distribution
