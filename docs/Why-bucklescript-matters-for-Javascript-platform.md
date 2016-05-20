**Disclaimer:** This post is opinionated, please take it with a grain of
salt and make your own judgement call.

### Why should I care about BuckleScript?

BuckleScript is designed for large scale programming under JavaScript
platform.

Its goal is not just running OCaml programs under JavaScript
platform, but also to try to bring all Javascript developer tools to the OCaml
ecosystem, while bringing the type safety and expressiveness of OCaml
languages to the Javascript developer community.

If you are interested in compilation-time cost that existing Javascript
transpilers ([babel](https://babeljs.io/), [buble](http://buble.surge.sh/) or [traceur](https://github.com/google/traceur-compiler)) suffer from or projects like
[typescript](https://www.typescriptlang.org/),
[Elm](http://elm-lang.org/), or [Purescript](http://www.purescript.org/) we think you will probably be interested in BuckleScript as well.

Bucklescript is not a new language. It reuses the existing [OCaml programming language](https://ocaml.org/) while still delivering debug-able and performant JavaScript code. We think it's better to reuse than having to invent a new language.

We think it's important that BuckleScript compiles **much faster** than existing
transpilers, generates **more efficient code** while keeping its Javascript
output **readable**.

When in production mode, BuckleScript will aggressively eliminate dead code
at the module level - more than just [tree-shaking](http://www.2ality.com/2015/12/webpack-tree-shaking.html), it will analyze the program to do a [purity](https://en.wikipedia.org/wiki/Pure_function) analysis and eliminate anything that doesn't have side-effects. On the global level, it produces code that is readily optimized by linker/bundlers such as the [Google Closure Compiler](https://developers.google.com/closure/compiler/).

Unlike most transpiled languages, BuckleScript generates **much smaller
code** than hand-written Javascript, eliminating the need to split your
libraries into small pieces like as done in [lodash](https://github.com/lodash/lodash) - we think this should be the compiler and linker's job.

### How does it compare with other transpilers?

1. It's OCaml, and we think Ocaml is awesome. The OCaml language is actually, **really close** to JavaScript after [type-erasure](https://en.wikipedia.org/wiki/Type_erasure). Take [purescript](http://www.purescript.org/) which is designed to generate readable Javascript for example, it's essentially [Haskell](https://www.haskell.org/) with strict evaluation semantics. If we just use OCaml instead, we don't need invent a new language, since it already offers many of the benefits of Haskell, and has a strict evaluation order by default (one of the key differences from Haskell). Moreover, as alluded to, by its full name -- Objective Caml, OCaml provides built-in support for Object Oriented programming. OCaml's [structual typing](https://en.wikipedia.org/wiki/Structural_type_system) and   [row-polymorphism](https://www.cl.cam.ac.uk/teaching/1415/L28/rows.pdf) captures the flexibility of Javascript programs very well -- structural typing is a formalized way of duck-typing. As opposed to Haskell, OCaml does not have an (in our opinion) abstruse way of [supporting side-effects](http://stackoverflow.com/questions/2488646/why-are-side-effects-modeled-as-monads-in-haskell), in fact, while it encourages functional programming in general, it natively supports straightforward imperative programming and Object-Oriented programming when you really need it. As a practical programmers, we find this to be a big deal: While we prefer programming in a functional style in general, we realize there are some cases that an imperative style makes perfect sense.

2. Bucklescript is not just OCaml, but the whole OCaml ecosystem

   The OCaml compiler implementation(s) are famous for blazing fast compile times, and as programmer, we think compile times really matter and are a very important feature of a compiled language.

    ![./dist/images/compile-time.png](./dist/images/compile-time.png)

   The above picture compares OCaml compilation-times with
   other languages: S for Scala, K for Koltin, TS for TypeScript.

   Bucklescript reuses most of the OCaml compiler (parsing/type
   checking and pattern match compilation), its type checker
   [was well engineered](http://okmij.org/ftp/ML/generalization.html)
   and written in an imperative style to make it fast.

   Before BuckleScript, there already existed an
   [OCaml to JS compiler](./Differences-from-js_of_ocaml.md). BuckleScript
   stands on the shoulders of such excellent tools. For example, from the very beginning, much like Js_of_ocaml,
   BuckleScript has an assembly backend, which we use for the best performance in our development
   environment and our
   [playground](http://bloomberg.github.io/bucklescript/js-demo/) to
   deliver the software to more people without installation - compared with Elm, Purescript, or typescript, none of which support both an ASM backend and a JS backend.

3. Bucklescript is an optimizing compiler and will do much more in the future.

   Since OCaml has a sound type system, and functional languages are
   generally easier to optimize, unlike typescript, we've already done lots of
   optimizations for efficient Javascript output, and envision much more in the future.


Here's a naive example to demonstrate the capabilities of BuckleScript:

The following OCaml code:
```OCaml
let test () =
  let m = ref IntMap.empty in
  let count = 1000000 in
  for i = 0 to count do
    m := IntMap.add i i !m
  done;
  for i = 0 to count  do
    ignore (IntMap.find i !m)
  done

let () = test()
```


Generates the following output code:

```js
function test() {
  var m = /* Empty */0;
  for(var i = 0; i<= 1000000; ++i){
    m = add(i, i, m);
  }
  for(var i$1 = 0; i$1<= 1000000; ++i$1){
    find(i$1, m);
  }
  return /* () */0;
}

test(/* () */0);

```

We re-wrote the example using using Facebook's
[immutable](http://facebook.github.io/immutable-js/) library for JavaScript

```js
'use strict';
var Immutable = require('immutable');
var Map = Immutable.Map;
var m = new Map();
var test = function() {
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

Now we compare the runtime performance:

- BuckleScript Immutable Map: 1186ms
- Facebook Immutable Map: 3415ms

We also compare code Size:

- BuckleScript (Prod mode): 899 Bytes
- Facebook Immutable: 55.3K Bytes

### How is it faster than hand-written JS code?

It's hard to explain JS performances in general, however, we will
explain some of the optimizations we've done. For example, in OCaml, `Map`
is like C++ templates, i.e. it will be instantiated by a comparison
function, like below:

```ocaml
module IntMap = Map.Make(struct
  type t = int
  let compare (x : int) y = compare x y
  end
)
```

In BuckleScript, this instantiation happens at compile time instead of
at runtime, thereby improving the performance. Moreover, the data representation or encoding of types is also optimized for speed. Consider the following example:

```OCaml
(** Map.t *)
type 'a t =
  | Empty
  | Node of 'a t * key * 'a * 'a t * int
```

Since there are only two branches in this
[algebraic data type](https://en.wikipedia.org/wiki/Algebraic_data_type),
`Empty` will be simply encoded as `0 /* Empty */` (with comments for readable
output) and `Node(l, x, data, r, height) ` will be encoded as
`/*Node*/ [l,x,data,r,height]`. Arrays are [faster](http://stackoverflow.com/questions/17295056/array-vs-object-efficiency-in-javascript) for most operations when compared to objects.

Other optimizations like [tail call conversion](http://stackoverflow.com/questions/310974/what-is-tail-call-optimization), inline and [constant
propagation](https://en.wikipedia.org/wiki/Constant_folding) are also introduced.

## How does it reduce generated code size?

We do three levels of dead code elimination. At the module and function levels,
BuckleScript will try to find dead code and remove unused values and side-effect free expressions (local variables and functions). At
the linker/bundler level, BuckleScript can make use of existing bundlers and even Google Closure Compiler or any existing bundler
to do the global analysis, because we produce code that conforms to the standard.

Even bucklescript runtime is written in OCaml itself, this means such
runtime can also benefit from dead code elimination.

There are other factors count, mostly code organized in functional
style are much easier to do the tree shaking while harder to remove
for OO style, OCaml's standard library and runtime are organized in an
independent style, which also helps dead code removal.
