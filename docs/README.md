
**BuckleScript Basics**

* [FAQ](./FAQ.md)

* [Compiler options](./Compiler-options.md)

* [How to adapt your build system](./How-to-adapt-your-build-system.md)

* [Semantic differences from other backends](./Semantic-differences-from-other-backends.md)


# Why JavaScript?

JavaScript is not just _*the*_ browser language, it's also
the **only** existing cross platform language. It is truly
everywhere: users don't need to install binaries or use package managers to access software, just a link will work.

Another important factor is that the JavaScript VM is quite fast and keeps getting faster. 
The JavaScript platform is therefore increasingly capable of supporting large applications.

# Why [BuckleScript](https://github.com/bloomberg/bucklescript)?

BuckleScript is mainly designed to solve the problems of large scale JavaScript programming:

1. **Lack of type-safety:** [OCaml]((https://ocaml.org/) offers an industrial-strength state-of-the-art type system and provides type inference (i.e. No verbose type annotation required), which proves [invaluable](http://programmers.stackexchange.com/questions/215482/what-are-the-safety-benefits-of-a-type-system) in managing large projects.

2. **Dead code:** A large amount of web-development relies on inclusion of code dependencies by copying or referencing CDNs (the very thing that makes JavaScript highly accessible), but this also introduces a lot of [dead code](https://en.wikipedia.org/wiki/Dead_code). This impacts performance adversely when the JavaScript VM has to interpret code that will never be invoked. BuckleScript provides powerful dead-code elimination at all levels. Function and module level elimination is facilitated by the sophistication of the type-system of OCaml, and at the global level BuckleScript generates code ready for dead-code elimination done by bundling tools such as the [Google closure-compiler](https://developers.google.com/closure/compiler/).

3. **Lack of offline optimizations:** JavaScript is a dynamic language, it takes a performance-hit for the VM to optimize code at runtime. While some JS engines circumvent the problem to some extent by [caching](http://v8project.blogspot.com/2015/07/code-caching.html), this is not available to all environments, and lack of a strong type system also limits the level of optimizations possible. Again, BuckleScript, using features of the OCaml type-system and compiler implementation is able to provide many optimizations during offline compilation, allowing the runtime code to be extremely fast.

While a strong type-system helps in countering these problems, at the same time we hope to avoid some of the problems faced in using other offline [transpilation](https://github.com/jashkenas/coffeescript/wiki/list-of-languages-that-compile-to-js) systems:

1. **Slow compilation:**
   OCaml bytecode compilation is known to be fast (one or two orders of magnitude faster
   than other similar langauges: [Scala](http://www.scala-lang.org/)
   or [Haskell](https://www.haskell.org/)), BuckleScript shares the same
   property. See the speeds at work in the [playground](http://bloomberg.github.io/bucklescript/js-demo/).

2. **Un-readable JS Code:**
   In compiling to JavaScript, many systems generate code, that while syntactically and semantically correct is
   not human-readable and very difficult to debug. Our BuckleScript implementation and the multi-pass compilation
   strategy of OCaml, allows us to avoid [name-mangling](https://en.wikipedia.org/wiki/Name_mangling), and produce JavaScript code that is human-readable and easier to debug and maintain.

3. **Loss of code-structure:**
   Many systems generate JavaScript code that is essentially a [big ball of mud](https://en.wikipedia.org/wiki/Big_ball_of_mud). We try to keep the original structure of the code by mapping one OCaml module to one JS module.

# Resources for Learning OCaml

The offical [ocaml](https://ocaml.org/) website has a comprehensive list of tutorials about the OCaml language, targeted at both developers new to functional programming and type-systems, and developers already familiar with these concepts.
