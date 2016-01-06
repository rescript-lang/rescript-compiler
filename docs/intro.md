# Why JavaScript Platform

Javascript is not just the *only* langauge in the browser, but also
the *only* cross platform langauge in town, it is truly
everywhere: users don't even need install any binaries to access your
software, just a simple link.

Another important factor is that the Javascript VM is quite fast and
will become enven faster, so that it is possible to deliver quite
large applications purly in Javascript platform.

# Why OCamlScript

OCamlScript is mainly designed to solve the problem of large scale
Javascript programming:

1. Industrial strength type system while not verbose typing required
   (thanks to OCaml's strong type system).

2. Dead code eliminatoin in different levels (inside functons, outside
   functions and across modules).

3. Offline optimizations. Javascript is a dynamic language, it takes
   time to optimize it during runtime, it's better to optimize it at
   compile time if we can.

At the same time we try to avoid the cost of compiling a langague to
Javascript as much as we can:

1. Very fast compilation, OCaml bytecode compilation is
   famous for fast compilation, in general one or two order faster
   than other similar langauges: [Scala](http://www.scala-lang.org/)
   or [Haskell](https://www.haskell.org/), our compiler shares the same
   property with OCaml's bytecode compiler

2. Easy interaction, no name mangling.

3. Separate compilatio. We map one OCaml module into one JS module



# Resources for Learning OCaml

The offical [ocaml](https://ocaml.org/) website has a comprehensive
list of tutorials about 
