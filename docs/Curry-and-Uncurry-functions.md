Note that OCaml's calling convention is curried by default, while JS
does not have native support. Curried and uncurried functions can be
told from type signatures.

For example

```ocaml
val f : int -> string -> int
val f_uncurry : int * string -> int [@uncurry]
```



- How BuckleScript compiles function application

To apply a function, you can do this

```ocaml
f 3 "x"
f_uncurry (3,"x") [@uncurry]
```
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
let f_uncurry = fun [@uncurry] (a, b) -> a + string_of_int b 
```

- When is uncurried function recommended

  - For FFI to JS functions, all object methods are *strongly recommended*
     to type it as uncurried function.

  - When function is passed as a callback

    This is mostly for performance issues, it is hard to optimize in
    such scenario.




