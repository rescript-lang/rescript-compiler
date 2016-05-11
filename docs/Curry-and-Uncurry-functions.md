JavaScript functions are all [uncurried](https://en.wikipedia.org/wiki/Currying), while OCaml functions are curried by default. 

A naive approach would be to Curry every OCaml functions but this has a non trivial cost and makes the generated code harder to read. Therefore BuckleScript will try its best to uncurry its function in both definition and call site.

It is also possible to help BuckleScript perform the uncurrying by using using the runtime provided `Fn` module: 

```OCaml 
let iter_f = Fn.mk1 my_function in 
List.iter iter_f l 
```


