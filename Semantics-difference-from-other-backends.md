
#. Custom data type

   In OCaml, the C FFI allows user to  define a custom data type and customize `caml_compare`, `caml_hash` behavior, etc.   This is not available in our backend (since we have no C FFI)

#. Physical (in)equality

   In general, you should only use physical equality as an optimization technique, but not rely on its correctness, since 
it is hardly coupled with the runtime

#. String char range
  
   Currently, bucklescript assumes that char ranges is `0-255`, user should be careful when they pass js string to ocaml side. Note that we are working on a solution for this problem

#. Weak map   

   OCaml's weak map is not available in bucklescript

#. Integers

OCaml has `int`, `int32`, `nativeint` and `int64`

- `int` `int32`
They are treated the same, and behaves like signed int32 

- `nativeint`
Treated as js float, except for division, for example, `Nativeint.div a b` will be translated into ` a /b | 0`, 
note there is a subtle difference that `Nativeint.shift_right_logical x 0` is different from `Int32.shift_right_local x 0`, the former is literally translated into `x >>> 0` (translated into an unsigned int), while the latter is ` x | 0`

- `int64`
It's like ocaml `int64`, the difference is the performance model, the use of `int64` requires memory allocation


