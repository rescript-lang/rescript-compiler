This section describes the semantic difference between BuckleScript generated code and other backends. This is particularly important when porting an existing application to JavaScript.

# Custom data type

   In OCaml, the C FFI allows the user to  define a custom data type and customize `caml_compare`, `caml_hash` behavior, etc.   This is not available in our backend (since we have no C FFI).

# Physical (in)equality

   In general, you should only use physical equality as an optimization technique, but not rely on its correctness, since it is tightly coupled with the runtime.

# String char range
  
   Currently, BuckleScript assumes that the char range is `0-255`. The user should be careful when they pass a JavaScript string to OCaml side. Note that we are working on a solution for this problem.

# Weak map   

  OCaml's weak map is not available in BuckleScript. The weak pointer is replaced by a strict pointer.

# Integers

OCaml has `int`, `int32`, `nativeint` and `int64` types. Both `int32` and `int64` have the exact same semantic.

- `int`
`int` in BuckleScript is the same as `int32` where it's platform dependent.

- `nativeint`
Treated as JavaScript float, except for division. For example, `Nativeint.div a b` will be translated into ` a /b | 0`. 

>Note there is a subtle difference in that `Nativeint.shift_right_logical x 0` is different from `Int32.shift_right_local x 0`. The former is literally translated into `x >>> 0` (translated into an unsigned int), while the latter is ` x | 0`.

- `Printf.printf`
The `Printf.print` implementation in BuckleScript requires a newline (`\n`) to trigger the printing. This behavior is not consistent with the buffered behavior of native OCaml. The only potential problem we foresee is that if the program terminates with no newline character, the text will never be printed.
 
- `Obj` module
We do our best to mimic the native compiler, but we have no guarantee and there are differences.

- `Hashtbl`
BuckleScript uses the same algorithm as native OCaml but the output is different due to runtime representation of int/int64/int32 and float.

- `Marshall`
Marshall module is not supported.

- `Sys.argv`
Command line arguments are always empty.
