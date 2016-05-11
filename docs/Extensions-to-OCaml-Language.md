BuckleScript leverages the OCaml support for extension with dedicated builtin extensions. Those BuckleScript extensions facilitates the integration of native JavaScript code as well as improve the generated code. 

> Note that all those extension will be correctly ignored by the native OCaml compiler.

1. extension `bs.raw`
  
   It can be either `[%bs.raw{|  this_is_arbitrary_js_expression |}]` or `[%%bs.raw{| this is arbitrary_js_statement |}`
   
   Use cases:
   for example if you want to use a JavaScript string, you can write code like this
   
   ```OCaml
   let x  : string = [%bs.raw{|"\x01\x02"|}]
   ```

   which will be compiled into 

   ```js
   var x = "\x01\x02"
   ``` 

   ```OCaml
   [%%bs.raw{|
   // Math.imul polyfill
   if (!Math.imul){
       Math.imul = function (..) {..}
    }
   |}]
   ```
   In the expression level, i.e, `[%js.raw ...]` user can add a type annotation, the compiler would use such type annotation to deduce its arities. for example, the next three versions:

   ```ocaml
   let f = [%bs.raw ("Math.max"  : float -> float -> float) ] 3.0 
   let f : float -> float -> float = [%bs.raw "Math.max" ] 3.0
   let f = ([%bs.raw "Math.max"] : float -> float -> float ) 3.0
   ```
   will be translated into 

   ```js
   function f(prim){
     return Math.max(3.0,prim);
   }
   ```
   Caveat:
   1. So far we don't do any sanity check in the quoted text (syntax check is a long-term goal)
   2. You should not refer symbols in OCaml code, it is not guaranteed that the order is correct.
      You should avoid introducing new symbols in the raw code, if needed, use the `$$` prefix (ie `$$your_func_name`) 

2. extension `bs.debugger`

   It can be `[%bs.debugger]`

   use case

   ```ocaml
   let f x y = 
      [%bs.debugger];
      x + y
   ```

   which will be compiled into 

   ```js
   function f (x,y) {
     debugger; // JavaScript developer tools will set an breakpoint and stop here
     x + y;
   }
   ```