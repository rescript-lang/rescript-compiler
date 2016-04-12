Bucklescript baked in some extension supports, for example OCaml itself supports attributes like `[@@ocaml.warning "-3"]`, bucklescript added a few, the reason is that some of them does need support from compiler, and it helps simplify the build system compared with an external ppx.

1. extension `js.raw`
  
   It can be either `[%js.raw{|  this_is_arbitrary_js_expression |}]` or `[%%js.raw{| this is arbitrary_js_statement |}`
   
   Use cases:
   for example if you want to use js raw string, you can write code like this
   
   ```ocaml
   let x  : string = [%js.raw{|"\x01\x02"|}]
   ```

   which will be compiled into 

   ```js
   var x = "\x01\x02"
   ``` 

   ```ocaml
   [%%js.raw{|
   // Math.imul polyfill
   if (!Math.imul){
       Math.imul = function (..) {..}
    }
   |}]
   ```
   In the expression level, i.e, `[%js.raw ...]` user can add a type annotation, the compiler would use such type annotation
   to deduce its arities. for example, the next three versions:

   ```ocaml
   let f = [%js.raw ("Math.max"  : float -> float -> float) ] 3.0 
   let f : float -> float -> float = [%js.raw "Math.max" ] 3.0
   let f = ([%js.raw "Math.max"] : float -> float -> float ) 3.0
   ```
   will be translated into 

   ```js
   function f(prim){
     return Math.max(3.0,prim);
   }
   ```
   Caveat:
   1. So far we don't do any sanity check in the quoted text (syntax check is our long-term goal)
   2. You should not refer symbols in OCaml code, it is not guaranteed that the order is correct.
      You should avoid introduce new symbols in those raw code, if needed, `$$your_func_name` prefix is needed

2. extension `js.debug`

   It can be `[%js.debug]`

   use case

   ```ocaml
   let f x y = 
      [%js.debug];
      x + y
   ```

   which will be compiled into 

   ```js
   function f (x,y) {
     debugger; // js developer tools will set an breakpoint and stop here
     x + y;
   }
   ```
    
