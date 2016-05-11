
## FFI to js functions

This part is similar to [traditional FFI](http://caml.inria.fr/pub/docs/manual-ocaml-4.02/intfc.html), 
syntax is described as below:

```OCaml
external value-name :  typexpr =  external-declaration  attributes
external-declaration :=	 string-literal  
```

Users need declare types of the foreign function (JS function here) and 
gives it a type and customized attributes

### attributes

* `bs.call`

  Example 

  ```ocaml
  external imul : int -> int -> int = "Math.imul" [@@bs.call]
  ```
>Note that if you want to make a single FFI for both c functions and JavaScript functions, you can 
 give JavaScript foreign function different name

  ```ocaml
  external imul : int -> int -> int = "c_imul" [@@bs.call "Math.imul"]
  ```

* `bs.new`

  This attribute is to help user create a JavaScript object
  example:

  ```ocaml
  external create_date : unit -> t = "Date" [@@bs.new]
  ```

* `bs.val` 

   Bind to a JavaScript value

   ```OCaml
   type dom 
      (* Abstract type for the DOM *)
   
   external dom : dome = "document" [@@bs.val]
  ```

* `bs.send`
  
  This attribute is to help user send a message to js object

  ```OCaml
  type id 
    (** Abstract type for id object *)
  external get_by_id : dom -> string -> id = "getElementById" [@@bs.send]
  ```
  
  The object is always the first argument and arguments follow.

  ```OCaml
  getElementById dom "xx"
  ```
  will be compiled as 
  ```js
  dom.getElementById("xx")
  ```

* `bs.get`, `bs.set`
  This attribute help get and set the property of a JavaScript object.

  ```OCaml
  type textarea
  external set_name : textarea -> string -> unit = "name" [@@bs.set]
  external get_name : textarea -> string = "name" [@@bs.get]
  ```

* `bs.set_index` `bs.get_index`

  This attribute help dynamic access to JavaScript property

  ```OCaml
  module Int32Array = struct
    type t
    external create : int -> t = "Int32Array" [@@bs.new]
    external get : t -> int -> int = "" [@@bs.get_index]
    external set : t -> int -> int -> unit = "" [@@bs.set_index]
  end
  ```

* `bs.obj`

  This attribute helps create JavaScript object literal

  A simple example:

  ```OCaml
  external make_config : hi:int -> lo:int -> unit -> t [@@bs.obj]
  let v = make_config ~hi:2 ~lo:3
  ```
  will be compiled as 

  ```js
  let v = { hi:2, lo:3}
  ```
  You can use optional as well

  ```ocaml
  external make_config : hi:int -> ?lo:int -> unit -> t = "" [@@bs.obj]
  let u = make_config ~hi:3 ()
  let v = make_config ~hi:3 ~lo:2 ()
  ```
  Will generate
  ```js
  let u = {hi : 3}
  let v = {hi : 3 , lo: 2}
  ```

* `bs.module`

   Qualify the JavaScript value by a module name

   ```OCaml
   external add : int -> int -> int = "add" [@@bs.call] [@@bs.module "x"]
   let f = add 3 4
   ```
   will be compiled as 

   ```js
   var X = require("x")
   var f = X.add(3,4)
   ```

   ```OCaml
   external add : int -> int -> int = "add" [@@bs.call] [@@bs.module "x" "U"]
   let f = add 3 4
   ```
   Will be compiled as

   ```js
   var U = require("x")
   var f = U.add(3,4)
   ```


## A simple example: binding to mocha unit test library

   If we want to provide bindings to the [mochajs](https://mochajs.org/) unit test framework, 
   below is an example

   ```OCaml
   external describe : string -> (unit -> unit) -> unit = "describe" [@@bs.call]
   external it : string -> (unit -> unit) -> unit = "it" [@@bs.call "it"]
   ```

   Since, `mochajs` is a test framework, we also need some assertion
   test, we can also describe the bindings to `assert.deepEqual` from
   nodejs `assert` library:

   ```ocaml
   external eq : 'a -> 'a -> unit = "deepEqual"  [@@bs.call] [@@bs.module "assert"]
   ```

On top of this we can write normal OCaml functions, for example:

   ```OCaml
   let assert_equal = eq
   let from_suites name suite  = 
       describe name (fun _ -> 
         List.iter (fun (name, code) -> it name code) suite)
   ```

   The compiler would generate code as below:

   ```js
   var Assert = require("assert");
   var List = require("../stdlib/list");

   function assert_equal(prim, prim$1) {
     return Assert.deepEqual(prim, prim$1);
   }

   function from_suites(name, suite) {
      return describe(name, function () {
              return List.iter(function (param) {
                          return it(param[0], param[1]);
                        }, suite);
            });
    }
   ```



## FFI to objects *experimental*

Note we will support using OCaml style's objects for FFI in the next release
