
## FFI to js functions

This part is similar to [traditional FFI](http://caml.inria.fr/pub/docs/manual-ocaml-4.02/intfc.html), 
syntax is described as below:

```ocaml
external value-name :  typexpr =  external-declaration  attributes
external-declaration :=	 string-literal  
```

Users need declare types of the foreign function (JS function here) and 
gives it a type and customized attributes

### attributes

* `bs.call`

  example 

  ```ocaml
  external imul : int -> int -> int = "Math.imul" [@@bs.call]
  ```
  Note that if you want to make a single ffi for both c functions and js functions, you can 
  give js foreign function different name

  ```ocaml
  external imul : int -> int -> int = "c_imul" [@@bs.call "Math.imul"]
  ```

* `bs.new`

  This attribute is to help user create a js object
  example:

  ```ocaml
  external create_date : unit -> t = "Date" [@@bs.new]
  ```
* `bs.send`
  
  This attribute is to help user send a message to js object

  ```ocaml
  external getById : dom -> string -> id = "getElementById" [@@bs.send]
  ```
  
  The object is always the first argument and arguments follow.

  ```ocaml
  getElementById dom "xx"
  ```
  will be compiled as 

  ```js
  dom.getElementById("xx")
  ```
* `bs.get`, `bs.set`
  This attribute help get and set the property of an js object.

  ```ocaml
  type textarea
  external set_name : textarea -> string -> unit = "name" [@@bs.set]
  external get_name : textarea -> string = "name" [@@bs.get]
  ```
* `bs.set_index` `bs.get_index`

  This attribute help dynamic access js property

  ```ocaml
  module Int32Array = struct
    type t
    external create : int -> t = "Int32Array" [@@bs.new]
    external get : t -> int -> int = "" [@@bs.get_index]
    external set : t -> int -> int -> unit = "" [@@bs.set_index]
  end
  ```

* `bs.obj`

  This attribute helps create js object literal

  A simple example:

  ```ocaml
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

  will generate

  ```js
  let u = {hi : 3}
  let v = {hi : 3 , lo: 2}
  ```

* `bs.value` 

   Bind to a js value

   ``` ocaml
   external v : int = "js_values" [@@bs.val]
  ```

* `bs.module`

   Qualify the js value by a module name

   ```ocaml
   external add : int -> int -> int = "add" [@@bs.call] [@@bs.module "x"]
   let f = add 3 4
   ```
   will be compiled as 

   ```ocaml
   var X = require("x")
   var f = X.add(3,4)
   ```

   ```ocaml
   external add : int -> int -> int = "add" [@@bs.call] [@@bs.module "x" "U"]
   let f = add 3 4
   ```

   will be compiled as

   ```ocaml
   var U = require("x")
   var f = U.add(3,4)
   ```

* `bs.scope`

   Similar to `bs.module` but does not introduce module dependency

   ```ocaml
   external dom : document = "document" [@@bs.val] [@@bs.scope "window"]
   let f = dom
   ```

    will be compiled as 

    ```js
    var f = window.document
    ```

## A simple example: binding to mocha unit test library

   If we want to provide bindings to the [mochajs](https://mochabs.org/) unittest framework, 
   below is an example

   ```ocaml
   external describe : string -> (unit -> unit) -> unit = "describe" [@@bs.call]
   external it : string -> (unit -> unit) -> unit = "it" [@@bs.call "it"]
   ```

   Since, `mochajs` is a test framework, we also need some assertion
   test, we can also describe the bindings to `assert.deepEqual` from
   nodejs `assert` library:

   ```ocaml
   external eq : 'a -> 'a -> unit = "deepEqual" 
    [@@bs.call]
    [@@bs.module "assert"]
   ```

On top of this we can write normal ocaml functions, for example:

   ```ocaml
   let assert_equal = eq
   let from_suites name suite  = 
       describe name (fun _ -> 
         List.iter (fun (name, code) -> it name code) suite)
   ```

   The compiler would generate code as below:

   ```ocaml
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

Note that OCaml has its own support for object, inheritance. 
Its method dispatching is different from how it works in JS.

We support both OCaml style objects and JS style objects. So when `obj # method`, 
we will first tell if `obj` is an OCaml object or JS object, 
(this can be partly done at compile-time, if not, it will be delayed at runtime). 
There is a limiation when writing bindings to JS object, we can declare its type 
and since OCaml support structural typing, it works exactly the same way, the limitation 
lies in that user can not `inherit`, `extend` JS object in the OCaml language. 
We encourage user write bindings to JS object api and create a functional interface on top of it.

### declare class types for JS objects

Note that since OCaml's a language which supporting currying by default, and when we write 
bindings to JS object, we can not see the actual JS source, so that the compiler can not 
inference the `arity` of js functions (just by type signatures, we can not tell the arity at all).

Also determining that `obj` in  `obj # meth` is JS object or OCaml object during run time 
is efficient.

Last but not least, for ocaml object typing, there is no difference between `propery` and `method` 
which is not the case in JS.

Currently, we use a naming convention in method name to encoding such information, we will 
first explain the naming convention and later introduce ppx extension to help simplify it.

Rule 1. 
- Method names without `__` are considered **normal names**, in such objects method call,
  we have to determine it's an OCaml object and JS object in the run-time, and it can 
  only be a method, it **can not be a js property**
  
Rule 2.
- Method names with `__`, the first part are considered normal name, while the 
  remaining parts are considered indicators, split by `_`. For example, 
  in `getElementById__1_unsafe`, `getElementById` are considered as normal name, 
  `["1", "unsafe"]` are indicators, they tell the compiler that its arity is `1`, 
  `unsafe` is not actually used by the compiler, but it is used for polymorphism 
  so that, we can encode two APIs while compiled to the same JS method call.
  
  ```ocaml
  class type dom = object
    method getElementById__1 : name -> htmlElement opt (* Typesafe *)
    method getElementById__1_unsafe : name -> htmlElement 
  end
  ```
  
  There are two other indicators `js` , `ml` which indicate whether it's `js` object
   or `ml` object. 
  Note that by default, it's JS object, unless you have explicit indicator `ml` 

Rule 3.    
- Names suffixed with `__r` is a property read, while names suffixed with `__w` 
  is a property write.
  
  For example:
  
  ```ocaml
  class type dom = object
    method name__r : string 
    method name__w : string -> unit   
  end
  
  let f (v : dom) = 
     v # name__r |> Js.log ; 
     v # name__w "xx" 
  ```
  
  Below it's the generated JS output
  
  ```js
  function f (v){
      console.log(v.name);
      v.name = "x";
      return 0
  }
  ```
  
Rule 4.

- `index__r`, `index__r_unsafe`, `index_w_unsafe`
   are specially handled as JS index operator (for example, array like objects)

   For example
  
   ```ocaml
   class type float32Array = object
     method index__r_unsafe : int -> float 
     method index__r : int -> float Js.def (* can be out of bound *)
     method index__w : int -> int ->  unit 
   end
   ```
#### ppx to write ocaml bindings to js type (for the definition)



```ocaml
class type node = object
  method nodeName : string [@r]


  (** provide both versions [nodeName__] for better performance,
      since its arity is zero and [string] is not a function, 
      there is no need to tell its arity.
      should have some ppx extension to solve the verbosity
  *)
  method nodeValue : string opt [@r][@unsafe]
  method nodeType : nodeType [@r]
  method parentNode : node  opt [@rw] [@unsafe]
  method childNodes : node nodeList  [@rw]
  method firstChild : node  opt [@unsafe] [@rw]
  method lastChild : node opt [@unsafe] [@rw]
  method previousSibling : node  opt [@unsafe] [rw]
  method nextSibling : node  opt [@unsafe] [@rw]
  method namespaceURI : string opt [@unsafe] [@rw]
  method insertBefore : node  -> node opt -> node
  method replaceChild : node  -> node  -> node 
  method removeChild : node  -> node 
  method appendChild : node -> node
  method hasChildNodes : boolean [@r]
  method cloneNode : boolean -> node
  method compareDocumentPosition : node  -> DocumentPosition.t
  method lookupNamespaceURI : string -> string opt
  method lookupPrefix : string -> string opt [@unsafe]
  method setTest : int -> int
  method setTest : int -> int -> int (* Test polymorphism *)
end [@@bs.class]

``` 

There is no ppx extension to help for the use site.

-           

### create JS objects

We rely on the attribute `bs.new`, for example,

```js
external current_date : unit -> Js_date.t = "Date"
[@@bs.new]
```
