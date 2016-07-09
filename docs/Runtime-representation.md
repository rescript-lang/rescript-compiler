
Below is a description of how OCaml values are encoded in JavaScript, 
the **internal** description means **users should not rely on its actual
encoding (and it is subject to change)**. We recommend that you write setter/getter functions 
to manipulate safely OCaml values from JavaScript. 

For example, users should not rely on how OCaml `list` is encoded in JavaScript; instead,
the OCaml stdlib provides three functions: `List.cons`, `List.hd` and `List.tl`. JavaScript
code should only rely on those three functions.

# OCaml type

## int
  - JavaScript type : number
  
## nativeint
  - JavaScript type : number

## int32
  - JavaScript type : number

## int64
  - JavaScript type : an array of two numbers
   
## float
  - JavaScript type : number
    
## bool
  - true  -> 1 
  - false -> 0
  
## char
  - JavaScript type : number
  for example, `'a'` is encoded as its ascii code `97`

## string
  - JavaScript type : string
        
## bytes         
  - JavaScript type : Number Array 
    
> TODO: investigate if we can encode it as buffer when in NodeJS.
    
## array         
  - JavaScript type : Array. 

## record *internal*
  - javascript type: object
  For instance:
  ```ocaml
  type t = { x : int; y : int }
  let v = {x = 1;  y = 2}
  ```
  `v` is encoded as `[1,2]

## tuple
  - JavaScript type: Array
  for example, `(a,b)` is encoded as `[a,b]`

## `'a option` *internal*
  - `None` ->  `0`
  - `Some a` -> `[a]`
 
## `'a list` *internal*
  - `[]` -> `0`
  - `x::y` -> `[x,y]`
    


## Variant *internal*
  - javascript type: object (for non nullary constructor)
  - javascript type: number (for nullary constructor)
  ```ocaml
    type t = 
         | A
         | B of int * int 
         | C of int * int 
         | D
  ```
  
   Here `A` is encoded as `0`, `D` is encoded as `1`.
   `B (1,2)` is encoded as `[1,2]` with property `tag` which is `0`
   you can imagine it is encoded as
   
   ```js
   Object.defineProperty([1,2], 'tag', { 'value' : 0 })
   ``` 

   `C (1,2)` is encoded as below
   
   ```js
   Object.defineProperty([1,2], 'tag', { 'value' : 1 })
   ```
   
   As we clarified before, the internal representation should not be relied upon, for example, 
   we might change the name of `tag` in the future.
   
   We are working to provide a ppx extension as below:
   
   ```ocaml
     type t = 
        | A
        | B of int * int 
        | C of int * int 
        | D [@@bs.export]
   ```
   So that it will a automatically provide `constructing` and `destructing` function
   
   ```
   val a : t 
   val b : int -> int -> t 
   val c : int -> int -> t 
   val d : int 
   
   val a_of_t : t -> bool
   val d_of_t : t -> bool   
   val b_of_t : t -> (int * int ) Js.opt
   val c_of_t : t -> (int * int ) Js.opt
   ```

## Polymorphic variant *internal*
   - javascript type: number (nullary constructor) 
   - javascript type: object (non-nullary constructor)
   
   ```ocaml
     type t = 
        | `A
        | `B of int * int 
        | `C of int
        | D [@@bb.export]
   ```
   For nullary constructor, ```A``, it is encoded as a hash function, **users should not rely
   on how the hash function works**. 
   For documentation purpose, ``A` is encoded as number `65`
   
   ```B(1,2)`` is encoded as `[66, [1,2] ]`
   
   ```C 3`` is encoded as `[67, 3]`

## exception *internal*

## extension *internal*

## object *internal*


# JavaScript type exposed to OCaml

BuckleScript exposes type safe interfaces to JavaScript native types. Those can be found in the `Js` module. 

## `Js.boolean` 
  - javascript type: boolean   
  - Js.true_ -> true
  - Js.false_ -> false
  We also provide a function `Js.to_bool` to convert `Js.boolean` to OCaml `bool`
  
  ```ocaml
  val to_bool : Js.boolean -> bool 
  ```
  
## `'a Js.opt` 
  - either `'a` or `null`  
  We provide several functions in `Js` module for constructing and destructing:  
  
  ```ocaml
  val from_opt : 'a opt -> 'a option
  val to_opt : 'a -> 'a opt
  val is_nil : 'a opt -> bool
  ```
  It's more efficient than `'a option` since it is unboxed. When the user 
  wants to get the value, he can write code as below:
  
  ```ocaml
  match Js.from_opt x with
  | Some x -> 
      (* non null branch *)
      begin
      (* ... *) 
      end
  | None ->
      (* null *)
      begin
      (* ... *) 
      end
  ```
  Note that `Js.from_opt` is in generally optimized, so that it will 
  not be boxed when converted to `'a option` and destructured immediately.
  
## `'a Js.def` 
  - either `'a'` or `undefined`
    
  We provide several functions in `Js` module for constructing and destructing:  
 
  ```ocaml
  val from_def : 'a def -> 'a option
  val to_def : 'a -> 'a opt
  val is_undef : 'a def -> bool 
  ```  

>Note in the future, we will have a *debug* mode, in which the 
corresponding js encoding will be instrumented with more information using `Object.defineProperty`
