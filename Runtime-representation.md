
Note in the future, we will have a *debug* mode, which means, in such mode, the 
corresponding js encoding will be instrumented with more information using `Object.defineProperty`


Below is a description of how ocaml values are encoded in js, 
the **internal** description means **users should not rely on its actual
encoding (and it is subject to change)**, user should expose constructor
function and destruct function instead.

For example, user should not rely on how `list` is encoded in js, instead,
the ocaml stdlib provide three functions: `List.cons`, `List.hd` and `List.tl`,
js code can only rely on such three functions.

## int
  - javascript type : number
  
## nativeint
  - javascript type : number

## int32
  - javascript type : number

## int64
  - javascript type : an array of two numbers
   
## float
  - javascript type : number
    
## bool
  - true -> 1 
  - false -> 0
  
## char
  - javascript type : number
  for example, `'a'` is encoded as its ascii code `97`

## string
  - javascript type : string
        
## bytes         
  - javascript type : Number Array 
    
  TODO: investigate if we can encode it as buffer when in NodeJS.
    
## array         
  - javascript type : Array

## tuple
  - javascript type: Array
  for example, `(a,b)` is encoded as `[a,b]`

## `'a option` *internal*
  - `None` ->  `0`
  - `Some a` -> `[a]`
 
## `'a list` *internal*
  - `[]` -> `0`
  - `x::y` -> `[x,y]`
    
## `Js.boolean` *provided by bucklescript*
  - javascript type: boolean   
  - Js.true_ -> true
  - Js.false_ -> false
  We also provide a function `Js.to_bool` to convert `Js.boolean` to ocaml `bool`
  
  ```ocaml
  val to_bool : Js.boolean -> bool 
  ```
  
## `'a Js.opt` *provided by bucklescript*
  - either `'a` or `null`  
  We provide several functions in `Js` module for constructing and destructing:  
  
  ```ocaml
  val from_opt : 'a opt -> 'a option
  val to_opt : 'a -> 'a opt
  val is_nil : 'a opt -> bool
  ```
  It's more efficient than `'a option` since it is unboxed, when user 
  want to get the value, he can write code as below
  
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
  
## `'a Js.def` *provided by bucklescript*
  - either `'a'` or `undefined`
    
  We provide several functions in `Js` module for constructing and destructing:  
 
  ```ocaml
  val from_def : 'a def -> 'a option
  val to_def : 'a -> 'a opt
  val is_undef : 'a def -> bool 
  ```  

  
## record *internal*
  - javascript type: object
  
  ```ocaml
  type t = { x : int; y : int }
  let v = {x = 1;  y = 2}
  ```
  `v` is encoded as `[1,2]`
  

## variant *internal*
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
   `B (1,2)` is encoded as `[1,2]` with instrucmented property `tag` which is `0`
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
   
   We provide a ppx extension as below:
   
   ```ocaml
     type t = 
        | A
        | B of int * int 
        | C of int * int 
        | D [@@bb.export]
   ```
   So that it will provide `constructing` and `destructing` function
   
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
   TODO: check with Rahul 
   
## polymorphic variant *internal*
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
   on how the hash function work**. 
   For documentation purpose, ``A` is encoded as number `65`
   
   ```B(1,2)`` is encoded as `[66, [1,2] ]`
   
   ```C 3`` is encoded as `[67, 3]` 
   
## exception *internal*

## extension *internal*

## object *internal*

TODO


