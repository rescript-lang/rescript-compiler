# Ideas aobut boolean support
## Why boolean is not transparent when

1. printing

```ocaml
Js.log true
```

2. pattern match

```ocaml
let f x y = 
  match x,y with
  | true, false -> 0
```

3. comparison

```ocaml
if v = true then 
```

## Where JS boolean could be introduced 

JS operatons which could generate JS booleans
- `not`
- Equality comparison

# `and`, `or` is fine
In JS, `and`, `or` is untyped, but it is a superset of OCaml semantics:


```js
x && y 
/* equivalent to  */ 
x ? x : y
```



There is no coersion so `1&&0`, `1&&1`, `0&&1`, `0&&0` are all the same as JS version.
Same for `or`

but `not` is not the same as `!`, `!` will do the conversion to enforce its result is JS boolean  

## It does not affect `if_then_else` compilation

since JS if is more capable, there is no need do any coercion  


# Arity handling

The runtime support is in `Curry` module, we have several functions 

## `Curry.app`

```ocaml
Curry.app f args 
(** [f] is an curried function, [args] are supplied arguments
   if matches then like normal function apply.
   if over-supply, take the first [arity] arguments,
    fully apply and continue [app f' rest]
   if under-supply, 
     create a closure, wait until all arguments ready.
     [Note, we don't necessary 
     need a closure here, we can have some 
     data structures like bytecode]   
 *)
```

```ocaml

Curry.curry_N o a1 a2 .. arity 
(** used by [Curry._N]  *)

Curry._N o a1 a2 .. aN 
(** 
  A fast version of [app f [|a1; a2; ..; aN |]].
  Check the arity of [o]if it hits, just do the application
*)

Curry.__N o 
(**
   Make sure the output of [o] is arity [N]. 
   This is used to convert a curried function [o] into
   uncurried. for example 
   {[ 
     fun [@bs] x y -> f x y 
    ]}
    Another use case:
    {[ 
      external f : ('a -> 'b [@bs.uncurry]) -> unit 

      f g (* The compiler will do such converison internally*)
    ]}
   Guess the arity of [o], if it hit, then return [o]

   Note when we want to target arity 0, in the first case
   {[
     fun [@bs] () -> f ()
   ]} will be compiled as 
   {{
     fun () -> f (0)
   }}
   SO [Curry.__0] will not be triggered
   
   We also have 
   some special logic to when converted to arity 0 
   in external settings
*)

```