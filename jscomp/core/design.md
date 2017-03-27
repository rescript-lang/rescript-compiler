
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

**ATT** if you want to support JS boolean better, think twice, it is
really hard to give two booleans first class support, and such bugs
are very hard to find (since in most cases they behave the same), so
what we can do is produce OCaml bool exclusively, only pass JS boolean
to JS ffi which requires it exclusively (very rare)
**NOTE** since OCaml boolean is everywhere, while JS boolean only happens in the FFI, we should by default produce OCaml boolean in the IR, and mark JS boolean explicitly instead.

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

# Global exports

Global exports identifiers are extracted from {!Translmod.get_export_identifiers} 
instead of inferred from lambda expression or cmi file.

1. We need be careful about externals.
2. Reading from fresh generated cmi is expensive.

# variable usage

Lalias-bound variables are never assigned, so it can only 
appear in `Lvar`, then it is easy to eliminate it 


# externals beta reduction

Note in general, it is fine whether we do beta reduction or not, it is just optimization. 

However, since we introduced `bs.splice` which does require the `spliced argument` to be captured

```ocaml
spliced_external a0 a1 [|b0;b1|]
```

There are two cases where get things complicated, people don't think `|>` is a function

```ocaml
x |> spliced_external a0 a1 [|b0;b1|]
```
Even though `|>` is a function, and `spliced_external` is escaped here, but people would 
expect it is equivalent to 

```ocaml
spliced_external a0 a1 [|b0;b1|] x 
```

So our optimizer needs to handle this case to make sure `spliced_external` not escaped, 
also becaues the interaction of `[@bs.splice]` and `[@bs.send]`, the spliced argument
is no longer  in tail position, so that people can write such code

```ocaml
spliced_external a0 a1 [|b0;b1|]
```
Internally in lambda layer it would be 

```ocaml
(fun c0 c1 c2 c3-> spliced_external c0 c1 c2 c3) a0 a1 [|b0;b1|]
```

We can simply do inlining, it may have side efffect in `b0`, `b1`, our optimizer also need handle such case.

Maybe in the future, we should lift the restriction about `bs.splice` (delegate to `slow` mode when we can not resolve it statically, my personal expereince is that people will complain about why it fails to compile more than why it is slow in some corner cases)

Note this also interacts with `[@bs.uncurry]`

for example

```ocaml
external filter : ('a -> bool [@bs.uncurry]) -> 'a array = "" [@@bs.send.pipe: 'a array]

let f xs =
    xs |> filter (fun x -> x > 2)
```

Here whether the callback gets inlined to the call of `filter` will have an effect on how `Pjs_fn_make` gets cancelled.

Note when we pattern match over the original lamba,`Levent` needs to be removed as early as possible. Due to the existence of `Levent`, we can not pattern match over nested original raw lambda.

We turned off event generation temporarily

# test undefined

Note such logic is already wrong:

```js
var x = undefined_value // thrown here 
if (typeof x === "undefined"){
    ...
}
```


# primitive handling

1. Some primitives introduced are for performance reasons, for example:

`#String.fromCharCode` which is essentialy the same as 

```ocaml
external of_char : char -> string = "String.fromCharCode"
[@@bs.val]
```

We introduced `#` so that we can do some optimizations.

2. Some of them are not expressible in OCaml FFI, for example
'#is_instance_array',
'#gt'

3. Some of them require a runtime polyfill support


# runtime

## anything to string
http://www.2ality.com/2012/03/converting-to-string.html
Note that `""+ Symbol()` does not work any more, we should favor `String` instead


# name mangling

## let bound identifier mangling

### keyword
Note there are two issues, if it is keyword, the output may not be parsable if we don't do name mangling


```js
> var case = 3
< SyntaxError: Cannot use the keyword 'case' as a variable name.
```
### global variable 
If it is global variable, it is parsable, it may trigger even subtle errors:

```js
(function(){ 'use strict'; var document = 3; console.log(document)})()
VM1146:1 3
3
```  
This could be problematic for bindings
```ocaml
let process = 3
Process.env##OCAML
```
In general global variables would be problematic for bindings

## property name mangling

Nowadays, JS engine support keywords as property name very well

```js
var f = { true : true, false : false }
```

But it has problems when it is too simple for parsing
```js
var f = { true, false} // parsign rules ambiguity
```

If we don't do ES6, we should not go with name mangling, however, it is mostly due to we can 
not express these keywords, such as `_open` as property in OCaml, so we did the name mangling

# function kind

OCaml indeed support two kind calling convention.

```ocaml
(Lfunction (Tupled(a0,a1,a2))) [a0,a1,a2]
```

and 

```ocaml
(Lfunction Curried (a0,a1,a2)) a0 a1 a2 
```


They also affect how beta reduction works, 

```ocaml
| Lapply(Lfunction(Curried, params, body), args, _)
    when optimize && List.length params = List.length args ->
      count bv (beta_reduce params body args)
| Lapply(Lfunction(Tupled, params, body), [Lprim(Pmakeblock _, args,_)], _)
    when optimize && List.length params = List.length args ->
      count bv (beta_reduce params body args)
```

It is generated by the backend via an argument 
`untuplify_fn` in `transl_funciton`.
Note if we want to take advantage of it in the future we need 
translate ` f x`  into (when `f` is `fun (a0,a1) -> a0 + a1 `

```ocaml
f (x[0],x[1])
```

currently it is turned on only in native mode

```ocaml
and transl_function loc untuplify_fn ..
```

two call sites

```ocaml
transl_function exp.exp_loc false ...
transl_function e.exp_loc !Clflags.native_code ...
```