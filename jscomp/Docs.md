Hello! This is the main directory for ReScript. `jscomp` is just a name that mirrors OCaml's own `bytecomp` and `asmcomp` (bytecode compilation and native compilation logic respectively). For building it, please see [CONTRIBUTING.md](../CONTRIBUTING.md).

## Sub directories

### [stdlib](./stdlib)

A copy of standard library from OCaml distribution(4.02) for fast development,
so that we don't need bootstrap compiler, everytime we deliver a new feature.

- Files copied
  - sources
  - Makefile.shared Compflags .depend Makefile
- Patches
  Most in [Makefile.shared](./stdlib/Makefile.shared)


## [test](./test)

The directory containing unit-test files, some unit tests are copied from OCaml distribution(4.02)

## compiler sourcetree

    - ext (portable)
    - common (portable)
    - bsb 
    - depends (portable)
    - core 
    - bspp
    - outcome_printer
    - stubs  
    - super_errors  
    - syntax 
## tools (deprecatd code)    
## xwatcher (dev tools)
## runtime    
## build_tests    
## bin
## cmd_tests
## ounit
## ounit_tests
## others (belt/stdlib/node bindings)

## uncurry 

```ocaml
f a  b [@bs]
```

```ocaml
(unsafeFullApply ((opaque f) a b ))
```
case 0

```ocaml
type 'a fn0 = {
  _0 : 'a
} 
```
create:
```
{_0 = fun () -> ...}
```
`(unit -> int) fn0` is different from `(unit -> int) fn1`

When `f` is an external, without opaque
```ocaml
apply (Obj.magic f a b) ==> apply (f a b)
```

`fun [@bs] a b -> c ` is the same as `fun [@bs] a -> fun b -> c `
from the view of parsetree



`unit -> int [@bs]` --> `int Js.arity0`

`f () [@bs]` --> `(run0 f)`

`fun [@bs] () -> body` -> `fn_mk0 (fun () -> ...)`

It is special handled due to `fn_mk0`, otherwise it will have arity 1

We can `-open Js.Uncurry` to make names look pretty



# bspack

ocamlopt.opt -I +compiler-libs unix.cmxa ./stubs/ext_basic_hash_stubs.c stubs/bs_hash_stubs.cmx  ocamlcommon.cmxa ext.cmxa common.cmxa depends.cmxa core/bspack_main.cmx -o bspack.dev