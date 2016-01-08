# Interaction with Javascript/Typescript

Note this section is still in design space, it's subject to change in
the future, we need to further document how it works in general.

## Call OCaml functions from Javascript/Typescript

Since ocamlscript guarantees all ocaml functions are exported as it
is, there is not too much work needed.

Some things need be taken care of:

1. `external` exports are not exported as js functions, if you really
   want to export those external functions, please write `val` instead. 

2. `operators` are escaped, since Javascript does not support user
   defined operators, for example instead of calling `Pervasives.(^)`,
   you have to call `Pervasives.$caret` from your Javascript functions
   (TODO: document the conversion rules)

## Call Javascript from OCaml

For plain Javascript functions, you need write Javascript types.

```
 OCaml values  |  Actual Javascript type
-----------------------------------------
 int           |  Number
-----------------------------------------
 bool          |  Number (0|1)
-----------------------------------------
 string        |  String
-----------------------------------------
 bytes         |  Number Array
-----------------------------------------
 array         |  Array
-----------------------------------------
 tuple         |  Array (index from 1)
-----------------------------------------
 record        |  Array (index from 1)
```


For example, if we want to provide bindings to the
[mochajs](https://mochajs.org/) unittest framework, below is an example

```ocaml
external describe : string -> (unit -> unit) -> unit = "" [@@js.call "describe"]
external it : string -> (unit -> unit) -> unit = "" [@@js.call "it"]
```
Since `mochajs` is a test framework, we also need some assertion
test, we can also describe the bindings to `assert.deepEqual` from
nodejs `assert` library:

```ocaml
external eq : 'a -> 'a -> unit = "" 
    [@@js.call "deepEqual"]
    [@@js.module "assert"]
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
                          return it(param[1], param[2]);
                        }, suite);
            });
}

```

